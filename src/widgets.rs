use druid::{
    kurbo::{Point, Rect, TranslateScale, Vec2},
    piet::{Image, InterpolationMode, Piet, PietImage},
    scroll_component::ScrollComponent,
    widget::prelude::*,
    Affine, Command, Data, ImageBuf, MouseButton, MouseEvent, RenderContext, Selector, Target,
};
use std::{
    cell::{Ref, RefCell},
    rc::Rc,
    sync::Arc,
};

/// The amount to scale scrolls by
const SCROLL_TWEAK: f64 = 0.5;
const MAX_SCALE: f64 = 15.0; // 1_500%

pub const SET_SCALE: Selector<f64> = Selector::new("image-viewer.set-scale");

pub struct ZoomImage {
    /// For drawing scrollbars.
    scroll_component: ScrollComponent,

    inner: Option<ZoomImageInner>,
}

impl ZoomImage {
    pub fn new() -> Self {
        ZoomImage {
            scroll_component: ScrollComponent::new(),
            inner: None,
        }
    }
}

impl Widget<Option<Arc<ImageBuf>>> for ZoomImage {
    fn event(
        &mut self,
        ctx: &mut EventCtx,
        event: &Event,
        data: &mut Option<Arc<ImageBuf>>,
        env: &Env,
    ) {
        match event {
            // f64::clamp has been stabilized. Use here once it's ridden the trains.
            Event::Wheel(MouseEvent {
                pos, wheel_delta, ..
            }) => {
                if let Some(inner) = &mut self.inner {
                    let scale = (SCROLL_TWEAK * -wheel_delta.y.signum()).exp();
                    inner.zoom(scale, *pos);
                    ctx.request_paint();
                }
            }
            Event::Command(cmd) => {
                if let Some(scale) = cmd.get(SET_SCALE) {
                    if matches!(cmd.target(), Target::Widget(wid) if wid == ctx.widget_id()) {
                        if let Some(inner) = &mut self.inner {
                            inner.zoom(*scale, (0., 0.).into());
                            ctx.request_paint();
                        }
                    }
                }
            }
            Event::MouseDown(MouseEvent {
                buttons,
                window_pos,
                ..
            }) if buttons.contains(MouseButton::Left) => {
                if let Some(inner) = &mut self.inner {
                    if !matches!(inner.mode, Mode::Drag(_)) {
                        inner.drag_start(*window_pos);
                        ctx.set_active(true);
                    }
                }
            }
            Event::MouseUp(MouseEvent { buttons, .. }) if !buttons.contains(MouseButton::Left) => {
                if let Some(inner) = &mut self.inner {
                    inner.drag_stop();
                    ctx.request_paint();
                }
                ctx.set_active(false);
            }
            Event::MouseMove(MouseEvent { window_pos, .. }) => {
                if let Some(inner) = &mut self.inner {
                    inner.drag_move(*window_pos, ctx);
                }
            }
            _ => (),
        }
    }

    fn lifecycle(
        &mut self,
        ctx: &mut LifeCycleCtx,
        event: &LifeCycle,
        data: &Option<Arc<ImageBuf>>,
        env: &Env,
    ) {
        match event {
            LifeCycle::WidgetAdded => {}
            LifeCycle::Size(size) => {
                if let Some(inner) = &mut self.inner {
                    inner.size = *size;
                    inner.zoom(1., Point::ZERO);
                    // If the widget size changed, then cancel drag and complete animation.
                    inner.mode = Mode::Normal;
                }
            }
            _ => (),
        }
    }

    fn update(
        &mut self,
        ctx: &mut UpdateCtx,
        old_data: &Option<Arc<ImageBuf>>,
        data: &Option<Arc<ImageBuf>>,
        env: &Env,
    ) {
        // TODO it would be nice if we could make the image here.
        match (old_data.as_ref(), data.as_ref()) {
            (Some(prev), Some(next)) if !prev.same(next) => {
                // Image must be `Some` because previous data is `Some`.
                self.inner.as_mut().unwrap().set_image((*next).clone());
                //ctx.submit_command(scale_cmd(self.scale, ctx.widget_id()));
                ctx.request_paint();
            }
            (None, Some(next)) => {
                self.inner = Some(ZoomImageInner::new((*next).clone(), ctx.size()));
                ctx.request_paint();
            }
            (Some(_), None) => {
                self.inner = None;
                ctx.request_paint();
            }
            (Some(_), Some(_)) | (None, None) => (), // no change
        }
    }

    fn layout(
        &mut self,
        ctx: &mut LayoutCtx,
        bc: &BoxConstraints,
        data: &Option<Arc<ImageBuf>>,
        env: &Env,
    ) -> Size {
        // We take all the space we can.
        // TODO take less space if we wouldn't fill it all due to zoom.
        // We'd still need to draw to the middle in case the constraints were tight from below.
        bc.max()
    }

    fn paint(&mut self, ctx: &mut PaintCtx, data: &Option<Arc<ImageBuf>>, env: &Env) {
        let inner = match &mut self.inner {
            Some(v) => v,
            // nothing to draw if we don't have an image.
            None => return,
        };
        debug_assert_eq!(inner.size, ctx.size());
        ctx.clip(inner.size.to_rect());

        let trans = inner.draw_transform();
        let image = inner.image(ctx);

        ctx.draw_image(
            &image,
            trans * image.size().to_rect(),
            InterpolationMode::Bilinear,
        );
    }
}

/// We only hold this state when an actual image is present.
struct ZoomImageInner {
    /// The underlying image we will be painting
    pub image: Arc<ImageBuf>,

    /// The size of the area we will be drawing to
    pub size: Size,
    /// The transformation to apply to the image for drawing. Maps image coords
    /// to widget coords.
    trans: TranslateScale,
    /// Whether we are in normal mode, or if there is a drag or animation in progress.
    pub mode: Mode,

    /// We need a cache for the piet image buffer, because we cannot create it
    /// until `paint` is called.
    piet_image: Option<Rc<PietImage>>,
}

impl ZoomImageInner {
    pub fn new(image: Arc<ImageBuf>, size: Size) -> Self {
        let mut out = Self {
            image,
            size,
            trans: Default::default(),
            mode: Mode::Normal,
            piet_image: None,
        };
        out.constrain_transform();
        out
    }

    fn set_image(&mut self, image: Arc<ImageBuf>) {
        self.image = image;
        self.piet_image = None;
        self.constrain_transform();
    }

    fn image(&mut self, rc: &mut Piet) -> Rc<PietImage> {
        if let Some(img) = self.piet_image.as_ref() {
            return img.clone();
        } else {
            self.piet_image
                .insert(Rc::new(self.image.to_image(rc)))
                .clone()
        }
    }

    /// Request a change to the zoom level.
    ///
    /// What the scale and offset actually change to will depend on constraints.
    ///
    /// A scale factor `> 1` means enlarge, `< 1` means shrink. A scale factor
    /// of `1` can be used to reapply the constraints.
    ///
    /// `origin` is the point in widget space where the zoom is centred.
    ///
    /// # Panics
    ///
    /// This function will panic unless `0 < scale_factor < infinity` and
    /// `center` is finite.
    fn zoom(&mut self, scale_factor: f64, origin: Point) {
        assert!(
            0. < scale_factor && scale_factor.is_finite(),
            "zoom scale factor must be in (0, infinity), got {}",
            scale_factor
        );
        assert!(
            origin.x.is_finite() && origin.y.is_finite(),
            "scale centre must be finite, found {:?}",
            origin
        );
        println!(
            "scale_factor: {:.2} image_size: {:.2} view_size {:.2} origin {:.2}",
            scale_factor,
            self.image.size(),
            self.size,
            origin,
        );

        // Get the point in image space that the zoom in centred on
        let origin_img = self.trans.inverse() * origin;
        dbg!(origin_img);

        // Scale
        let scale = self.trans.as_tuple().1 * scale_factor;
        // Constrain the scale
        let scale = constrain_scale(self.image.size(), self.size, scale);

        // Offset
        let origin_scaled = origin_img.to_vec2() * scale;

        // Calculate the new offset (the new mouse position on the image - the mouse position on
        // screen
        let diff = origin.to_vec2() - origin_scaled;

        self.trans = TranslateScale::new(diff, scale);
        self.constrain_transform();
    }

    /// Get the maximum scale that will fit the whole image in.
    fn min_scale(&self) -> f64 {
        let img_size = self.image.size();
        let min_x_scale = self.size.width / img_size.width;
        let min_y_scale = self.size.height / img_size.height;
        min_x_scale.min(min_y_scale)
    }

    /// Transform the image at 100% scale positioned at 0,0 to the correct image
    /// position, taking into account any drag operation or animation in progress.
    fn draw_transform(&self) -> TranslateScale {
        match self.mode {
            Mode::Normal => self.trans,
            Mode::Drag(Drag { diff, .. }) => {
                let (trans, scale) = self.trans.as_tuple();
                TranslateScale::new(trans + diff, scale)
            }
            Mode::Anim(AnimState { trans }) => trans,
        }
    }

    fn drag_start(&mut self, window_pos: Point) {
        self.mode = Mode::Drag(Drag {
            start: window_pos,
            diff: Vec2::ZERO,
        });
    }

    fn drag_stop(&mut self) {
        if let Mode::Drag(drag) = &self.mode {
            let (trans, scale) = self.trans.as_tuple();
            self.trans = TranslateScale::new(trans + drag.diff, scale);
            self.constrain_transform();
        }
        if matches!(self.mode, Mode::Drag(_)) {
            self.mode = Mode::Normal;
        }
    }

    fn drag_move(&mut self, window_pos: Point, ctx: &mut EventCtx) {
        if let Mode::Drag(drag) = &mut self.mode {
            drag.diff = window_pos - drag.start;
            ctx.request_paint();
        }
    }

    fn constrain_transform(&mut self) {
        self.trans = constrain_transform(self.image.size(), self.size, self.trans);
    }
}

enum Mode {
    Normal,
    Drag(Drag),
    Anim(AnimState),
}

struct Drag {
    /// The mouse position at the start of the scroll
    start: Point,
    /// A temporary offset
    ///
    /// This offset is allowed to go outside allowed values, so that mouse
    /// behavior feels natural. This means we need to apply constraints
    /// to the offset value before using it for display.
    diff: Vec2,
}

/// For animation
struct AnimState {
    /// The current transform,
    trans: TranslateScale,
}

enum Finished {
    Yes,
    No,
}

impl Finished {
    fn is_finished(&self) -> bool {
        matches!(self, Finished::Yes)
    }
}

/// Get a new scale nearer to `to` than `cur`.
///
/// The second value is the t in lerp, for scaling other things the same proportion.
fn scale_towards(cur: f64, to: f64) -> (f64, f64) {
    assert!(
        cur.is_finite() && to.is_finite(),
        "invalid params passed to `scale_towards`"
    );
    // special case where `cur == to` (avoids singularity below)
    if cur == to {
        return (to, 1.);
    }

    // denominator is the number of frames to get to `to`.
    // TODO make const (if exp becomes const)
    let step = (SCROLL_TWEAK / 5.).exp();
    let next = if cur < to {
        to.min(cur * step)
    } else {
        to.max(cur / step)
    };
    (next, (cur - next) / (cur - to))
}

/// Given an offset, return one that is closest while keeping the image on screen.
fn constrain_offset(img_size: Size, widget_size: Size, scale: f64, offset: Vec2) -> Vec2 {
    assert!(offset.x.is_finite() && offset.y.is_finite());

    Vec2 {
        x: offset
            .x
            .max(widget_size.width - img_size.width * scale)
            .min(0.),
        y: offset
            .y
            .max(widget_size.height - img_size.height * scale)
            .min(0.),
    }
}

/// Given an offset, return one that is closest while keeping the image on screen.
fn constrain_transform(img_size: Size, widget_size: Size, trans: TranslateScale) -> TranslateScale {
    let (Vec2 { x: tx, y: ty }, scale) = trans.as_tuple();

    // Firstly, constrain the scaling.
    let scale = constrain_scale(img_size, widget_size, scale);

    // Then, constrain the translation.
    // For each direction:
    //  - At the lower end, the bottom/right side must be >= the widget edge
    //  - At the upper end, the top/left size must be <= the widget edge (always 0.)
    //  - If we can't satisfy both of these, then center the image in the widget
    //    (it will be too small)
    let offset_x = widget_size.width - img_size.width * scale;
    let tx = if offset_x > 0. {
        offset_x * 0.5
    } else {
        tx.min(0.).max(offset_x)
    };
    let offset_y = widget_size.height - img_size.height * scale;
    let ty = if offset_y > 0. {
        offset_y * 0.5
    } else {
        ty.min(0.).max(offset_y)
    };

    TranslateScale::new(Vec2::new(tx, ty), scale)
}

fn constrain_scale(img_size: Size, widget_size: Size, scale: f64) -> f64 {
    //  - At the lower end, the scale should be bigger than the smaller of
    //    - 100%
    //    - the biggest size that can fit the whole image in.
    //  - At the higher end, the scale should be smaller than some maximum scale.
    // If both constrains are not satisfyable, then choose the size from the minimum test.
    let min_x_scale = widget_size.width / img_size.width;
    let min_y_scale = widget_size.height / img_size.height;
    let min_scale = min_x_scale.min(min_y_scale).min(1.);
    scale.min(MAX_SCALE).max(min_scale)
}
