use druid::{
    kurbo::{Point, Rect, Vec2},
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

    /*
    /// Zoom im/out by given scale, centred at given point.
    fn zoom(&mut self, scale_factor: f64, mouse_pos: Point, size: Size) {
        if scale_factor.is_finite() && scale_factor > 0. {
            return;
        }
        let old_scale = self.scale;

        // Get the mouse position relative to the image.
        // If the image exists, use its offset, else use our target offset..
        let draw_offset = self
            .inner
            .as_ref()
            .map(|img| img.draw_offset)
            .unwrap_or(self.offset);
        let image_pos = mouse_pos - draw_offset;

        self.scale *= scale_factor;
        // Constrain the scale
        self.scale = self.scale.max(self.min_scale(size)).min(MAX_SCALE);

        // Now we have the new scale we can apply it to our calculated image position to get the
        // same position in the new image.
        let new_image_pos = (self.scale / old_scale) * image_pos;

        // Calculate the new offset (the new mouse position on the image - the mouse position on
        // screen
        self.offset = self.constrain_offset(-(new_image_pos - mouse_pos.to_vec2()), size);
        self.draw_scale = self.scale;
        self.draw_offset = self.offset;
    }

    /// Translate the image.
    ///
    /// We need to know the size of the viewport, and we can't get this without it being passed in.
    /// If scale is currently being animated, then this will be animated too, so that the cursor
    /// position doesn't change (except for constraints), otherwise it will be instant.
    fn translate(&mut self, amt: Vec2, size: Size) {
        // we are doing what `BoxConstraints` does, but we can't use it because we don't want its
        // rounding behavior.
        self.offset = self.offset + amt;
        self.offset = self.constrain_offset(self.offset, size);
        self.draw_offset = self.offset;
    }

    /// Step forward the animation.
    fn step_animation(&mut self) -> Finished {
        let (next_scale, t) = scale_towards(self.draw_scale, self.scale);
        self.draw_scale = next_scale;
        self.draw_offset = self.draw_offset.lerp(self.offset, t);
        if self.draw_scale == self.scale {
            Finished::Yes
        } else {
            Finished::No
        }
    }

    /// Take an offset, and clamp it to the allowed values.
    fn constrain_offset(&self, offset: Vec2, size: Size) -> Vec2 {
        let img_size = self.image_size();
        let max_offset_x = (img_size.width * self.scale - size.width).max(0.);
        let max_offset_y = (img_size.height * self.scale - size.height).max(0.);
        let max_offset = Size::new(max_offset_x, max_offset_y);
        offset
            .to_size()
            .clamp(-1. * max_offset, Size::ZERO)
            .to_vec2()
    }

    /// Invalidate the image cache.
    fn invalidate_image(&mut self) {
        self.image = None;
    }

    /// The final offset we will draw the image to. Also useful for hit testing.
    fn draw_offset(&self, size: Size) -> Point {
        let offset = match self.drag_offset {
            Some(drag_offset) => self.draw_offset + drag_offset,
            None => self.draw_offset,
        };
        // TODO shouldn't need this, turn it into a [debug_]assert?
        let offset = self.constrain_offset(offset, size);
        let draw_size = self.draw_size();
        Point::new(
            if draw_size.width < size.width {
                (size.width - draw_size.width) * 0.5
            } else {
                offset.x
            },
            if draw_size.height < size.height {
                (size.height - draw_size.height) * 0.5
            } else {
                offset.y
            },
        )
    }

    fn image_size(&self) -> Size {
        self.image.as_ref().map(Image::size).unwrap()
    }

    fn constrain_scale_offset(&mut self, size: Size) {
        let new_scale = self.scale.min(self.min_scale(size));
        if new_scale != self.scale {
            self.scale = new_scale;
            self.draw_scale = new_scale;
        }
        let new_offset = self.constrain_offset(self.offset, size);
        if new_offset != self.offset {
            self.offset = new_offset;
            self.draw_offset = new_offset;
        }
    }
    */
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
        assert_eq!(inner.size, ctx.size());
        ctx.clip(inner.size.to_rect());
        assert!(
            inner.scale > 0.,
            "scale should be > 0, found {}",
            inner.scale
        );

        let draw_size = inner.draw_size();
        if draw_size.is_empty() {
            // infinitely small, nothing to draw
            return;
        }
        let draw_offset = inner.draw_offset();
        let image = inner.image(ctx);
        /*
        if draw_size.width < inner.size.width {
            ctx.transform(Affine::translate((
                (inner.size.width - draw_size.width) * 0.5,
                0.,
            )))
        }
        if draw_size.height < inner.size.height {
            ctx.transform(Affine::translate((
                0.,
                (inner.size.height - draw_size.height) * 0.5,
            )))
        }
        */
        ctx.draw_image(&image, inner.image_location(), InterpolationMode::Bilinear);
    }
}

/// We only hold this state when an actual image is present.
struct ZoomImageInner {
    /// The underlying image we will be painting
    pub image: Arc<ImageBuf>,

    /// The size of the area we will be drawing to
    pub size: Size,
    /// How much to scale the image
    pub scale: f64,
    /// The offset of the top-left of the viewport with respect to the image.
    ///
    /// If a drag operation is in progress, or if we are animating, then this might
    /// not be the actually displayed offset.
    pub offset: Vec2,
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
            scale: 1.,
            offset: (0., 0.).into(),
            mode: Mode::Normal,
            piet_image: None,
        };
        out.scale = out.scale.min(MAX_SCALE).max(out.min_scale());
        out
    }

    fn set_image(&mut self, image: Arc<ImageBuf>) {
        self.image = image;
        self.piet_image = None;
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
        let origin = origin.to_vec2();
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
            "scale: {:.2} image_size: {:.2} view_size {:.2} draw_size {:.2} offset {:.2}",
            self.scale(),
            self.image.size(),
            self.size,
            self.draw_size(),
            self.draw_offset()
        );

        // Get the point in image space that the zoom in centred on
        let origin_img = (origin - self.draw_offset()) / self.scale;
        dbg!(origin_img);

        // Scale
        self.scale *= scale_factor;
        // Constrain the scale
        self.scale = self.scale.max(self.min_scale()).min(MAX_SCALE);

        // Offset
        let origin_scaled = origin_img * self.scale;

        // Calculate the new offset (the new mouse position on the image - the mouse position on
        // screen
        self.offset = constrain_offset(
            self.image.size(),
            self.size,
            self.scale,
            origin - origin_scaled,
        );
    }

    /// Get the maximum scale that will fit the whole image in.
    fn min_scale(&self) -> f64 {
        let img_size = self.image.size();
        let min_x_scale = self.size.width / img_size.width;
        let min_y_scale = self.size.height / img_size.height;
        min_x_scale.min(min_y_scale)
    }

    /// The actual scale we will draw using.
    fn scale(&self) -> f64 {
        match &self.mode {
            Mode::Anim(AnimState { scale, .. }) => *scale,
            _ => self.scale,
        }
    }

    /// The actual size we will draw.
    fn draw_size(&self) -> Size {
        self.image.size() * self.scale()
    }

    /// How much to offset the image for drawing
    fn draw_offset(&self) -> Vec2 {
        match &self.mode {
            Mode::Normal => self.offset,
            Mode::Drag(Drag { offset, .. }) => {
                constrain_offset(
                    self.image.size(),
                    self.size,
                    self.scale,
                    *offset + self.offset,
                )
                //*offset
            }
            Mode::Anim(AnimState { offset, .. }) => *offset,
        }
    }

    /// Return the point in widget space, taking into account any centering of
    /// the image.
    fn center_transform(&self) -> Affine {
        let draw_size = self.draw_size();
        let x = if draw_size.width < self.size.width {
            (self.size.width - draw_size.width) * 0.5
        } else {
            0.
        };
        let y = if draw_size.height < self.size.height {
            (self.size.height - draw_size.height) * 0.5
        } else {
            0.
        };
        Affine::translate((x, y))
    }

    /// Where the image should be drawn.
    fn image_location(&self) -> Rect {
        Rect::from_origin_size(self.draw_offset().to_point(), self.draw_size())
    }

    fn drag_start(&mut self, window_pos: Point) {
        self.mode = Mode::Drag(Drag {
            start: window_pos,
            offset: Vec2::ZERO,
        });
    }

    fn drag_stop(&mut self) {
        if let Mode::Drag(drag) = &self.mode {
            self.offset = constrain_offset(
                self.image.size(),
                self.size,
                self.scale,
                self.offset + drag.offset,
            );
        }
        if matches!(self.mode, Mode::Drag(_)) {
            self.mode = Mode::Normal;
        }
    }

    fn drag_move(&mut self, window_pos: Point, ctx: &mut EventCtx) {
        if let Mode::Drag(drag) = &mut self.mode {
            drag.offset = window_pos - drag.start;
            ctx.request_paint();
        }
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
    offset: Vec2,
}

/// For animation
struct AnimState {
    /// The current scale,
    scale: f64,
    /// The current offset.
    offset: Vec2,
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
