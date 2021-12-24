use druid::{
    kurbo::{Point, TranslateScale, Vec2},
    piet::{Image, InterpolationMode, Piet, PietImage},
    scroll_component::ScrollComponent,
    widget::prelude::*,
    Command, Data, ImageBuf, MouseButton, MouseEvent, RenderContext, Selector,
};
use std::{rc::Rc, sync::Arc};

/// The amount to scale scrolls by
const SCROLL_TWEAK: f64 = 0.5;
const MIN_SCALE: f64 = 0.2; // 20%
const MAX_SCALE: f64 = 15.0; // 1_500%

/// Set the zoom to a particular scale.
pub const SET_SCALE: Selector<f64> = Selector::new("image-viewer.set-scale");
/// Change the zoom by a factor (<1. is shrink, >1 is grow)
pub const ZOOM: Selector<f64> = Selector::new("image-viewer.zoom");
/// This widget will report changes to scale or offset.
pub const NOTIFY_TRANSFORM: Selector<TranslateScale> =
    Selector::new("image-viewer.notify-transform");

pub struct ZoomImage {
    /// The transformation to apply to the image for drawing. Maps image coords
    /// to widget coords.
    trans: TranslateScale,
    /// Whether we are in normal mode, or if there is a drag or animation in progress.
    mode: Mode,

    /// We need a cache for the piet image buffer, because we cannot create it
    /// until `paint` is called.
    piet_image: Option<Rc<PietImage>>,
}

impl Widget<Arc<ImageBuf>> for ZoomImage {
    fn event(&mut self, ctx: &mut EventCtx, event: &Event, data: &mut Arc<ImageBuf>, _env: &Env) {
        match event {
            Event::Command(cmd) => {
                if let Some(scale) = cmd.get(SET_SCALE) {
                    // TODO figure out how to use widget ids.
                    //if matches!(cmd.target(), Target::Widget(wid) if wid == ctx.widget_id()) {
                    // Zoom around the middle of the widget
                    let zoom_point = (ctx.size() * 0.5).to_vec2().to_point();
                    self.zoom_to(data, ctx.size(), *scale, zoom_point);
                    ctx.request_paint();
                    if self.is_animating() {
                        ctx.request_anim_frame();
                    }
                    ctx.submit_command(self.notify_transform());
                    //}
                }
                if let Some(scale_factor) = cmd.get(ZOOM) {
                    // Zoom around the middle of the widget
                    let zoom_point = (ctx.size() * 0.5).to_vec2().to_point();
                    self.zoom(data, ctx.size(), *scale_factor, zoom_point);
                    ctx.request_paint();
                    if self.is_animating() {
                        ctx.request_anim_frame();
                    }
                    ctx.submit_command(self.notify_transform());
                    //}
                }
            }
            Event::Wheel(MouseEvent {
                pos, wheel_delta, ..
            }) => {
                let scale = (SCROLL_TWEAK * -wheel_delta.y.signum()).exp();
                self.zoom(data, ctx.size(), scale, *pos);
                ctx.request_paint();
                if self.is_animating() {
                    ctx.request_anim_frame();
                }
                ctx.submit_command(self.notify_transform());
            }
            Event::MouseDown(MouseEvent {
                buttons,
                window_pos,
                ..
            }) if buttons.contains(MouseButton::Left) => {
                if !self.is_dragging() {
                    self.drag_start(*window_pos);
                    ctx.set_active(true);
                }
            }
            Event::MouseUp(MouseEvent { buttons, .. }) if !buttons.contains(MouseButton::Left) => {
                if self.drag_stop(data, ctx.size()) {
                    ctx.request_anim_frame();
                }
                ctx.request_paint();
                ctx.set_active(false);
                ctx.submit_command(self.notify_transform());
            }
            Event::MouseMove(MouseEvent { window_pos, .. }) => {
                self.drag_move(*window_pos, ctx);
            }
            Event::AnimFrame(time) => {
                // scale to ms.
                let time = *time as f64 * 0.000_001;
                if let Mode::Anim(anim) = &mut self.mode {
                    anim.update(time);
                    if anim.is_complete() {
                        self.mode = Mode::Normal;
                    } else {
                        ctx.request_anim_frame();
                    }
                }
                ctx.request_paint();
            }
            _ => (),
        }
    }

    fn lifecycle(
        &mut self,
        ctx: &mut LifeCycleCtx,
        event: &LifeCycle,
        data: &Arc<ImageBuf>,
        _env: &Env,
    ) {
        match event {
            LifeCycle::WidgetAdded => (),
            LifeCycle::Size(size) => {
                self.constrain_transform(data, *size);
                ctx.submit_command(self.notify_transform());
                // Cancel drag and complete animation.
                self.mode = Mode::Normal;
            }
            _ => (),
        }
    }

    fn update(
        &mut self,
        ctx: &mut UpdateCtx,
        old_data: &Arc<ImageBuf>,
        data: &Arc<ImageBuf>,
        _env: &Env,
    ) {
        // TODO it would be nice if we could make the image here.
        if !old_data.same(data) {
            // invalidate image
            self.piet_image = None;
            self.constrain_transform(data, ctx.size());
            ctx.submit_command(self.notify_transform());
            ctx.request_paint();
        }
    }

    fn layout(
        &mut self,
        _ctx: &mut LayoutCtx,
        bc: &BoxConstraints,
        _data: &Arc<ImageBuf>,
        _env: &Env,
    ) -> Size {
        // We take all the space we can.
        // TODO take less space if we wouldn't fill it all due to zoom.
        // We'd still need to draw to the middle in case the constraints were tight from below.
        bc.max()
    }

    fn paint(&mut self, ctx: &mut PaintCtx, data: &Arc<ImageBuf>, _env: &Env) {
        let widget_area = ctx.size().to_rect();
        ctx.clip(widget_area);

        let trans = self.draw_transform();
        let image = self.image(data, ctx);

        ctx.draw_image(
            &image,
            trans * image.size().to_rect(),
            InterpolationMode::Bilinear,
        );
    }
}

impl ZoomImage {
    pub fn new() -> Self {
        Self {
            trans: Default::default(),
            mode: Mode::Normal,
            piet_image: None,
        }
    }

    fn image(&mut self, data: &Arc<ImageBuf>, rc: &mut Piet) -> Rc<PietImage> {
        if let Some(img) = self.piet_image.as_ref() {
            return img.clone();
        } else {
            self.piet_image.insert(Rc::new(data.to_image(rc))).clone()
        }
    }

    /// Request to change the zoom level by the given factor.
    ///
    /// What the scale and offset actually change to will depend on constraints.
    ///
    /// A scale factor `> 1` means enlarge, `< 1` means shrink. A scale factor
    /// of `1` does nothing.
    ///
    /// `origin` is the point in widget space where the zoom is centred.
    ///
    /// # Panics
    ///
    /// This function will panic unless `0 < scale_factor < infinity` and
    /// `origin` is finite.
    fn zoom(&mut self, data: &Arc<ImageBuf>, widget_size: Size, scale_factor: f64, origin: Point) {
        /*
        println!(
            "scale_factor: {:.2} image_size: {:.2} view_size {:.2} origin {:.2}",
            scale_factor,
            self.image.size(),
            self.size,
            origin,
        );
        */
        self.zoom_to(
            data,
            widget_size,
            self.trans.as_tuple().1 * scale_factor,
            origin,
        );
    }

    /// Request a change to the zoom level.
    ///
    /// What the scale and offset actually change to will depend on constraints.
    ///
    /// A scale factor `> 1` means enlarge, `< 1` means shrink. A scale factor
    /// of `1` does nothing.
    ///
    /// `origin` is the point in widget space where the zoom is centred.
    ///
    /// # Panics
    ///
    /// This function will panic unless `0 < scale_factor < infinity` and
    /// `center` is finite.
    fn zoom_to(&mut self, data: &Arc<ImageBuf>, widget_size: Size, scale: f64, origin: Point) {
        assert!(
            0. < scale && scale.is_finite(),
            "zoom scale must be in (0, infinity), got {}",
            scale
        );
        assert!(
            origin.x.is_finite() && origin.y.is_finite(),
            "scale centre must be finite, found {:?}",
            origin
        );
        /*
        println!(
            "scale_factor: {:.2} image_size: {:.2} view_size {:.2} origin {:.2}",
            scale_factor,
            self.image.size(),
            self.size,
            origin,
        );
        */

        let old_trans = self.trans;

        // Get the point in image space that the zoom in centred on
        let origin_img = self.trans.inverse() * origin;

        // Scale
        // Constrain the scale
        let scale = constrain_scale(data.size(), widget_size, scale);

        // Offset
        let origin_scaled = origin_img.to_vec2() * scale;

        // Calculate the new offset (the new mouse position on the image - the mouse position on
        // screen. TODO this works but I don't know why. Actually do the math.
        let diff = origin.to_vec2() - origin_scaled;

        self.trans = TranslateScale::new(diff, scale);
        self.constrain_transform(data, widget_size);
        if !trans_approx_eq(self.trans, old_trans) {
            match &mut self.mode {
                Mode::Normal => {
                    self.mode = Mode::Anim(AnimState::new(old_trans, self.trans));
                }
                Mode::Anim(anim) => {
                    self.mode = Mode::Anim(AnimState::new(anim.current, self.trans))
                }
                // If we're dragging then don't animate
                Mode::Drag(_) => (),
            }
        }
    }

    /// Transform the image at 100% scale positioned at (0,0) to the correct image
    /// position, taking into account any drag operation or animation in progress.
    fn draw_transform(&self) -> TranslateScale {
        match self.mode {
            Mode::Normal => self.trans,
            Mode::Drag(Drag { diff, .. }) => {
                let (trans, scale) = self.trans.as_tuple();
                TranslateScale::new(trans + diff, scale)
            }
            Mode::Anim(AnimState { current, .. }) => current,
        }
    }

    /// Switch to drag state.
    fn drag_start(&mut self, window_pos: Point) {
        // TODO if mid-animation, capture that state so that point under mouse stays
        // the same (if possible after constraints)
        self.mode = Mode::Drag(Drag {
            start: window_pos,
            diff: Vec2::ZERO,
        });
    }

    /// Complete drag state (go back to normal).
    ///
    /// Returns true if we need to request animation frame.
    fn drag_stop(&mut self, data: &Arc<ImageBuf>, widget_size: Size) -> bool {
        if let Mode::Drag(drag) = &self.mode {
            let (trans, scale) = self.trans.as_tuple();
            let current_trans = TranslateScale::new(trans + drag.diff, scale);
            self.trans = current_trans;
            self.constrain_transform(data, widget_size);
            if trans_approx_eq(self.trans, current_trans) {
                self.mode = Mode::Normal;
                false
            } else {
                self.mode = Mode::Anim(AnimState::new(current_trans, self.trans));
                true
            }
        } else {
            false
        }
    }

    /// Update the drag state.
    ///
    /// Does nothing if we aren't in the drag state.
    fn drag_move(&mut self, window_pos: Point, ctx: &mut EventCtx) {
        if let Mode::Drag(drag) = &mut self.mode {
            drag.diff = window_pos - drag.start;
            ctx.request_paint();
        }
    }

    /// Helper function to call `constrain_transform` for this image.
    fn constrain_transform(&mut self, data: &Arc<ImageBuf>, widget_size: Size) {
        self.trans = constrain_transform(data.size(), widget_size, self.trans);
    }

    fn is_dragging(&self) -> bool {
        matches!(self.mode, Mode::Drag(_))
    }

    fn is_animating(&self) -> bool {
        matches!(self.mode, Mode::Anim(_))
    }

    fn notify_transform(&self) -> Command {
        NOTIFY_TRANSFORM.with(self.trans.inverse())
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
    current: TranslateScale,
    /// The target transform,
    to: TranslateScale,
    /// How much to increment each parameter per millisecond
    inc: TranslateScale,
}

impl AnimState {
    fn new(from: TranslateScale, to: TranslateScale) -> Self {
        // Target animation length is 500ms.
        const TARGET_ANIM_LEN: f64 = 100.;

        // The animation is already complete. So we just put anything in the increment fields because
        // on the first iteration the animation will end.
        if trans_approx_eq(from, to) {
            return AnimState {
                current: to,
                to,
                inc: TranslateScale::default(),
            };
        }
        let (
            Vec2 {
                x: x_from,
                y: y_from,
            },
            s_from,
        ) = from.as_tuple();
        let (Vec2 { x: x_to, y: y_to }, s_to) = to.as_tuple();
        // Calculate the change per millisecond.
        let x_inc = (x_to - x_from) / TARGET_ANIM_LEN;
        let y_inc = (y_to - y_from) / TARGET_ANIM_LEN;
        let s_inc = (s_to - s_from) / TARGET_ANIM_LEN;
        Self {
            current: from,
            to,
            inc: TranslateScale::new(Vec2::new(x_inc, y_inc), s_inc),
        }
    }

    /// Update the animation, given the time in ms.
    fn update(&mut self, time: f64) {
        let (Vec2 { x: x_cur, y: y_cur }, s_cur) = self.current.as_tuple();
        let (Vec2 { x: x_to, y: y_to }, s_to) = self.to.as_tuple();
        let (Vec2 { x: x_inc, y: y_inc }, s_inc) = self.inc.as_tuple();

        let mut x_next = x_cur + x_inc * time;
        let mut y_next = y_cur + y_inc * time;
        let mut s_next = s_cur + s_inc * time;

        if (x_next - x_to) * (x_cur - x_to) < 0. {
            x_next = x_to;
        }
        if (y_next - y_to) * (y_cur - y_to) < 0. {
            y_next = y_to;
        }
        if (s_next - s_to) * (s_cur - s_to) < 0. {
            s_next = s_to;
        }
        self.current = TranslateScale::new(Vec2::new(x_next, y_next), s_next);
    }

    /// Is the animation complete
    fn is_complete(&self) -> bool {
        trans_approx_eq(self.current, self.to)
    }
}

/// Takes any transform and returns the "closest" transform that is inside our constraints.
fn constrain_transform(img_size: Size, widget_size: Size, trans: TranslateScale) -> TranslateScale {
    let (offset, scale) = trans.as_tuple();

    // Firstly, constrain the scaling.
    let scale = constrain_scale(img_size, widget_size, scale);

    // Then, given the chosen scale, constrain the offset.
    let offset = constrain_offset(img_size, widget_size, scale, offset);

    TranslateScale::new(offset, scale)
}

fn constrain_scale(img_size: Size, widget_size: Size, scale: f64) -> f64 {
    //  - At the lower end, the scale should be bigger than the smaller of
    //    - a compile-time minimum scale (e.g. 20%)
    //    - the biggest size that can fit the whole image in.
    //  - At the higher end, the scale should be smaller than some maximum scale.
    // If both constrains are not satisfyable, then choose the size from the minimum test.
    let min_x_scale = widget_size.width / img_size.width;
    let min_y_scale = widget_size.height / img_size.height;
    let min_scale = min_x_scale.min(min_y_scale).min(MIN_SCALE);
    scale.min(MAX_SCALE).max(min_scale)
}

fn constrain_offset(img_size: Size, widget_size: Size, scale: f64, offset: Vec2) -> Vec2 {
    // For each direction:
    //  - At the lower end, the bottom/right side must be >= the widget edge
    //  - At the upper end, the top/left size must be <= the widget edge (always 0.)
    //  - If we can't satisfy both of these, then center the image in the widget
    //    (it will be too small)
    let Vec2 { x: tx, y: ty } = offset;
    let diff_x = widget_size.width - img_size.width * scale;
    let tx = if diff_x > 0. {
        diff_x * 0.5
    } else {
        tx.min(0.).max(diff_x)
    };
    let diff_y = widget_size.height - img_size.height * scale;
    let ty = if diff_y > 0. {
        diff_y * 0.5
    } else {
        ty.min(0.).max(diff_y)
    };
    Vec2::new(tx, ty)
}

/// Compare two transforms to see if they are approximately equal.
fn trans_approx_eq(t1: TranslateScale, t2: TranslateScale) -> bool {
    const EPSILON: f64 = 1e-6;
    let (t1, s1) = t1.as_tuple();
    let (t2, s2) = t2.as_tuple();
    (t2 - t1).hypot2() < EPSILON.powi(2) && (s1 - s2).abs() < EPSILON
}
