use druid::{
    kurbo::{Point, Rect, Vec2},
    piet::{Image, InterpolationMode, PietImage},
    scroll_component::ScrollComponent,
    widget::prelude::*,
    Command, Data, ImageBuf, MouseButton, MouseEvent, RenderContext, Selector, Target,
};
use std::sync::Arc;

/// The amount to scale scrolls by
const SCROLL_TWEAK: f64 = 0.5;
const MAX_SCALE: f64 = 15.0; // 1_500%

pub const UPDATE_SCALE: Selector<f64> = Selector::new("image-viewer.update-scale");

pub struct ZoomImage {
    /// For drawing scrollbars.
    scroll_component: ScrollComponent,

    inner: Option<ZoomImageInner>,
}

impl ZoomImage {
    pub fn new() -> Self {
        ZoomImage {
            scale: f64::INFINITY,
            offset: (0., 0.).into(),
            scroll_component: ScrollComponent::new(),
            image: None,
            drag_start: Point::ZERO,
            drag_offset: None,
            draw_scale: 1.,
            draw_offset: (0., 0.).into(),
        }
    }

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

    /// Get the minimum scale that will fit the whole image in.
    fn min_scale(&self, size: Size) -> f64 {
        let img_size = self.image_size();
        let min_x_scale = size.width / img_size.width;
        let min_y_scale = size.height / img_size.height;
        min_x_scale.min(min_y_scale)
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
                let scale = (SCROLL_TWEAK * -wheel_delta.y.signum()).exp();
                self.zoom(scale, *pos, ctx.size());
                self.offset = self.constrain_offset(self.offset, ctx.size());
                self.draw_offset = self.offset;
                ctx.submit_command(scale_cmd(self.scale, ctx.widget_id()));
                ctx.request_paint();
            }
            Event::Command(cmd) => {
                if let Some(scale) = cmd.get(UPDATE_SCALE) {
                    if matches!(cmd.target(), Target::Widget(wid) if wid == ctx.widget_id()) {
                        self.zoom(*scale, (0., 0.).into(), ctx.size());
                        self.offset = self.constrain_offset(self.offset, ctx.size());
                        self.draw_offset = self.offset;
                        ctx.request_paint();
                    }
                }
            }
            Event::MouseDown(MouseEvent {
                buttons,
                window_pos,
                ..
            }) if buttons.contains(MouseButton::Left) => {
                if !ctx.is_active() {
                    self.drag_start = *window_pos;
                    self.drag_offset = Some(Vec2::ZERO);
                    ctx.set_active(true);
                }
            }
            Event::MouseUp(MouseEvent { buttons, .. }) if !buttons.contains(MouseButton::Left) => {
                if let Some(drag_offset) = self.drag_offset {
                    self.offset = self.constrain_offset(self.offset + drag_offset, ctx.size());
                    self.draw_offset = self.offset;
                }
                self.drag_offset = None;
                ctx.set_active(false);
            }
            Event::MouseMove(MouseEvent { window_pos, .. }) => {
                if ctx.is_active() {
                    self.drag_offset = Some(*window_pos - self.drag_start);
                    ctx.request_paint();
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
                if data.is_some() {
                    self.image = None;
                    ctx.submit_command(scale_cmd(self.scale, ctx.widget_id()));
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
                self.invalidate_image();
                ctx.submit_command(scale_cmd(self.scale, ctx.widget_id()));
                ctx.request_paint();
            }
            (None, Some(next)) => {
                self.invalidate_image();
                ctx.submit_command(scale_cmd(self.scale, ctx.widget_id()));
                ctx.request_paint();
            }
            (Some(_), None) => {
                self.invalidate_image();
                ctx.submit_command(scale_cmd(self.scale, ctx.widget_id()));
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
        bc.max()
    }

    fn paint(&mut self, ctx: &mut PaintCtx, data: &Option<Arc<ImageBuf>>, env: &Env) {
        log::debug!("size: {:?} scale: {:?}", ctx.size(), self.scale,);
        let size = ctx.size();
        ctx.clip(size.to_rect());
        assert!(
            self.scale >= 0.,
            "scale should be >= 0, found {}",
            self.scale
        );
        if self.scale == 0. {
            // infinitely small, nothing to draw
            return;
        }

        if let Some(data) = data.as_ref() {
            // rebuild cache if necessary
            if self.image.is_none() {
                self.image = Some(data.to_image(&mut **ctx));
                self.constrain_scale_offset(size);
            }
            let draw_size = self.draw_size();
            if self.draw_size().is_empty() {
                return;
            }
            let draw_offset = self.draw_offset(size);
            ctx.draw_image(
                self.image.as_ref().unwrap(),
                Rect::from_origin_size(draw_offset, draw_size),
                InterpolationMode::Bilinear,
            );
        }
    }
}

/// We only hold this state when an actual image is present.
struct ZoomImageInner {
    /// The underlying image we will be painting
    image: PietImage,

    /// The size of the area we will be drawing to
    size: Size,
    /// How much to scale the image
    scale: f64,
    /// The offset of the top-left of the viewport with respect to the image.
    ///
    /// If a drag operation is in progress, or
    offset: Vec2,

    mode: Mode,
}

impl ZoomImage {
    pub fn new(image: PietImage, size: Size) -> Self {
        image,
        size,
        scale: 1.,
        offset: (0., 0.).into(),
        mode: Mode::Normal,
    }

    /// The actual scale we will draw using.
    fn draw_scale(&self) -> f64 {
        match &self.mode {
            Mode::Tween { scale } => scale,
            _ => self.scale
        }
    }

    /// The actual size we will draw.
    fn draw_size(&self) -> Size {
        self.size * self.draw_scale()
    }


    /// How much to offset the image for drawing
    fn draw_offset(&self) -> Vec2 {
        match &self.mode {
            Mode::Normal => self.offset,
            Mode::Drag(Drag { offset, .. }) => offset,
            Mode::Anim(Tween { offset, .. }) =>
        }
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
}

enum Mode {
    Normal,
    Drag(Drag),
    Anim(Tween),
}

struct Drag {
    /// The mouse position at the start of the scroll
    start: Point,
    /// A temporary offset
    offset: Vec2,
}

/// For animation
struct Tween {
    /// The current scale,
    scale: f64,
    /// The current offset.
    offset: Vec2,
}


fn scale_cmd(scale: f64, id: WidgetId) -> Command {
    Command::new(UPDATE_SCALE, scale, Target::Widget(id))
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
