use druid::{
    kurbo::{Point, Rect, Vec2},
    piet::{Image, InterpolationMode},
    scroll_component::ScrollComponent,
    widget::prelude::*,
    Command, Data, ImageBuf, MouseButton, MouseEvent, RenderContext, Selector, Target,
};
use std::sync::Arc;

/// The amount to scale scrolls by
const SCROLL_TWEAK: f64 = 0.5;
const MAX_SCALE: f64 = 15.0; // 1_500%

const UPDATE_SCALE: Selector<f64> = Selector::new("image-viewer.update-scale");

pub struct ZoomImage {
    /// The underlying image we will be painting
    image: Option<Image>,
    /// A cache of the image size
    ///
    /// Might be able to remove this if `piet::Image` gains the ability to get the size of the
    /// image ([piet#370](https://github.com/linebender/piet/pull/370))
    image_size: Size,
    /// How much to scale the image
    scale: f64,
    /// The offset of the top-left of the viewport with respect to the image.
    offset: Vec2,
    /// For drawing scrollbars.
    scroll_component: ScrollComponent,

    // Stuff for drag scrolling.
    /// The mouse position at the start of the scroll
    drag_start: Point,
    /// A temporary offset
    drag_offset: Option<Vec2>,

    // Stuff for animating zoom.
    draw_scale: f64,
    draw_offset: Vec2,
}

impl ZoomImage {
    pub fn new() -> Self {
        ZoomImage {
            image: None,
            image_size: Size::ZERO,
            scale: 1.,
            offset: (0., 0.).into(),
            scroll_component: ScrollComponent::new(),
            drag_start: Point::ZERO,
            drag_offset: None,
            draw_scale: 1.,
            draw_offset: (0., 0.).into(),
        }
    }

    /// Zoom im/out by given scale, centred at given point.
    fn zoom(&mut self, scale_factor: f64, mouse_pos: Point, size: Size) {
        assert!(scale_factor.is_finite() && scale_factor > 0.);
        let old_scale = self.scale;
        // Get the mouse position relative to the image.
        let image_pos = mouse_pos - self.draw_offset(size);
        self.scale *= scale_factor;
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
        let max_offset_x = (self.image_size.width * self.scale - size.width).max(0.);
        let max_offset_y = (self.image_size.height * self.scale - size.height).max(0.);
        let max_offset = Size::new(max_offset_x, max_offset_y);
        offset
            .to_size()
            .clamp(-1. * max_offset, Size::ZERO)
            .to_vec2()
    }

    /// Get the minimum scale that will fit the whole image in.
    fn min_scale(&self, size: Size) -> f64 {
        let min_x_scale = size.width / self.image_size.width;
        let min_y_scale = size.height / self.image_size.height;
        min_x_scale.min(min_y_scale)
    }

    /// Invalidate the image cache.
    fn invalidate_image(&mut self) {
        self.image = None;
        self.scale = 0.;
        self.offset = (0., 0.).into();
        self.draw_scale = 0.;
        self.draw_offset = (0., 0.).into();
    }

    /// The actual size we will draw.
    fn draw_size(&self) -> Size {
        self.image_size * self.draw_scale
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
                ctx.submit_command(scale_cmd(self.scale));
                ctx.request_paint();
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
            LifeCycle::Size(size) => {
                self.scale = self.scale.min(self.min_scale(*size));
                self.offset = self.constrain_offset(self.offset, *size);
                self.draw_scale = self.scale;
                self.draw_offset = self.offset;
                ctx.submit_command(scale_cmd(self.scale));
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
                self.image_size = next.size();
                self.scale = self.min_scale(ctx.size());
                self.draw_scale = self.scale;
                ctx.submit_command(scale_cmd(self.scale));
                ctx.request_paint();
            }
            (None, Some(next)) => {
                self.invalidate_image();
                self.image_size = next.size();
                self.scale = self.min_scale(ctx.size());
                self.draw_scale = self.scale;
                ctx.submit_command(scale_cmd(self.scale));
                ctx.request_paint();
            }
            (Some(_), None) => {
                self.invalidate_image();
                self.image_size = Size::ZERO;
                self.scale = self.min_scale(ctx.size());
                self.draw_scale = self.scale;
                ctx.submit_command(scale_cmd(self.scale));
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
        let size = ctx.size();
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
                self.image = Some(data.to_piet_image(&mut **ctx));
            }
            let draw_size = self.draw_size();
            if self.draw_size().is_empty() {
                return;
            }
            let draw_offset = self.draw_offset(size);
            ctx.draw_image(
                self.image.as_ref().unwrap(),
                Rect::from_origin_size(draw_offset, draw_size),
                InterpolationMode::NearestNeighbor,
            );
        }
    }
}

fn scale_cmd(scale: f64) -> Command {
    // todo make the target use the widget id.
    Command::new(UPDATE_SCALE, scale, Target::Global)
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
