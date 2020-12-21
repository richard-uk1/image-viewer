mod widgets;

use crossbeam_channel as channel;
use druid::{
    commands::{OPEN_FILE, SHOW_OPEN_PANEL},
    kurbo::Point,
    theme,
    widget::{prelude::*, Flex, Label, Svg},
    AppDelegate, AppLauncher, ArcStr, Command, Data, DelegateCtx, Env, FileDialogOptions, FileSpec,
    Handled, ImageBuf, Lens, Selector, SingleUse, Target, Widget, WidgetExt, WidgetPod, WindowDesc,
};
use std::{error::Error, path::PathBuf, sync::Arc, thread};

use crate::widgets::ZoomImage;

const FILE_LOADED: Selector<SingleUse<Result<ImageBuf, Box<dyn Error + Send + Sync>>>> =
    Selector::new("image-viewer.file-loaded");
const OPEN_IMAGE_SVG: &str = include_str!(concat!(env!("CARGO_MANIFEST_DIR"), "/image.svg"));
const ALL_IMAGES: FileSpec = FileSpec::new("Image", &["jpg", "jpeg", "gif", "bmp", "png"]);

#[derive(Debug, Clone, Data, Lens)]
struct AppData {
    image: Option<Arc<ImageBuf>>,
    error: ArcStr,
}

impl AppData {
    fn set_image(&mut self, image: Arc<ImageBuf>) {
        self.image = Some(image);
        self.error = "".into()
    }

    fn set_error(&mut self, error: ArcStr) {
        self.image = None;
        self.error = error;
    }
}

pub fn main() {
    let main_window = WindowDesc::new(ui_builder).title("Image Viewer");
    // Set our initial data
    let data = AppData {
        image: None,
        error: "".into(),
    };
    let launcher = AppLauncher::with_window(main_window).use_simple_logger();

    // worker thread for IO
    let evt_sink = launcher.get_external_handle();
    let (gui_send, io_recv) = channel::unbounded::<UiMsg>();
    let io_thread = thread::spawn(move || loop {
        match io_recv.recv() {
            Ok(UiMsg::LoadImage(path)) => {
                if let Err(_) = evt_sink.submit_command(
                    FILE_LOADED,
                    SingleUse::new(ImageBuf::from_file(path)),
                    Target::Global,
                ) {
                    log::error!("should be unreachable");
                    break;
                }
            }
            Ok(UiMsg::Shutdown) | Err(_) => break,
        }
    });

    launcher
        .delegate(Delegate {
            gui_send: gui_send.clone(),
        })
        .launch(data)
        .expect("launch failed");

    // shut down gracefully
    gui_send.send(UiMsg::Shutdown).unwrap();
    io_thread.join().unwrap();
}

fn ui_builder() -> impl Widget<AppData> {
    let ribbon = Flex::row().with_child(open_button()).align_left();
    Flex::column()
        .with_child(ribbon)
        .with_flex_child(ZoomImage::new().lens(AppData::image).center(), 1.0)
        .with_child(Label::raw().lens(AppData::error).align_left())
    //.debug_paint_layout()
}

fn open_button() -> impl Widget<AppData> {
    BgHover::new(
        Flex::column()
            .with_child(
                Svg::new(OPEN_IMAGE_SVG.parse().unwrap())
                    .fix_height(30.)
                    .fix_width(50.),
            )
            // no need for spacer because of spacing around image
            .with_child(Label::new("Open"))
            .padding(4.)
            .on_click(|ctx, _, _| {
                ctx.submit_command(SHOW_OPEN_PANEL.with(
                    FileDialogOptions::new().allowed_types(vec![
                        ALL_IMAGES,
                        FileSpec::JPG,
                        FileSpec::GIF,
                    ]),
                ));
            }),
    )
}

enum UiMsg {
    LoadImage(PathBuf),
    Shutdown,
}

struct Delegate {
    gui_send: channel::Sender<UiMsg>,
}

impl AppDelegate<AppData> for Delegate {
    fn command(
        &mut self,
        _ctx: &mut DelegateCtx,
        _target: Target,
        cmd: &Command,
        data: &mut AppData,
        _env: &Env,
    ) -> Handled {
        if let Some(file) = cmd.get(OPEN_FILE) {
            if let Err(e) = self.gui_send.send(UiMsg::LoadImage(file.path().to_owned())) {
                data.set_error(format!("error sending message to io thread: {}", e).into());
            }
            Handled::Yes
        } else if let Some(img) = cmd.get(FILE_LOADED) {
            match img.take().unwrap() {
                Ok(img) => data.set_image(Arc::new(img)),
                Err(e) => data.set_error(format!("error decoding/loading image: {}", e).into()),
            }
            Handled::Yes
        } else {
            Handled::No
        }
    }
}

struct BgHover<T, W> {
    hot: bool,
    inner: WidgetPod<T, W>,
}

impl<T, W: Widget<T>> BgHover<T, W> {
    fn new(inner: W) -> Self {
        BgHover {
            hot: false,
            inner: WidgetPod::new(inner),
        }
    }
}

impl<T: Data, W: Widget<T>> Widget<T> for BgHover<T, W> {
    fn event(&mut self, ctx: &mut EventCtx, event: &Event, data: &mut T, env: &Env) {
        self.inner.event(ctx, event, data, env)
    }
    fn lifecycle(&mut self, ctx: &mut LifeCycleCtx, event: &LifeCycle, data: &T, env: &Env) {
        match event {
            LifeCycle::HotChanged(hot) => {
                self.hot = *hot;
                ctx.request_paint();
            }
            _ => (),
        }
        self.inner.lifecycle(ctx, event, data, env)
    }
    fn update(&mut self, ctx: &mut UpdateCtx, old_data: &T, data: &T, env: &Env) {
        self.inner.update(ctx, data, env)
    }
    fn layout(&mut self, ctx: &mut LayoutCtx, bc: &BoxConstraints, data: &T, env: &Env) -> Size {
        let size = self.inner.layout(ctx, bc, data, env);
        self.inner.set_origin(ctx, data, env, Point::ZERO);
        size
    }
    fn paint(&mut self, ctx: &mut PaintCtx, data: &T, env: &Env) {
        if self.hot {
            let r = ctx.size().to_rect();
            let bg = ctx.solid_brush(env.get(theme::BUTTON_DARK));
            ctx.fill(r, &bg);
        }
        self.inner.paint(ctx, data, env)
    }
}
