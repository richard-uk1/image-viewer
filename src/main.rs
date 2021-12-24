mod widgets;

use crossbeam_channel::{self as channel, Receiver, RecvError};
use druid::{
    commands::{OPEN_FILE, SHOW_OPEN_PANEL},
    kurbo::Point,
    theme,
    widget::{prelude::*, Flex, Label, Maybe, Svg},
    AppDelegate, AppLauncher, ArcStr, Command, Data, DelegateCtx, Env, ExtEventSink,
    FileDialogOptions, FileSpec, Handled, ImageBuf, Lens, Selector, SingleUse, Target, Widget,
    WidgetExt, WidgetPod, WindowDesc,
};
use notify::{RecommendedWatcher, RecursiveMode, Watcher};
use qu::ick_use::*;
use std::{error::Error, path::PathBuf, sync::Arc, thread, time::Duration};

use crate::widgets::{ZoomImage, NOTIFY_TRANSFORM, SET_SCALE, ZOOM};

const FILE_LOADED: Selector<SingleUse<Result<ImageBuf, Box<dyn Error + Send + Sync>>>> =
    Selector::new("image-viewer.file-loaded");
const OPEN_IMAGE_SVG: &str = include_str!(concat!(env!("CARGO_MANIFEST_DIR"), "/image.svg"));
const ZOOM_SVG: &str = include_str!(concat!(env!("CARGO_MANIFEST_DIR"), "/zoom.svg"));
const ALL_IMAGES: FileSpec = FileSpec::new("Image", &["jpg", "jpeg", "gif", "bmp", "png"]);

#[derive(Debug, Clone, Data, Lens)]
struct AppData {
    image: Option<Arc<ImageBuf>>,
    error: ArcStr,
    info: ArcStr,
}

impl AppData {
    fn new() -> Self {
        Self {
            image: None,
            error: "".into(),
            info: "".into(),
        }
    }

    fn set_image(&mut self, image: Arc<ImageBuf>) {
        self.image = Some(image);
        self.error = "".into()
    }

    fn set_error(&mut self, error: ArcStr) {
        self.image = None;
        self.error = error;
    }
}

#[qu::ick]
pub fn main() -> Result {
    let main_window = WindowDesc::new(ui_builder()).title("Image Viewer");
    // Set our initial data
    let data = AppData::new();
    let launcher = AppLauncher::with_window(main_window);

    // worker thread for IO
    let (ui_tx, ui_rx) = channel::unbounded::<UiMsg>();
    let mut io_state = IoState::new(ui_rx, launcher.get_external_handle())?;
    let io_thread = thread::spawn(move || io_state.run());

    launcher
        .delegate(Delegate {
            ui_tx: ui_tx.clone(),
        })
        .launch(data)
        .expect("launch failed");

    // shut down gracefully
    ui_tx.send(UiMsg::Shutdown).unwrap();
    io_thread.join().unwrap();
    Ok(())
}

/// State for the i/o thread
struct IoState {
    ui_rx: Receiver<UiMsg>,
    evt_sink: ExtEventSink,
    open_file: Option<PathBuf>,
    watcher: RecommendedWatcher,
    watcher_rx: Receiver<Result<notify::Event, notify::Error>>,
}

impl IoState {
    fn new(ui_rx: Receiver<UiMsg>, evt_sink: ExtEventSink) -> Result<Self> {
        let (watcher_tx, watcher_rx) = channel::unbounded();
        Ok(Self {
            ui_rx,
            evt_sink,
            open_file: None,
            watcher: notify::recommended_watcher(watcher_tx)?,
            watcher_rx,
        })
    }
    fn run(&mut self) {
        loop {
            channel::select! {
                recv(self.ui_rx) -> msg => if !self.handle_ui(msg) {
                    break;
                },
                recv(self.watcher_rx) -> msg => if !self.handle_notify(msg) {
                    break;
                }
            }
        }
    }

    // returns false on error
    fn handle_ui(&mut self, msg: Result<UiMsg, RecvError>) -> bool {
        match msg {
            Ok(UiMsg::LoadImage(path)) => self.load_img(path),
            Ok(UiMsg::Shutdown) | Err(_) => false,
        }
    }

    fn handle_notify(
        &mut self,
        msg: Result<Result<notify::Event, notify::Error>, RecvError>,
    ) -> bool {
        let msg = match msg {
            Ok(v) => v,
            Err(e) => {
                log::error!("{}", e);
                return false;
            }
        };
        let evt = match msg {
            Ok(v) => v,
            Err(e) => {
                log::error!("{}", e);
                return false;
            }
        };
        match &evt.kind {
            notify::EventKind::Modify(_) => {
                if let Some(path) = self.open_file.take() {
                    // sleep for a bit to let the write finish
                    thread::sleep(Duration::from_millis(1000));
                    self.load_img(path)
                } else {
                    true
                }
            }
            _ => true,
        }
    }

    fn load_img(&mut self, path: PathBuf) -> bool {
        if let Some(prev) = self.open_file.as_ref() {
            self.watcher.unwatch(prev).unwrap(); // TODO handle errors
        }
        self.open_file = None;
        let image = ImageBuf::from_file(&path);
        // only update state if the load was successful.
        log::debug!("watching {}", path.display());
        self.watcher
            .watch(&path, RecursiveMode::NonRecursive)
            .unwrap();
        self.open_file = Some(path.clone());
        if let Err(_) =
            self.evt_sink
                .submit_command(FILE_LOADED, SingleUse::new(image), Target::Global)
        {
            log::error!("should be unreachable");
            false
        } else {
            true
        }
    }
}

fn ui_builder() -> impl Widget<AppData> {
    let ribbon = Flex::row()
        .with_child(open_button())
        .with_child(zoom_out_button())
        .with_child(zoom_1_button())
        .with_child(zoom_in_button())
        .align_left();
    Flex::column()
        .with_child(ribbon)
        .with_flex_child(
            Maybe::or_empty(|| ZoomImage::new())
                .lens(AppData::image)
                .center(),
            1.0,
        )
        .with_child(
            Flex::row()
                .with_child(Label::raw().lens(AppData::error))
                .with_flex_spacer(1.)
                .with_child(Label::raw().lens(AppData::info)),
        )
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

const ZOOM_FACTOR: f64 = 1.5;

fn zoom_out_button() -> impl Widget<AppData> {
    BgHover::new(
        Flex::column()
            .with_child(
                Svg::new(OPEN_IMAGE_SVG.parse().unwrap())
                    .fix_height(30.)
                    .fix_width(50.),
            )
            // no need for spacer because of spacing around image
            .with_child(Label::new("<"))
            .padding(4.)
            .on_click(|ctx, _, _| {
                ctx.submit_command(ZOOM.with(ZOOM_FACTOR.recip()));
            }),
    )
}

fn zoom_1_button() -> impl Widget<AppData> {
    BgHover::new(
        Flex::column()
            .with_child(
                Svg::new(ZOOM_SVG.parse().unwrap())
                    .fix_height(30.)
                    .fix_width(50.),
            )
            // no need for spacer because of spacing around image
            .with_child(Label::new("100%"))
            .padding(4.)
            .on_click(|ctx, _, _| {
                ctx.submit_command(SET_SCALE.with(1.));
            }),
    )
}

fn zoom_in_button() -> impl Widget<AppData> {
    BgHover::new(
        Flex::column()
            .with_child(
                Svg::new(OPEN_IMAGE_SVG.parse().unwrap())
                    .fix_height(30.)
                    .fix_width(50.),
            )
            // no need for spacer because of spacing around image
            .with_child(Label::new(">"))
            .padding(4.)
            .on_click(|ctx, _, _| {
                ctx.submit_command(ZOOM.with(ZOOM_FACTOR));
            }),
    )
}

enum UiMsg {
    LoadImage(PathBuf),
    Shutdown,
}

struct Delegate {
    ui_tx: channel::Sender<UiMsg>,
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
            if let Err(e) = self.ui_tx.send(UiMsg::LoadImage(file.path().to_owned())) {
                data.set_error(format!("error sending message to io thread: {}", e).into());
            }
            Handled::Yes
        } else if let Some(img) = cmd.get(FILE_LOADED) {
            match img.take().unwrap() {
                Ok(img) => data.set_image(Arc::new(img)),
                Err(e) => data.set_error(format!("error decoding/loading image: {}", e).into()),
            }
            Handled::Yes
        } else if let Some(trans) = cmd.get(NOTIFY_TRANSFORM) {
            let (translate, scale) = trans.as_tuple();
            data.info = format!(
                "scale: {:4.0}% translate: ({:.0},{:.0})",
                // little fiddle to get correct values
                scale.recip() * 100.,
                translate.x.max(0.),
                translate.y.max(0.),
            )
            .into();
            Handled::No
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
    fn update(&mut self, ctx: &mut UpdateCtx, _old_data: &T, data: &T, env: &Env) {
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
