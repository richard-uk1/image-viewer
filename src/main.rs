use crossbeam_channel as channel;
use druid::{
    commands::{OPEN_FILE, SHOW_OPEN_PANEL},
    piet::ImageFormat,
    widget::{Controller, Flex, Image, Label, Svg},
    AppDelegate, AppLauncher, ArcStr, Command, Data, DelegateCtx, Env, FileDialogOptions, FileSpec,
    Handled, ImageBuf, Lens, Selector, SingleUse, Target, UpdateCtx, Widget, WidgetExt, WindowDesc,
};
use once_cell::sync::OnceCell;
use std::{error::Error, path::PathBuf, sync::Arc, thread};

const FILE_LOADED: Selector<SingleUse<Result<ImageBuf, Box<dyn Error + Send + Sync>>>> =
    Selector::new("image-viewer.file-loaded");

const OPEN_IMAGE_SVG: &str = include_str!(concat!(env!("CARGO_MANIFEST_DIR"), "/image.svg"));

#[derive(Debug, Clone, Data, Lens)]
struct AppData {
    image: Arc<ImageBuf>,
    error: ArcStr,
}

impl AppData {
    fn set_image(&mut self, image: Arc<ImageBuf>) {
        self.image = image;
        self.error = "".into()
    }

    fn set_error(&mut self, error: ArcStr) {
        self.image = Arc::new(empty_img());
        self.error = error;
    }
}

pub fn main() {
    let main_window = WindowDesc::new(ui_builder).title("Image Viewer");
    // Set our initial data
    let data = AppData {
        image: Arc::new(empty_img()),
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
        .with_flex_child(Image::new(empty_img()).controller(ImageCtrl).center(), 1.0)
        .with_child(Label::raw().lens(AppData::error).align_left())
    //.debug_paint_layout()
}

fn open_button() -> impl Widget<AppData> {
    Flex::column()
        .with_child(Svg::new(OPEN_IMAGE_SVG.parse().unwrap()).fix_height(30.))
        // no need for spacer because of spacing around image
        .with_child(Label::new("Open"))
        .padding(4.)
        .on_click(|ctx, _, _| {
            ctx.submit_command(
                SHOW_OPEN_PANEL.with(FileDialogOptions::new().allowed_types(vec![
                    FileSpec::PNG,
                    FileSpec::JPG,
                    FileSpec::GIF,
                ])),
            );
        })
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

struct ImageCtrl;

impl Controller<AppData, Image> for ImageCtrl {
    fn update(
        &mut self,
        child: &mut Image,
        ctx: &mut UpdateCtx,
        old_data: &AppData,
        data: &AppData,
        _env: &Env,
    ) {
        if !Arc::ptr_eq(&data.image, &old_data.image) {
            child.set_image_data((*data.image).clone());
            ctx.request_layout();
        }
    }
}

/// Workaround because an empty image will crash piet-cairo
fn empty_img() -> ImageBuf {
    static EMPTY: OnceCell<Arc<[u8]>> = OnceCell::new();
    let buf = EMPTY.get_or_init(|| {
        let buf = [0u8; 4];
        Arc::new(buf)
    });
    ImageBuf::from_raw((*buf).clone(), ImageFormat::RgbaSeparate, 1, 1)
}
