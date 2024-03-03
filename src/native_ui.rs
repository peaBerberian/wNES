use sdl2::event::Event;
use sdl2::keyboard::Keycode;
// use sdl2::pixels::Color;
use sdl2::pixels::PixelFormatEnum;
// use sdl2::EventPump;

use crate::controller::NesController;
use crate::ppu::Frame;

#[derive(Clone, Debug)]
pub(crate) enum NativeUiError {
    UnknownSdl2Error(String),
}

impl std::error::Error for NativeUiError {}

impl std::fmt::Display for NativeUiError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            NativeUiError::UnknownSdl2Error(msg) => write!(f, "Sdl2Error: {msg}"),
        }
    }
}

pub(crate) trait WNesUi: Sized {
    fn render_frame(
        &mut self,
        frame: &Frame,
        ctrls: (&mut NesController, &mut NesController)
    );
}

pub(crate) struct NativeUi {
    event_pump: sdl2::EventPump,
    canvas: sdl2::render::WindowCanvas,
    texture_creator: sdl2::render::TextureCreator<sdl2::video::WindowContext>,
}

impl NativeUi {
    pub(crate) fn try_new() -> Result<Self, NativeUiError> {
        // init sdl2
        let sdl_context = sdl2::init()?;
        let window = sdl_context
            .video()?
            .window("wNES", (256.0 * 3.0) as u32, (240.0 * 3.0) as u32)
            .position_centered()
            .build()?;

        let event_pump = sdl_context.event_pump()?;

        let mut canvas = window.into_canvas().present_vsync().build()?;
        canvas.set_scale(3.0, 3.0)?;

        let creator = canvas.texture_creator();
        // let texture = creator
        //     .create_texture_target(PixelFormatEnum::RGB24, 256, 240)?;

        // // let mut rng = rand::thread_rng();

        Ok(Self {
            event_pump,
            canvas,
            texture_creator: creator,
        })
    }
}

impl WNesUi for NativeUi {
    fn render_frame(
        &mut self,
        frame: &Frame,
        (controller1, _): (&mut NesController, &mut NesController),
    ) {
        let mut texture = self
            .texture_creator
            .create_texture_target(PixelFormatEnum::RGB24, 256, 240)
            .unwrap();
        texture.update(None, &frame.data, 256 * 3).unwrap();
        self.canvas.copy(&texture, None, None).unwrap();
        self.canvas.present();
        for event in self.event_pump.poll_iter() {
            match event {
                Event::Quit { .. }
                | Event::KeyDown {
                    keycode: Some(Keycode::Escape),
                    ..
                } => std::process::exit(0),
                Event::KeyDown {
                    keycode: Some(kc), ..
                } => match kc {
                    Keycode::Up => controller1.set_up(true),
                    Keycode::Down => controller1.set_down(true),
                    Keycode::Left => controller1.set_left(true),
                    Keycode::Right => controller1.set_right(true),
                    Keycode::A => controller1.set_a(true),
                    Keycode::B => controller1.set_b(true),
                    Keycode::Return => controller1.set_start(true),
                    Keycode::Backspace => controller1.set_select(true),
                    _ => {}
                },
                Event::KeyUp {
                    keycode: Some(kc), ..
                } => match kc {
                    Keycode::Up => controller1.set_up(false),
                    Keycode::Down => controller1.set_down(false),
                    Keycode::Left => controller1.set_left(false),
                    Keycode::Right => controller1.set_right(false),
                    Keycode::A => controller1.set_a(false),
                    Keycode::B => controller1.set_b(false),
                    Keycode::Return => controller1.set_start(false),
                    Keycode::Backspace => controller1.set_select(false),
                    _ => {}
                },
                _ => { /* do nothing */ }
            }
        }
    }
}

impl From<String> for NativeUiError {
    fn from(err: String) -> Self {
        NativeUiError::UnknownSdl2Error(err)
    }
}

impl From<sdl2::video::WindowBuildError> for NativeUiError {
    fn from(err: sdl2::video::WindowBuildError) -> Self {
        NativeUiError::UnknownSdl2Error(err.to_string())
    }
}

impl From<sdl2::IntegerOrSdlError> for NativeUiError {
    fn from(err: sdl2::IntegerOrSdlError) -> Self {
        NativeUiError::UnknownSdl2Error(err.to_string())
    }
}

impl From<sdl2::render::TextureValueError> for NativeUiError {
    fn from(err: sdl2::render::TextureValueError) -> Self {
        NativeUiError::UnknownSdl2Error(err.to_string())
    }
}
