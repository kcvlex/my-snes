#![allow(dead_code)]

extern crate sdl2;

use sdl2::event::Event;
use sdl2::keyboard::Keycode;

use std::env;
use std::fs;

use media::{audio, render};
use my_snes::{emulator, joypad};

const SCREEN_WIDTH: u32 = 256 * 2;
const SCREEN_HEIGHT: u32 = 240 * 2;
const X_SCALE: u8 = 1;
const Y_SCALE: u8 = 1;

fn key_mapping(key: Keycode) -> Option<joypad::Button> {
    match key {
        Keycode::H => Some(joypad::Button::Left),
        Keycode::J => Some(joypad::Button::Down),
        Keycode::K => Some(joypad::Button::Up),
        Keycode::L => Some(joypad::Button::Right),
        Keycode::A => Some(joypad::Button::A),
        Keycode::B => Some(joypad::Button::B),
        Keycode::X => Some(joypad::Button::X),
        Keycode::Y => Some(joypad::Button::Y),
        Keycode::LShift => Some(joypad::Button::L),
        Keycode::RShift => Some(joypad::Button::R),
        Keycode::Space => Some(joypad::Button::Start),
        Keycode::LCtrl => Some(joypad::Button::Select),
        _ => None,
    }
}

fn main_(path: &str) -> Result<(), String> {
    env_logger::init();

    let rom = fs::read(path).expect("file not found");

    let context = sdl2::init()?;
    let renderer = render::Renderer::init(&context, "rust-sdl2_gfx: draw line & FPSManager", SCREEN_WIDTH, SCREEN_HEIGHT, X_SCALE, Y_SCALE)?;
    let audio_player = audio::AudioPlayer::init(&context)?;

    let mut events = context.event_pump()?;

    let mut emu = emulator::Emulator::make(&rom[..], renderer, audio_player).map_err(|e| e.to_string())?;

    emu.cpu_debug(true);
    emu.reset();
    emu.resume_audio();

    // emu.enable_debugger();
    'main: loop {
        for _ in 0..100 {
            emu.step();
        }
        for event in events.poll_iter() {
            emu.handle_event(&event, key_mapping);
            match event {
                Event::Quit { .. } => break 'main,
                Event::KeyDown { keycode: Some(Keycode::Escape), .. } => break 'main,
                // Event::KeyDown { keycode: Some(Keycode::M), .. } => pause = false,
                _ => {}
            }
        }
    }

    Ok(())
}

fn main() -> Result<(), String> {
    let args: Vec<String> = env::args().collect();
    let path = &args[1];
    main_(path)
}
