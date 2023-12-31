use sdl2::gfx::primitives::DrawRenderer;
use std::collections::VecDeque;
use sdl2::pixels::Color;

#[derive(Debug)]
pub struct Pixel {
    pub x: i16,
    pub y: i16,
    pub w: u8,
    pub h: u8,
    pub color: Color,
    pub frame: u32,
}

pub struct Renderer {
    buffer: VecDeque<Pixel>,
    canvas: sdl2::render::WindowCanvas,
    x_scale: u8,
    y_scale: u8,
}

impl Renderer {
    pub fn init(
        ctx: &sdl2::Sdl,
        title: &str, 
        width: u32,
        height: u32,
        x_scale: u8,
        y_scale: u8
    ) -> Result<Self, String> {
        let video_subsys = ctx.video()?;
        let canvas = video_subsys
            .window(title, width * (x_scale as u32), height * (y_scale as u32))
            .position_centered()
            .opengl()
            .build()
            .map_err(|e| e.to_string())?
            .into_canvas()
            .build()
            .map_err(|e| e.to_string())?;
        Ok(Self {
            buffer: VecDeque::new(),
            canvas,
            x_scale,
            y_scale,
        })
    }

    pub fn draw(&mut self, frame: u32) -> Result<(), String> {
        self.canvas
            .set_draw_color(sdl2::pixels::Color::RGB(0, 0, 0));
        self.canvas.clear();
        while !self.buffer.is_empty() {
            let fframe = self.buffer.front().unwrap().frame;
            if frame < fframe {
                break;
            }
            let front = self.buffer.pop_front().unwrap();
            let Pixel { x, y, w, h, color, .. } = front;
            let w = w as i16;
            let h = h as i16;
            self.canvas.rectangle(
                (x + 0) * self.x_scale as i16,
                (y + 0) * self.y_scale as i16,
                (x + w) * self.x_scale as i16,
                (y + h) * self.y_scale as i16,
                color,
            )?;
        }
        self.canvas.present();
        Ok(())
    }

    pub fn push(&mut self, pixel: Pixel) {
        self.buffer.push_back(pixel);
    }
}
