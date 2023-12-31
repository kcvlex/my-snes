use sdl2::audio::{AudioQueue, AudioSpecDesired};

pub struct AudioPlayer {
    buf: Vec<(i16, i16)>,
    queue: AudioQueue<i16>,
}

const FREQ: u32 = 32000;

impl AudioPlayer {
    pub fn init(ctx: &sdl2::Sdl) -> Result<Self, String> {
        let audio_subsystem = ctx.audio()?;
        let spec = AudioSpecDesired {
            freq: Some(FREQ as i32),
            channels: Some(2),
            samples: None,
        };
        let queue = audio_subsystem.open_queue(None, &spec)?;
        Ok(Self {
            buf: Vec::new(),
            queue,
        })
    }

    pub fn resume(&mut self) {
        self.queue.resume()
    }

    pub fn push(&mut self, left: i16, right: i16) {
        self.buf.push((left, right))
    }

    pub fn wait_finish(&self) {
        while 0 < self.queue.size() {
            std::thread::sleep(std::time::Duration::from_millis(30));
        }
    }

    fn do_send(&mut self) -> Result<(), String> {
        let data = self.buf.iter().flat_map(|v| [v.0, v.1]).collect::<Vec<i16>>();
        self.buf.clear();
        self.queue.queue_audio(&data)?;
        Ok(())
    }

    pub fn send_force(&mut self) -> Result<(), String> {
        self.do_send()
    }

    pub fn send(&mut self) -> Result<(), String> {
        if FREQ / 10 < self.queue.size() {
            return Ok(());
        }
        self.do_send()
    }
}
