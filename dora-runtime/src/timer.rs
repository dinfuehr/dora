use std::time::Instant;

pub struct Timer {
    start: Option<Instant>,
}

impl Timer {
    pub fn new(active: bool) -> Timer {
        let start = if active { Some(Instant::now()) } else { None };

        Timer { start }
    }

    pub fn stop(&mut self) -> f32 {
        let duration = self.start.expect("not started").elapsed();

        duration.as_secs_f32() / 1000f32
    }

    pub fn stop_with<F>(&self, f: F)
    where
        F: FnOnce(f32),
    {
        if let Some(start) = self.start {
            let duration = start.elapsed();

            f(duration.as_secs_f32() / 1000f32);
        }
    }
}
