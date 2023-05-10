use std::time::Instant;

pub struct Timer {
    active: bool,
    timestamp: Instant,
}

impl Timer {
    pub fn new(active: bool) -> Timer {
        Timer {
            active,
            timestamp: timestamp(),
        }
    }

    pub fn stop(&mut self) -> f32 {
        assert!(self.active);
        let curr = timestamp();
        let last = self.timestamp;
        self.timestamp = curr;

        (curr - last).as_millis() as f32
    }

    pub fn stop_with<F>(&self, f: F) -> u128
    where
        F: FnOnce(u128),
    {
        if self.active {
            let ts = timestamp() - self.timestamp;

            f(ts.as_millis());

            ts.as_nanos()
        } else {
            0u128
        }
    }
}

pub fn in_ms(ns: u64) -> f32 {
    (ns as f32) / 1000.0 / 1000.0
}

pub fn timestamp() -> Instant {
    Instant::now()
}
