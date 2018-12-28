use time;

pub struct Timer {
    active: bool,
    timestamp: u64,
}

impl Timer {
    pub fn new(active: bool) -> Timer {
        let ts = if active { timestamp() } else { 0 };

        Timer {
            active: active,
            timestamp: ts,
        }
    }

    pub fn stop(&self) -> f32 {
        assert!(self.active);

        in_ms(timestamp() - self.timestamp)
    }

    pub fn stop_with<F>(&self, f: F) -> u64
    where
        F: FnOnce(f32),
    {
        if self.active {
            let ts = timestamp() - self.timestamp;

            f(in_ms(ts));

            ts
        } else {
            0
        }
    }

    pub fn ms<F>(active: bool, f: F) -> f32
    where
        F: FnOnce(),
    {
        if active {
            let ts = timestamp();
            f();
            let diff = timestamp() - ts;
            in_ms(diff)
        } else {
            f();
            0.0f32
        }
    }
}

pub fn in_ms(ns: u64) -> f32 {
    (ns as f32) / 1000.0 / 1000.0
}

pub fn timestamp() -> u64 {
    time::precise_time_ns()
}
