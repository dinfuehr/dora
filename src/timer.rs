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

    pub fn stop_with<F>(&mut self, f: F) -> u64
        where F: FnOnce(u64)
    {
        if self.active {
            let ts = timestamp() - self.timestamp;

            f(ts);

            ts
        } else {
            0
        }
    }
}

pub fn in_ms(ns: u64) -> u64 {
    ns / 1000 / 1000
}

fn timestamp() -> u64 {
    time::precise_time_ns()
}
