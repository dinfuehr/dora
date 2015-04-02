use std::default::Default;

#[derive(PartialEq,Eq,Copy,Debug)]
pub enum Leave {
    Fix, Maybe, Not
}

impl Leave {
    fn seq(self: Leave, other: Leave) -> Leave {
        if self == Leave::Fix {
            return Leave::Fix;
        }

        match other {
            Leave::Fix => Leave::Fix,
            Leave::Maybe => Leave::Maybe,
            Leave::Not => self
        }
    }

    fn merge(self: Leave, other: Leave) -> Leave {
        if self == other {
            self
        } else if self == Leave::Fix || other == Leave::Fix {
            Leave::Maybe
        } else if self == Leave::Maybe || other == Leave::Maybe {
            Leave::Maybe
        } else {
            Leave::Not
        }
    }
}

#[derive(Debug)]
pub struct ReturnState {
    pub returns: Leave,
    pub breaks: Leave,
    pub continues: Leave,

    pub stops: bool,
}

impl ReturnState {
    pub fn new() -> ReturnState {
        ReturnState {
            returns: Leave::Not,
            breaks: Leave::Not,
            continues: Leave::Not,
            stops: false,
        }
    }

    pub fn returns() -> ReturnState {
        ReturnState {
            returns: Leave::Fix,
            breaks: Leave::Not,
            continues: Leave::Not,
            stops: true,
        }
    }

    pub fn breaks() -> ReturnState {
        ReturnState {
            returns: Leave::Not,
            breaks: Leave::Fix,
            continues: Leave::Not,
            stops: true,
        }
    }

    pub fn continues() -> ReturnState {
        ReturnState {
            returns: Leave::Not,
            breaks: Leave::Not,
            continues: Leave::Fix,
            stops: true,
        }
    }

    pub fn for_while(s: ReturnState) -> ReturnState {
        ReturnState {
            returns: s.returns,
            breaks: Leave::Not,
            continues: Leave::Not,
            stops: s.returns == Leave::Fix,
        }
    }

    pub fn for_loop(s: ReturnState) -> ReturnState {
        ReturnState {
            returns: s.returns,
            breaks: Leave::Not,
            continues: Leave::Not,
            stops: s.returns == Leave::Fix || s.breaks == Leave::Not,
        }
    }

    pub fn seq(self: &mut ReturnState, o: ReturnState) {
        assert!(!self.stops);

        self.returns = self.returns.seq(o.returns);
        self.continues = self.continues.seq(o.continues);
        self.breaks = self.breaks.seq(o.breaks);
        self.stops = o.stops;
    }

    pub fn merge(self: ReturnState, o: ReturnState) -> ReturnState {
        ReturnState {
            returns: self.returns.merge(o.returns),
            breaks: self.breaks.merge(o.breaks),
            continues: self.continues.merge(o.continues),
            stops: self.stops && o.stops,
        }
    }
}

impl Default for ReturnState {
    fn default() -> ReturnState {
        ReturnState::new()
    }
}
