pub const print_chunk = false;
pub const log_gc = false;
pub const stress_gc = false;

pub fn testable() bool {
    return !print_chunk and !log_gc and !stress_gc;
}
