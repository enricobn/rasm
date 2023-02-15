set -e
time RUST_LOG=info RUST_BACKTRACE=full cargo run --release examples/breakout.rasm
time RUST_LOG=info RUST_BACKTRACE=full cargo run --release examples/gameoflife_sdl.rasm
time RUST_LOG=info RUST_BACKTRACE=full cargo run --release examples/gameoflife_vec.rasm
time RUST_LOG=info RUST_BACKTRACE=full cargo run --release examples/gameoflife_vec_sdl.rasm
time RUST_LOG=info RUST_BACKTRACE=full cargo run --release examples/showimage.rasm
