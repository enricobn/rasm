set -e
time RUST_LOG=info RUST_BACKTRACE=full cargo run --release resources/examples/breakout/breakout.rasm
time RUST_LOG=info RUST_BACKTRACE=full cargo run --release resources/examples/gameoflife.rasm
time RUST_LOG=info RUST_BACKTRACE=full cargo run --release resources/examples/gameoflife_sdl.rasm
time RUST_LOG=info RUST_BACKTRACE=full cargo run --release resources/examples/gameoflife_vec.rasm
time RUST_LOG=info RUST_BACKTRACE=full cargo run --release resources/examples/gameoflife_vec_sdl.rasm
time RUST_LOG=info RUST_BACKTRACE=full cargo run --release resources/examples/showimage.rasm
