set -e
time RUST_LOG=info RUST_BACKTRACE=full cargo run --release rasm/resources/examples/breakout breakout
time RUST_LOG=info RUST_BACKTRACE=full cargo run --release rasm/resources/examples/gameoflife.rasm gameoflife
time RUST_LOG=info RUST_BACKTRACE=full cargo run --release rasm/resources/examples/gameoflife_sdl.rasm gameoflife_sdl
time RUST_LOG=info RUST_BACKTRACE=full cargo run --release rasm/resources/examples/gameoflife_vec.rasm gameoflife_vec
time RUST_LOG=info RUST_BACKTRACE=full cargo run --release rasm/resources/examples/gameoflife_vec_sdl.rasm gameoflife_vec_sdl
time RUST_LOG=info RUST_BACKTRACE=full cargo run --release rasm/resources/examples/showimage.rasm showimage
time RUST_LOG=info RUST_BACKTRACE=full cargo run --release rasm/resources/examples/bouncing_quads.rasm bouncing_quads