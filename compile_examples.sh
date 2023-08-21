set -e
time RUST_LOG=info RUST_BACKTRACE=full cargo run --release rasm/resources/examples/breakout breakout.out
time RUST_LOG=info RUST_BACKTRACE=full cargo run --release rasm/resources/examples/gameoflife.rasm gameoflife.out
time RUST_LOG=info RUST_BACKTRACE=full cargo run --release rasm/resources/examples/gameoflife_sdl.rasm gameoflife_sdl.out
time RUST_LOG=info RUST_BACKTRACE=full cargo run --release rasm/resources/examples/gameoflife_vec.rasm gameoflife_vec.out
time RUST_LOG=info RUST_BACKTRACE=full cargo run --release rasm/resources/examples/gameoflife_vec_sdl.rasm gameoflife_vec_sdl.out
time RUST_LOG=info RUST_BACKTRACE=full cargo run --release rasm/resources/examples/showimage.rasm showimage.out
time RUST_LOG=info RUST_BACKTRACE=full cargo run --release rasm/resources/examples/bouncing_quads.rasm bouncing_quads.out
time RUST_LOG=info RUST_BACKTRACE=full cargo run --release rasm/resources/test/fibonacci.rasm fibonacci.out