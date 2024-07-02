mkdir -p tmp
set -e
time RUST_LOG=info RUST_BACKTRACE=full cargo run --release build rasm/resources/examples/breakout -o tmp/breakout
time RUST_LOG=info RUST_BACKTRACE=full cargo run --release build rasm/resources/examples/gameoflife -o tmp/gameoflife
time RUST_LOG=info RUST_BACKTRACE=full cargo run --release build rasm/resources/examples/gameoflife_sdl -o tmp/gameoflife_sdl
time RUST_LOG=info RUST_BACKTRACE=full cargo run --release build rasm/resources/examples/gameoflife_vec -o tmp/gameoflife_vec
time RUST_LOG=info RUST_BACKTRACE=full cargo run --release build rasm/resources/examples/gameoflife_vec_sdl -o tmp/gameoflife_vec_sdl
time RUST_LOG=info RUST_BACKTRACE=full cargo run --release build rasm/resources/examples/showimage -o tmp/showimage
time RUST_LOG=info RUST_BACKTRACE=full cargo run --release build rasm/resources/examples/bouncing_quads -o tmp/bouncing_quads
time RUST_LOG=info RUST_BACKTRACE=full cargo run --release build rasm/resources/test/fibonacci.rasm -o tmp/fibonacci