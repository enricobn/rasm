set -e
time RUST_LOG=info RUST_BACKTRACE=full cargo run --release build rasm/resources/examples/breakout -o breakout
time RUST_LOG=info RUST_BACKTRACE=full cargo run --release build rasm/resources/examples/gameoflife -o gameoflife
time RUST_LOG=info RUST_BACKTRACE=full cargo run --release build rasm/resources/examples/gameoflife_sdl -o gameoflife_sdl
time RUST_LOG=info RUST_BACKTRACE=full cargo run --release build rasm/resources/examples/gameoflife_vec -o gameoflife_vec
time RUST_LOG=info RUST_BACKTRACE=full cargo run --release build rasm/resources/examples/gameoflife_vec_sdl -o gameoflife_vec_sdl
time RUST_LOG=info RUST_BACKTRACE=full cargo run --release build rasm/resources/examples/showimage -o showimage
time RUST_LOG=info RUST_BACKTRACE=full cargo run --release build rasm/resources/examples/bouncing_quads -o bouncing_quads
time RUST_LOG=info RUST_BACKTRACE=full cargo run --release build rasm/resources/test/fibonacci.rasm -o fibonacci