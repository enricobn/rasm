set -e
time RUST_LOG=info RUST_BACKTRACE=full cargo run --release build -f rasm/resources/examples/breakout -o breakout
time RUST_LOG=info RUST_BACKTRACE=full cargo run --release build -f rasm/resources/examples/gameoflife.rasm -o gameoflife
time RUST_LOG=info RUST_BACKTRACE=full cargo run --release build -f rasm/resources/examples/gameoflife_sdl -o gameoflife_sdl
time RUST_LOG=info RUST_BACKTRACE=full cargo run --release build -f rasm/resources/examples/gameoflife_vec.rasm -o gameoflife_vec
time RUST_LOG=info RUST_BACKTRACE=full cargo run --release build -f rasm/resources/examples/gameoflife_vec_sdl -o gameoflife_vec_sdl
time RUST_LOG=info RUST_BACKTRACE=full cargo run --release build -f rasm/resources/examples/showimage -o showimage
time RUST_LOG=info RUST_BACKTRACE=full cargo run --release build -f rasm/resources/examples/bouncing_quads -o bouncing_quads
time RUST_LOG=info RUST_BACKTRACE=full cargo run --release build -f rasm/resources/test/fibonacci.rasm -o fibonacci