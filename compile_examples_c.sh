mkdir -p tmp
set -e
cargo build --release
time RUST_LOG=info RUST_BACKTRACE=full target/release/rasm build -t c rasm/resources/examples/breakout -o tmp/breakout $1
time RUST_LOG=info RUST_BACKTRACE=full target/release/rasm build -t c rasm/resources/examples/gameoflife -o tmp/gameoflife $1
time RUST_LOG=info RUST_BACKTRACE=full target/release/rasm build -t c rasm/resources/examples/gameoflife_sdl -o tmp/gameoflife_sdl $1
time RUST_LOG=info RUST_BACKTRACE=full target/release/rasm build -t c rasm/resources/examples/gameoflife_vec -o tmp/gameoflife_vec $1
time RUST_LOG=info RUST_BACKTRACE=full target/release/rasm build -t c rasm/resources/examples/gameoflife_vec_sdl -o tmp/gameoflife_vec_sdl $1
time RUST_LOG=info RUST_BACKTRACE=full target/release/rasm build -t c rasm/resources/examples/showimage -o tmp/showimage $1
time RUST_LOG=info RUST_BACKTRACE=full target/release/rasm build -t c rasm/resources/examples/bouncing_quads -o tmp/bouncing_quads $1
time RUST_LOG=info RUST_BACKTRACE=full target/release/rasm build -t c rasm/resources/test/fibonacci.rasm -o tmp/fibonacci $1