mkdir -p tmp
set -e
cargo build --release
time RUST_LOG=info RUST_BACKTRACE=full target/release/rasm build -t c rasm/resources/examples/breakout -o tmp $1
time RUST_LOG=info RUST_BACKTRACE=full target/release/rasm build -t c rasm/resources/examples/gameoflife -o tmp $1
time RUST_LOG=info RUST_BACKTRACE=full target/release/rasm build -t c rasm/resources/examples/gameoflife_sdl -o tmp $1
time RUST_LOG=info RUST_BACKTRACE=full target/release/rasm build -t c rasm/resources/examples/gameoflife_vec -o tmp $1
time RUST_LOG=info RUST_BACKTRACE=full target/release/rasm build -t c rasm/resources/examples/gameoflife_vec_sdl -o tmp $1
time RUST_LOG=info RUST_BACKTRACE=full target/release/rasm build -t c rasm/resources/examples/gameoflife_tc -o tmp $1
time RUST_LOG=info RUST_BACKTRACE=full target/release/rasm build -t c rasm/resources/examples/showimage -o tmp $1
time RUST_LOG=info RUST_BACKTRACE=full target/release/rasm build -t c rasm/resources/examples/bouncing_quads -o tmp $1
time RUST_LOG=info RUST_BACKTRACE=full target/release/rasm build -t c rasm/resources/test/fibonacci.rasm -o tmp $1