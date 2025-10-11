cargo build --release
rm -r tmp
mkdir tmp
RUST_LOG=info RUST_BACKTRACE=full target/release/rasm build rasm/resources/examples/gameoflife_iter_sdl -t c -o tmp $1
echo "run..."
time tmp/gameoflife_iter_sdl rasm/resources/examples/f116.cells 3
