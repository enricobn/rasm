set -e
cargo run --release -- install stdlib
cargo run --release -- install rasm/resources/examples/sdl/
cargo run --release -- install rasm/resources/examples/uv/
cargo run --release -- install rasm/resources/examples/rasm2d/