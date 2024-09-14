set -e
RUST_LOG=info RUST_BACKTRACE=full cargo run build rasm/resources/test/$1.rasm -t c -o tmp/$1
tmp/$1
