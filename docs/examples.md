# Examples

To compile the examples successfully, install some RASM libraries by running:

`./install_libs.sh`

In the examples, the `--` is really not needed when running manually from the command line, it's needed if you are running it with an IDE that parses markdown and lets you run the program from the IDE.

## breakout

```bash
cargo run --release -- build rasm/resources/examples/breakout/ -o .
```

a "breakout" executable will be created in the current folder

## fibonacci

```bash
cargo run --release -- build rasm/resources/test/fibonacci.rasm -o .
```

```bash
./fibonacci 40
```

it should print the 40th Fibonacci number (102334155)

## Fibonacci example

```rasm
pub fn fib(n: int) -> int {
    if(lessOrEqual(n, 1), n, {
        fib(n.sub(1)).add(fib(n.sub(2)));
    });
}

// Using stdlib
let nums = vec!(1, 2, 3, 4, 5);
let sum = nums.foldLeft(0, fn(acc, n) { acc.add(n); });
println!("Sum: {}", sum);
```