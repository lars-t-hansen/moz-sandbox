fn main() {
    println!("{}", fib(35))
}

fn fib(i:int) -> int {
    if i < 2 { return i; }
    fib(i-1) + fib(i-2)
}

