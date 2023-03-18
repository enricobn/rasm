use std::cmp::min;

const N: u32 = 3;

#[cfg(test)]
#[test]
fn test_f32() {
    //let mut rng = rand::thread_rng();

    print_n(-7.375);
    print_n(-7.4);
    print_n(-0.375);
    print_n(-1.375);
    print_n(-1.3754321);
    print_n(f32::INFINITY);
    print_n(f32::NEG_INFINITY);
    print_n(f32::NAN);
}

fn print_n(n: f32) {
    println!("{}", n);
    let to_bits = n.to_bits();

    /*
    let bits = format!("{:b}", to_bits);
    let sign_s = bits.get(0..1).unwrap().to_string();
    let exponent_s = bits.get(1..9).unwrap().to_string();
    let mantissa_s = bits.get(9..).unwrap().to_string();
    println!("{bits}\nsign {sign_s}\nexponent {exponent_s}\nmantissa {mantissa_s}");

     */

    let sign = to_bits & 0x80000000;

    if sign != 0 {
        println!("  sign -");
    } else {
        println!("  sign +");
    }

    let mantissa = to_bits & 0x7FFFFF;
    let real_mantissa = mantissa | 0x800000;
    let mut exponent = to_bits & 0x7F800000;
    exponent >>= 23;

    if exponent == 255 {
        if mantissa == 0 {
            println!("  infinity");
        } else {
            println!("  NaN");
        }
    } else if exponent == 0 {
        print_denorm(real_mantissa);
    } else if exponent < 127 {
        let biased_exponent = 127 - exponent;
        print_negative_exponent(real_mantissa, biased_exponent);
    } else if exponent >= 127 {
        let biased_exponent = exponent - 127;
        print_positive_exponent(real_mantissa, biased_exponent);
    }
}

fn print_denorm(real_mantissa: u32) {
    println!("  print_denorm({real_mantissa})");
    let count = count_bytes(real_mantissa);
    println!("  count_bytes {count}");
    let aligned_mantissa = right_align(real_mantissa);
    //println!("decimal_numbers_count {decimal_numbers_count}");

    print_aligned(aligned_mantissa, count);
}

fn print_positive_exponent(real_mantissa: u32, exponent: u32) {
    println!("  print_positive_exponent({real_mantissa}, {exponent})");
    let count = count_bytes(real_mantissa);
    println!("  count_bytes {count}");
    let aligned_mantissa = right_align(real_mantissa);
    //println!("decimal_numbers_count {decimal_numbers_count}");

    print_aligned(aligned_mantissa, count - exponent - 1);
}

fn print_negative_exponent(real_mantissa: u32, exponent: u32) {
    println!("  print_negative_exponent({real_mantissa}, {exponent})");
    let count = count_bytes(real_mantissa);
    println!("  count_bytes {count}");
    let aligned_mantissa = right_align(real_mantissa);
    //println!("decimal_numbers_count {decimal_numbers_count}");

    print_aligned(aligned_mantissa, count + exponent - 1);
}

fn print_aligned(aligned_mantissa: u32, decimal_numbers_count: u32) {
    println!("  print_aligned({aligned_mantissa}, {decimal_numbers_count})");
    let int_number = aligned_mantissa >> decimal_numbers_count;
    println!("  int {int_number}");

    let mut and_for_decimal_numbers: u32 = 1;
    and_for_decimal_numbers <<= decimal_numbers_count;
    and_for_decimal_numbers -= 1;
    let mut decimal_numbers = aligned_mantissa & and_for_decimal_numbers;

    let mut actual_count = min(decimal_numbers_count, 12);
    if decimal_numbers_count > 12 {
        decimal_numbers >>= decimal_numbers_count - 12;
    }
    let mut actual_number = 0u32;
    let mut actual_divider = 0;
    let mut five_multiplier = 1;

    loop {
        if actual_count == 0 {
            break;
        }

        let mut loop_count = actual_count;
        let mut decimal_numbers_to_print = decimal_numbers;

        if actual_count > N {
            decimal_numbers_to_print = decimal_numbers >> (actual_count - N);
            loop_count = N;
        }
        let decimals = get_decimals(decimal_numbers_to_print, loop_count, five_multiplier);
        let mut actual_decimals = decimals;
        for _ in 0..actual_divider {
            actual_decimals /= 10;
        }

        let mut int_decimals = actual_decimals;
        for _ in 0..actual_divider {
            int_decimals *= 10;
        }

        let mut reminder = decimals - int_decimals;
        println!("  reminder {}", reminder);

        actual_number += actual_decimals;
        let mut decimal_numbers_and = 1 << loop_count;
        decimal_numbers_and -= 1;
        decimal_numbers &= decimal_numbers_and;

        if actual_divider >= N {
            for _ in 0..(actual_divider - N) {
                reminder /= 10;
            }
            println!("  weighted reminder {}", reminder);
            //decimal_numbers += reminder;
        } else {
            println!("  actual_divider {}", actual_divider);
        }

        actual_count -= loop_count;
        actual_divider += loop_count;
        five_multiplier += loop_count;
        /*}else {
            let mut decimals = get_decimals(decimal_numbers, actual_count, five_multiplier);
            for i in 0..actual_divider {
                decimals = decimals / 10;
            }
            actual_number += decimals;
            break;
        }
         */
    }
    println!("  dec {actual_number}");
}

fn print_decimals(n: u32, count: u32) {
    println!("  print_decimals({n}, {count})");
    //print!(".");

    let mut result = 0u32;
    let mut five_multiplier = 5u32;
    let mut actual_count = count;

    loop {
        if actual_count == 0 {
            break;
        }
        if get_nth_bit(n, actual_count) {
            let ten_multiplier = ten_exp_n_minus_one(actual_count);
            let multiplier = five_multiplier * ten_multiplier;
            println!("  multiplier {multiplier}");
            result += multiplier;
        }
        five_multiplier *= 5;
        actual_count -= 1;
    }
    println!("  dec {result}");
}

fn get_decimals(n: u32, count: u32, five_multiplier: u32) -> u32 {
    print!("  get_decimals({n}, {count}, {five_multiplier})");
    let mut result = 0u32;
    let mut actual_count = count;

    let mut current_five_multiplier = five_multiplier;

    loop {
        if actual_count == 0 {
            break;
        }
        if get_nth_bit(n, actual_count) {
            let mut multiplier = ten_exp_n_minus_one(actual_count);
            multiplier *= u32::pow(5, current_five_multiplier);
            result += multiplier;
        }
        current_five_multiplier += 1;
        actual_count -= 1;
    }
    println!(" = {result}");
    result
}

fn ten_exp_n_minus_one(n: u32) -> u32 {
    let mut result = 1;
    let mut actual_n = n;
    loop {
        actual_n -= 1;
        if actual_n == 0 {
            break;
        }
        result *= 10;
    }
    result
}

fn get_nth_bit(n: u32, pos: u32) -> bool {
    let v = 1u32 << (pos - 1);
    v & n != 0
}

fn right_align(n: u32) -> u32 {
    let size = count_bytes(n);
    let to_shits = 24 - size;
    n >> to_shits
}

fn count_bytes(n: u32) -> u32 {
    let mut count = 0;
    let mut actual = n << 8;
    while actual != 0 {
        actual <<= 1;
        count += 1;
    }
    count
}
