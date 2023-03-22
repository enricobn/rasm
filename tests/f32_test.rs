use log::{debug, log_enabled, Level};

#[cfg(test)]
#[test]
fn test_f32() {
    let _ = env_logger::builder().is_test(true).try_init();
    //let mut rng = rand::thread_rng();

    print_n(-7.375);
    print_n(-7.4);
    print_n(-1.375);

    print_n(-1.3754321);
    print_n(-1.1234567);
    print_n(-0.375);
    print_n(-0.4);
    print_n(-0.3754321);
    print_n(f32::INFINITY);
    print_n(f32::NEG_INFINITY);
    print_n(f32::NAN);
}

fn print_n(n: f32) {
    debug!("{}", n);
    if !log_enabled!(Level::Debug) {
        let expected = format!("expected {n}");
        print!("{:width$}got ", expected, width = 25);
    }
    let to_bits = n.to_bits();

    /*
    let bits = format!("{:b}", to_bits);
    let sign_s = bits.get(0..1).unwrap().to_string();
    let exponent_s = bits.get(1..9).unwrap().to_string();
    let mantissa_s = bits.get(9..).unwrap().to_string();
    println!("{bits}\nsign {sign_s}\nexponent {exponent_s}\nmantissa {mantissa_s}");

     */
    let mut result = String::new();

    let sign = to_bits & 0x80000000;

    if sign != 0 {
        debug!("  sign -");
        if !log_enabled!(Level::Debug) {
            print!("-");
        }
        result.push('-');
    } else {
        debug!("  sign +");
    }

    let mantissa = to_bits & 0x7FFFFF;
    let real_mantissa = mantissa | 0x800000;
    let mut exponent = to_bits & 0x7F800000;
    exponent >>= 23;

    result += &if exponent == 255 {
        if mantissa == 0 {
            if log_enabled!(Level::Debug) {
                debug!("  infinity");
            } else {
                println!("inf");
            }
            "inf".to_owned()
        } else {
            if log_enabled!(Level::Debug) {
                debug!("  NaN");
            } else {
                println!("NaN");
            }
            "NaN".to_owned()
        }
    } else {
        let count = count_bytes(real_mantissa);
        let aligned_mantissa = right_align(real_mantissa);

        if exponent == 0 {
            print_aligned(aligned_mantissa, count)
        } else {
            print_aligned(aligned_mantissa, count + 127 - exponent - 1)
        }
    };

    if n.is_nan() {
        assert!(result.parse::<f32>().unwrap().is_nan())
    } else {
        assert_eq!(n, result.parse::<f32>().unwrap());
    }
}

fn print_aligned(aligned_mantissa: u32, decimal_numbers_count: u32) -> String {
    debug!("  print_aligned({aligned_mantissa}, {decimal_numbers_count})");
    let int_number = aligned_mantissa >> decimal_numbers_count;
    if log_enabled!(Level::Debug) {
        debug!("  int {int_number}");
    } else {
        print!("{int_number}");
    }

    let mut and_for_decimal_numbers: u32 = 1;
    and_for_decimal_numbers <<= decimal_numbers_count;
    and_for_decimal_numbers -= 1;
    let decimal_numbers = aligned_mantissa & and_for_decimal_numbers;

    let decimals = print_decimals(decimal_numbers, decimal_numbers_count);

    format!("{int_number}{decimals}")
}

fn print_decimals(n: u32, count: u32) -> String {
    debug!("  print_decimals({n}, {count})");

    let mut result = 0u32;
    let mut five_multiplier = 5u32;
    let mut actual_count = count;
    let mut temp_result = 0u32;
    let mut inner_count = 0;

    loop {
        debug!("  temp_result {temp_result} five_multiplier {five_multiplier}");
        if actual_count == 0 {
            break;
        }

        // when we reach the 10th binary number, we cannot continue multiplying by 10 the result
        // because we overflow, so we use another approach, we divide by 10 the 5 multiplier

        if inner_count == 10 {
            debug!("  temp_result {temp_result}");
            result = temp_result;
            temp_result = 0;
        }

        if inner_count >= 10 {
            five_multiplier /= 10;
        } else {
            temp_result *= 10;
        }

        if get_nth_bit(n, actual_count) {
            temp_result += five_multiplier;
        }

        five_multiplier *= 5;
        actual_count -= 1;
        inner_count += 1;
    }

    debug!("  result {result} temp_result {temp_result}");

    result += temp_result;

    if log_enabled!(Level::Debug) {
        debug!("  dec {result}");
    } else {
        println!(".{result}");
    }
    format!(".{result}")
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
