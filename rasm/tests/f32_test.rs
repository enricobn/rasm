use log::debug;
use rand::Rng;

#[cfg(test)]
#[test]
fn test_f32() {
    let _ = env_logger::builder().is_test(true).try_init();

    test_print_f32(10.0);
    test_print_f32(15.0);

    test_print_f32(-7.375);
    test_print_f32(-7.4);
    test_print_f32(-1.375);

    test_print_f32(-1.3754321);
    test_print_f32(-1.1234567);
    test_print_f32(-0.375);
    test_print_f32(-0.4);
    test_print_f32(-0.3754321);

    test_print_f32(0.0);
    test_print_f32(10.0);
    test_print_f32(20.1);
    test_print_f32(20.001);
    test_print_f32(-0.0);
    test_print_f32(-10.0);
    test_print_f32(-20.1);
    test_print_f32(-20.001);

    test_print_f32(f32::INFINITY);
    test_print_f32(f32::NEG_INFINITY);
    test_print_f32(f32::NAN);

    let mut rng = rand::thread_rng();

    for _ in 0..1000 {
        let n = rng.gen::<f64>() * 1_000f64 - 500f64;
        test_print_f32(n as f32);
    }
}

fn test_print_f32(n: f32) {
    debug!("{n}");
    let expected = format!("expected {n}");
    debug!("{:width$}got ", expected, width = 25);

    let to_bits = n.to_bits();
    debug!("  bits {to_bits}");

    let mut result = String::new();

    let sign = to_bits & 0x80000000;

    if sign != 0 {
        debug!("  sign -");
        result.push('-');
    } else {
        debug!("  sign +");
    }

    let mut mantissa = to_bits & 0x7FFFFF;
    let mut exponent = to_bits & 0x7F800000;

    exponent >>= 23;

    result += &if exponent == 255 {
        if mantissa == 0 {
            debug!("  infinity");
            "inf".to_owned()
        } else {
            debug!("  NaN");
            "NaN".to_owned()
        }
    } else {
        debug!("  exponent {exponent}");
        if exponent == 0 {
            print_aligned(mantissa, 1)
        } else {
            mantissa |= 0x800000;
            if exponent >= 127 {
                print_aligned(mantissa, exponent - 126)
            } else {
                print_aligned(mantissa >> (126 - exponent), 0)
            }
        }
    };

    if n.is_nan() {
        assert!(result.parse::<f32>().unwrap().is_nan())
    } else if n.is_infinite() {
        assert!(result.parse::<f32>().unwrap().is_infinite())
    } else {
        let v = result.parse::<f32>().unwrap();
        assert!((n - v).abs() < 0.00001);
    }
}

fn print_aligned(aligned_mantissa: u32, int_numbers_count: u32) -> String {
    debug!("  print_aligned({aligned_mantissa}, {int_numbers_count})");
    let int_number = aligned_mantissa >> (24 - int_numbers_count);
    debug!("  int {int_number}");

    let mut and_for_decimal_numbers: u32 = 1;
    and_for_decimal_numbers <<= 24 - int_numbers_count;
    and_for_decimal_numbers -= 1;
    let decimal_numbers = aligned_mantissa & and_for_decimal_numbers;

    let decimals = print_decimals(decimal_numbers, 24 - int_numbers_count);

    format!("{int_number}{decimals}")
}

fn print_decimals(n: u32, count: u32) -> String {
    debug!("  print_decimals({n}, {count})");

    let mut five_multiplier = 5u32;
    let mut temp_result = 0u32;
    let mut inner_count = 0;
    let mut actual_count = count;
    let mut result = 0u32;

    loop {
        debug!("  temp_result {temp_result} five_multiplier {five_multiplier}");
        if actual_count == 0 {
            break;
        }

        // when we reach the 10th binary number, we cannot continue multiplying by 10 the result
        // because we overflow, so we use another approach, we divide by 10 the 5 multiplier

        if inner_count == 9 {
            debug!("  temp_result {temp_result}");
            result = temp_result;
            temp_result = 0;
        }

        if inner_count >= 9 {
            five_multiplier /= 10;
        } else {
            temp_result *= 10;
        }

        if get_nth_bit(n, actual_count) {
            debug!("  found");
            temp_result += five_multiplier;
        }

        five_multiplier *= 5;
        actual_count -= 1;
        inner_count += 1;
    }

    debug!("  result {result} temp_result {temp_result} inner_count {inner_count}");

    result += temp_result;

    let padded = format!("{:0width$}", result, width = 9);

    debug!("  dec {padded}");

    format!(".{padded}")
}

fn get_nth_bit(n: u32, pos: u32) -> bool {
    let v = 1u32 << (pos - 1);
    v & n != 0
}
