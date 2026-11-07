use crate::render::{rgb_codes, sgr};

#[cfg_attr(feature = "elixir_support", rustler::nif)]
pub fn colorize_git_sha(sha: &str, sha_length: u32, pad_left: u32, pad_right: u32) -> String {
    _colorize_git_sha(sha, sha_length, pad_left, pad_right)
}

fn _colorize_git_sha(sha: &str, sha_length: u32, pad_left: u32, pad_right: u32) -> String {
    let mut parts: Vec<(Code, String)> = vec![(Code::Bold, String::new())];
    parts.extend(add_padding(
        pad_left,
        pad_right,
        colorize_git_sha_24_bit(sha, sha_length),
    ));
    parts.push((Code::Reset, String::new()));
    render_colorized(&parts)
}

#[derive(Clone, Copy, Debug, PartialEq)]
enum Code {
    Bold,
    Reset,
    Background((u8, u8, u8)),
}

fn colorize_git_sha_24_bit(sha: &str, sha_length: u32) -> Vec<(Code, String)> {
    let bytes = hex_decode(sha);
    // XOR the first and second halves of the input bytes (20 bytes) to generate
    // 10 more bytes. Now we have 30 bytes in total in bytes_final.
    let bytes1 = &bytes[..bytes.len().min(10)];
    let bytes2 = &bytes[bytes.len().saturating_sub(10)..];
    let mut bytes_final: Vec<u8> = bytes1.iter().zip(bytes2).map(|(a, b)| a ^ b).collect();
    bytes_final.extend(bytes.iter().rev());
    // Using the 30 bytes in bytes_final, generate 10 colors (of 3 bytes, or 24
    // bits, each).
    let colors: Vec<(u8, u8, u8)> = bytes_final
        .chunks_exact(3)
        .map(|c| (c[0], c[1], c[2]))
        .collect();

    // Split the (shortened) hex string into groups of 4 hex chars each.
    let hex_chars: Vec<char> = sha.chars().take(sha_length as usize).collect();
    let groups: Vec<String> = hex_chars.chunks(4).map(|c| c.iter().collect()).collect();

    // Pair up the colors and groups, starting from the end of both lists. So
    // the last group always gets the last color.
    let n = colors.len().min(groups.len());
    (0..n)
        .rev()
        .map(|k| {
            (
                Code::Background(colors[colors.len() - 1 - k]),
                groups[groups.len() - 1 - k].clone(),
            )
        })
        .collect()
}

// Decode a string of hex digits into bytes. Like Haskell's base16-bytestring
// (which we used before), we give up and return no bytes at all if the input
// is not entirely made of pairs of hex digits.
fn hex_decode(s: &str) -> Vec<u8> {
    let digits: Option<Vec<u8>> = s
        .chars()
        .map(|c| c.to_digit(16).map(|d| d as u8))
        .collect();
    match digits {
        Some(d) if d.len() % 2 == 0 => d.chunks(2).map(|p| (p[0] << 4) | p[1]).collect(),
        _ => Vec::new(),
    }
}

fn add_padding(pad_left: u32, pad_right: u32, mut colorized: Vec<(Code, String)>) -> Vec<(Code, String)> {
    if let Some(first) = colorized.first_mut() {
        first.1 = " ".repeat(pad_left as usize) + &first.1;
    }
    if let Some(last) = colorized.last_mut() {
        last.1.push_str(&" ".repeat(pad_right as usize));
    }
    colorized
}

fn render_colorized(parts: &[(Code, String)]) -> String {
    parts
        .iter()
        .map(|(code, text)| match *code {
            Code::Bold => sgr(&[1]) + text,
            Code::Reset => sgr(&[0]) + text,
            Code::Background((r, g, b)) => {
                let (fr, fg, fb) = get_contrasting_color(r, g, b);
                sgr(&rgb_codes(r, g, b, true)) + &sgr(&rgb_codes(fr, fg, fb, false)) + text
            }
        })
        .collect()
}

// Choose white text for dark backgrounds, and black text for light ones.
fn get_contrasting_color(r: u8, g: u8, b: u8) -> (u8, u8, u8) {
    if get_perceived_lightness(r, g, b) < 50.0 {
        (255, 255, 255)
    } else {
        (0, 0, 0)
    }
}

// The CIE L* lightness (0 to 100) of an sRGB color, relative to the D65 white
// point.
fn get_perceived_lightness(r: u8, g: u8, b: u8) -> f64 {
    let [yr, yg, yb] = srgb_luminance_coefficients();
    let y = yr * linearize(r) + yg * linearize(g) + yb * linearize(b);
    if (6.0f64 / 29.0).powi(3) < y {
        116.0 * y.powf(1.0 / 3.0) - 16.0
    } else {
        (29.0f64 / 3.0).powi(3) * y
    }
}

// Undo the sRGB transfer function (gamma).
fn linearize(c: u8) -> f64 {
    let c = c as f64 / 255.0;
    if c == 1.0 {
        1.0
    } else if c <= 0.04045 {
        c / 12.92
    } else {
        ((c + 0.055) / 1.055).powf(2.4)
    }
}

// The Y row of the matrix that converts linear sRGB into CIE XYZ. We derive it
// from the chromaticities of the sRGB primaries and of the D65 white point (the
// same way that Haskell's colour library does), instead of hardcoding rounded
// coefficients.
fn srgb_luminance_coefficients() -> [f64; 3] {
    let primaries = [(0.64, 0.33), (0.30, 0.60), (0.15, 0.06)];
    let (wx, wy) = (0.31271, 0.32902);
    let xyz = |(x, y): (f64, f64)| [x / y, 1.0, (1.0 - x - y) / y];
    let p: Vec<[f64; 3]> = primaries.iter().map(|&c| xyz(c)).collect();
    let w = xyz((wx, wy));
    // Solve P * s = w for s, where the columns of P are the primaries.
    let m = [
        [p[0][0], p[1][0], p[2][0]],
        [p[0][1], p[1][1], p[2][1]],
        [p[0][2], p[1][2], p[2][2]],
    ];
    let det3 = |m: [[f64; 3]; 3]| {
        m[0][0] * (m[1][1] * m[2][2] - m[1][2] * m[2][1])
            - m[0][1] * (m[1][0] * m[2][2] - m[1][2] * m[2][0])
            + m[0][2] * (m[1][0] * m[2][1] - m[1][1] * m[2][0])
    };
    let d = det3(m);
    let mut s = [0.0; 3];
    for (i, si) in s.iter_mut().enumerate() {
        let mut mi = m;
        for row in 0..3 {
            mi[row][i] = w[row];
        }
        *si = det3(mi) / d;
    }
    // The Y row of the final matrix is just the scaled Y components of the
    // primaries, which are all 1.
    [s[0], s[1], s[2]]
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_hex_decode() {
        assert_eq!(hex_decode("00ff10"), vec![0, 255, 16]);
        assert_eq!(hex_decode("ABcd"), vec![0xab, 0xcd]);
        assert_eq!(hex_decode("abc"), Vec::<u8>::new());
        assert_eq!(hex_decode("zz"), Vec::<u8>::new());
    }

    #[test]
    fn test_luminance_coefficients() {
        let [r, g, b] = srgb_luminance_coefficients();
        assert!((r - 0.2126).abs() < 1e-3);
        assert!((g - 0.7152).abs() < 1e-3);
        assert!((b - 0.0722).abs() < 1e-3);
        assert!((r + g + b - 1.0).abs() < 1e-9);
    }

    #[test]
    fn test_lightness() {
        assert!((get_perceived_lightness(255, 255, 255) - 100.0).abs() < 1e-9);
        assert_eq!(get_perceived_lightness(0, 0, 0), 0.0);
        assert_eq!(get_contrasting_color(0, 0, 0), (255, 255, 255));
        assert_eq!(get_contrasting_color(255, 255, 0), (0, 0, 0));
    }

    #[test]
    fn test_colorize_empty() {
        // Not a hex string, so there are no colors to use.
        assert_eq!(_colorize_git_sha("xyz", 3, 1, 1), "\x1b[1m\x1b[0m");
    }

    #[test]
    fn test_colorize_groups() {
        let sha = "3b83e1cd0123456789abcdef0123456789abcdef";
        let out = _colorize_git_sha(sha, 8, 1, 1);
        // Two groups of 4 hex chars, with 1 space of padding on each side.
        assert!(out.starts_with("\x1b[1m\x1b[48;2;"));
        assert!(out.contains("m 3b83\x1b[48;2;"));
        assert!(out.ends_with("me1cd \x1b[0m"));
    }
}
