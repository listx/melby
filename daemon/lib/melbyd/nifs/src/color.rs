use csscolorparser::Color;

#[derive(Default, Debug, PartialEq, Eq)]
#[cfg_attr(feature = "elixir_support", derive(rustler::NifTuple))]
pub struct Color24BitRust {
    pub red: u8,
    pub green: u8,
    pub blue: u8,
    pub alpha: u8,
}

#[cfg_attr(feature = "elixir_support", rustler::nif)]
pub fn parse_color(color_str: &str) -> Color24BitRust {
    let vals = match color_str.parse::<Color>() {
        Ok(color) => color.to_rgba8(),
        Err(e) => {
          eprintln!("{:?} {:?}", e, color_str);
          [127, 127, 127, 255]
        }
    };

    Color24BitRust {
        red: vals[0],
        green: vals[1],
        blue: vals[2],
        alpha: vals[3],
    }
}

fn _parse_color(color_str: &str) -> [u8; 4] {
    match color_str.parse::<Color>() {
        Ok(color) => color.to_rgba8(),
        Err(e) => {
          eprintln!("{:?} {:?}", e, color_str);
          [127, 127, 127, 255]
        }
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_parse_color() {
        assert_eq!(_parse_color("#ff0000"), [255, 0, 0, 255]);
        // Invalid strings get parsed as grey.
        assert_eq!(_parse_color(""), [127, 127, 127, 255]);
        assert_eq!(_parse_color("?"), [127, 127, 127, 255]);
        assert_eq!(_parse_color("hello world"), [127, 127, 127, 255]);
    }
}
