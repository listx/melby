#[derive(Clone, Debug, PartialEq)]
#[cfg_attr(feature = "elixir_support", derive(rustler::NifTaggedEnum))]
pub enum Color {
    // Red, green, and blue channels. Only the lower 8 bits of each channel are
    // used.
    Rgb((u32, u32, u32)),
    // One of the well-known "256" colors as used in ANSI escape codes. The
    // first 16 colors (0 through 15) are the traditional 16 colors used before
    // 256 colors became a thing. The colors 232-255 represent a grayscale
    // gradient (dark to light), and finally the middle 216 colors (16-231)
    // represent additional colors in 6 squares, each with 36 colors. See the
    // table here: https://en.wikipedia.org/wiki/ANSI_escape_code
    Palette(u32),
}

#[derive(Clone, Copy, Debug, PartialEq)]
#[cfg_attr(feature = "elixir_support", derive(rustler::NifUnitEnum))]
pub enum TextStyle {
    Bold,
    Italic,
    Underline,
    UnderlineDouble,
    Blink,
    BlinkRapid,
}

#[derive(Clone, Debug, Default, PartialEq)]
#[cfg_attr(feature = "elixir_support", derive(rustler::NifMap))]
pub struct Widget {
    pub str: String,
    pub fg: Option<Color>,
    pub bg: Option<Color>,
    pub styles: Vec<TextStyle>,
    pub drop_delim_left: bool,
    pub drop_delim_right: bool,
}

#[derive(Clone, Copy, Debug, PartialEq)]
#[cfg_attr(feature = "elixir_support", derive(rustler::NifUnitEnum))]
pub enum RenderFormat {
    Unspecified,
    UnixTerminal,
}

#[cfg_attr(feature = "elixir_support", rustler::nif)]
pub fn render_widgets(widgets: Vec<Widget>, delimiter: Widget, format: RenderFormat) -> String {
    _render_widgets(&widgets, &delimiter, format)
}

fn _render_widgets(widgets: &[Widget], delimiter: &Widget, format: RenderFormat) -> String {
    inject_delimiters(widgets, delimiter)
        .into_iter()
        .map(|w| render_widget(w, format))
        .collect()
}

fn inject_delimiters<'a>(widgets: &'a [Widget], delimiter: &'a Widget) -> Vec<&'a Widget> {
    let mut out = Vec::new();
    for (i, w) in widgets.iter().enumerate() {
        out.push(w);
        if let Some(next) = widgets.get(i + 1) {
            if !(w.drop_delim_right || next.drop_delim_left) {
                out.push(delimiter);
            }
        }
    }
    out
}

fn render_widget(widget: &Widget, format: RenderFormat) -> String {
    match format {
        RenderFormat::Unspecified => "RENDER_FORMAT_UNSPECIFIED is unimplemented".to_string(),
        RenderFormat::UnixTerminal => {
            format!("{}{}{}", sgr(&text_prop_codes(widget)), widget.str, sgr(&[0]))
        }
    }
}

#[derive(Clone, Copy)]
enum Layer {
    Foreground,
    Background,
}

fn text_prop_codes(widget: &Widget) -> Vec<u8> {
    let mut codes = Vec::new();
    if let Some(c) = &widget.fg {
        codes.extend(color_codes(c, Layer::Foreground));
    }
    if let Some(c) = &widget.bg {
        codes.extend(color_codes(c, Layer::Background));
    }
    codes.extend(widget.styles.iter().map(|s| style_code(*s)));
    codes
}

pub(crate) fn rgb_codes(r: u8, g: u8, b: u8, background: bool) -> Vec<u8> {
    vec![if background { 48 } else { 38 }, 2, r, g, b]
}

fn color_codes(color: &Color, layer: Layer) -> Vec<u8> {
    let background = matches!(layer, Layer::Background);
    match *color {
        Color::Rgb((r, g, b)) => rgb_codes(r as u8, g as u8, b as u8, background),
        Color::Palette(n) => vec![if background { 48 } else { 38 }, 5, n as u8],
    }
}

fn style_code(style: TextStyle) -> u8 {
    match style {
        TextStyle::Bold => 1,
        TextStyle::Italic => 3,
        TextStyle::Underline => 4,
        TextStyle::UnderlineDouble => 21,
        TextStyle::Blink => 5,
        TextStyle::BlinkRapid => 6,
    }
}

// Build an SGR ("Select Graphic Rendition") escape sequence out of the given
// parameters. An empty list of parameters still results in a (valid) escape
// sequence, "\x1b[m".
pub(crate) fn sgr(codes: &[u8]) -> String {
    let params: Vec<String> = codes.iter().map(|c| c.to_string()).collect();
    format!("\x1b[{}m", params.join(";"))
}

#[cfg(test)]
mod test {
    use super::*;

    fn w(s: &str) -> Widget {
        Widget {
            str: s.to_string(),
            ..Default::default()
        }
    }

    #[test]
    fn test_render_plain() {
        assert_eq!(
            _render_widgets(&[w("a"), w("b")], &w(" "), RenderFormat::UnixTerminal),
            "\x1b[ma\x1b[0m\x1b[m \x1b[0m\x1b[mb\x1b[0m"
        );
    }

    #[test]
    fn test_render_colors_and_styles() {
        let widget = Widget {
            str: " x ".to_string(),
            fg: Some(Color::Rgb((0, 0, 0))),
            bg: Some(Color::Rgb((255, 215, 0))),
            styles: vec![TextStyle::Bold],
            ..Default::default()
        };
        assert_eq!(
            _render_widgets(&[widget], &w(" "), RenderFormat::UnixTerminal),
            "\x1b[38;2;0;0;0;48;2;255;215;0;1m x \x1b[0m"
        );
        let widget = Widget {
            str: "y".to_string(),
            fg: Some(Color::Palette(196)),
            bg: Some(Color::Rgb((256 + 1, 2, 3))),
            styles: vec![TextStyle::Italic, TextStyle::UnderlineDouble],
            ..Default::default()
        };
        assert_eq!(
            _render_widgets(&[widget], &w(" "), RenderFormat::UnixTerminal),
            "\x1b[38;5;196;48;2;1;2;3;3;21my\x1b[0m"
        );
    }

    #[test]
    fn test_inject_delimiters() {
        let rendered = |ws: Vec<Widget>| {
            inject_delimiters(&ws, &w("|"))
                .iter()
                .map(|w| w.str.clone())
                .collect::<String>()
        };
        assert_eq!(rendered(vec![]), "");
        assert_eq!(rendered(vec![w("a")]), "a");
        assert_eq!(rendered(vec![w("a"), w("b"), w("c")]), "a|b|c");
        let mut b = w("b");
        b.drop_delim_left = true;
        assert_eq!(rendered(vec![w("a"), b.clone(), w("c")]), "ab|c");
        b.drop_delim_right = true;
        assert_eq!(rendered(vec![w("a"), b, w("c")]), "abc");
    }

    #[test]
    fn test_render_unspecified_format() {
        assert_eq!(
            _render_widgets(&[w("a")], &w(" "), RenderFormat::Unspecified),
            "RENDER_FORMAT_UNSPECIFIED is unimplemented"
        );
    }
}
