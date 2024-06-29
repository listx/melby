pub mod color;
pub mod path_shorten;

rustler::init!(
    "Elixir.Melbyd.Nifs",
    [
        path_shorten::path_shorten,
        color::parse_color,
    ]
);
