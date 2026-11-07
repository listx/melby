pub mod color;
pub mod git_sha;
pub mod path_aliases;
pub mod path_shorten;
pub mod render;

#[cfg(feature = "elixir_support")]
rustler::init!(
    "Elixir.Melbyd.Nifs",
    [
        path_shorten::path_shorten,
        color::parse_color,
        render::render_widgets,
        git_sha::colorize_git_sha,
        path_aliases::parse_path_aliases,
    ]
);
