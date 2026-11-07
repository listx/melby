pub mod color;
pub mod git_sha;
pub mod path_aliases;
pub mod path_shorten;
pub mod render;

// Every function marked with #[rustler::nif] gets registered automatically.
#[cfg(feature = "elixir_support")]
rustler::init!("Elixir.Melbyd.Nifs");
