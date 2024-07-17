use std::collections::HashMap;
use std::error::Error;
use std::path::PathBuf;
use clap::{Parser, Subcommand};
pub mod grpc_generated;
use grpc_generated::melby_client::view_client::ViewClient;
use grpc_generated::melby_client::ViewRequest;
#[derive(Parser)]
#[command(version, about, long_about = None)]
/// Global options (applies to all subcommands).
struct Cli {
    /// Config file path.
    #[arg(short, long,
          value_name = "LUA_CONFIG_FILE", default_value = "~/.melby/melby.lua")]
    config: PathBuf,

    /// Inline Lua config if you want to test out small(er) snippets of code
    /// directly without having to write a LUA_CONFIG_FILE on disk somewhere.
    /// Currently ignored by the daemon.
    #[arg(short = 'r', long,
          value_name = "RAW_INLINE_LUA_CONFIG", default_value = "")]
    config_raw: String,

    /// Port of melbyd (presumed to be running on localhost).
    #[arg(short, long, default_value_t = 50051)]
    melbyd_port: u16,

    /// Subcommands.
    #[command(subcommand)]
    command: Option<Commands>,
}
/// Subcommands and their options.
#[derive(Subcommand)]
enum Commands {
    View {
        #[arg(short, long, default_value_t = 0)]
        shell_pid: u32,
    },
}
#[tokio::main]
async fn main() -> Result<(), Box<dyn Error>> {
    let cli = Cli::parse();

    match &cli.command {
        Some(Commands::View { shell_pid }) => {
            view(&cli, *shell_pid).await?;
        }
        None => {
            ()
        }
    }

    Ok(())
}
async fn view(cli: &Cli, shell_pid: u32) -> Result<(), Box<dyn Error>> {
    let url = format!("http://127.0.0.1:{0}", cli.melbyd_port);
    let mut client = ViewClient::connect(url).await?;

    // FIXME: Instead of slurping up everything, only slurp up
    // an "allowlist" of known ENV vars we want to feed into the daemon. This
    // allowlist could be defined in settings.yml or even as an additive list of
    // flags. We could do the additive list of flags first and then move to
    // settings.yml in the future if there are just too many flags.
    let mut env_vars: HashMap<String, String> = HashMap::new();
    for (k, v) in std::env::vars() {
        env_vars.insert(k, v);
    }

    let req = ViewRequest {
        config_path: cli.config.display().to_string(),
        config: cli.config_raw.to_string(),
        env_vars: env_vars,
        shell_pid: shell_pid.to_string(),
    };

    let request = tonic::Request::new(req);
    let response = client.get_view(request).await?;

    println!("{}", response.get_ref().view);

    Ok(())
}
