use std::collections::HashMap;
use std::error::Error;
use std::path::PathBuf;
use std::time::Duration;
use clap::{Parser, Subcommand};
use tonic::transport::{Channel, Endpoint};

pub mod grpc_generated;
use grpc_generated::melby_client::view_client::ViewClient;
use grpc_generated::melby_client::ViewRequest;
use grpc_generated::melby_client::version_client::VersionClient;
use grpc_generated::melby_client::VersionRequest;

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
    /// Print which builds of melbyc and melbyd are running.
    Version,
}

#[tokio::main]
async fn main() {
    let cli = Cli::parse();

    let result = match &cli.command {
        Some(Commands::View { shell_pid }) => view(&cli, *shell_pid).await,
        Some(Commands::Version) => version(&cli).await,
        None => Ok(()),
    };

    if let Err(e) = result {
        eprintln!("melbyc: {}", describe(e.as_ref()));
        std::process::exit(1);
    }
}

// Describe an error along with all of its causes. Errors from tonic are often
// vague on their own (such as "transport error"), and only their causes say
// what actually went wrong (such as "Connection refused").
fn describe(e: &dyn Error) -> String {
    let mut msg = e.to_string();
    let mut cause = e.source();
    while let Some(c) = cause {
        msg.push_str(&format!(": {}", c));
        cause = c.source();
    }
    msg
}

const VIEW_TIMEOUT: Duration = Duration::from_millis(1000);

async fn connect(cli: &Cli) -> Result<Channel, Box<dyn Error>> {
    let url = format!("http://127.0.0.1:{0}", cli.melbyd_port);
    let channel = Endpoint::from_shared(url)?
        .connect_timeout(VIEW_TIMEOUT)
        .timeout(VIEW_TIMEOUT)
        .connect()
        .await
        .map_err(|e| format!("cannot reach melbyd on port {}: {}", cli.melbyd_port, describe(&e)))?;
    Ok(channel)
}

async fn view(cli: &Cli, shell_pid: u32) -> Result<(), Box<dyn Error>> {
    let channel = connect(cli).await?;
    let mut client = ViewClient::new(channel);

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

const GIT_REV: &str = match option_env!("MELBY_GIT_REV") {
    Some(rev) => rev,
    None => "unknown",
};

async fn version(cli: &Cli) -> Result<(), Box<dyn Error>> {
    let exe = std::env::current_exe()?;
    println!("melbyc {} {} {}", env!("CARGO_PKG_VERSION"), GIT_REV, exe.display());

    let channel = connect(cli).await?;
    let mut client = VersionClient::new(channel);
    let response = client.get_version(tonic::Request::new(VersionRequest {})).await?;
    let v = response.get_ref();
    println!("melbyd {} {} {}", v.version, v.git_rev, v.root_dir);

    Ok(())
}
