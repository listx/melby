use std::collections::HashMap;
use std::error::Error;
use std::path::PathBuf;
use clap::{Parser, Subcommand};

pub mod grpc_generated;
use grpc_generated::melby_client::view_client::ViewClient;
use grpc_generated::melby_client::ViewRequest;

#[derive(Parser)]
#[command(version, about, long_about = None)]
struct Cli {
    /// Optional name to operate on
    name: Option<String>,

    /// Sets a custom config file
    #[arg(short, long, value_name = "FILE")]
    config: Option<PathBuf>,

    /// Turn debugging information on
    #[arg(short, long, action = clap::ArgAction::Count)]
    debug: u8,

    #[command(subcommand)]
    command: Option<Commands>,
}

#[derive(Subcommand)]
enum Commands {
    /// does testing things
    Test {
        /// lists test values
        #[arg(short, long)]
        list: bool,
    },
}

#[tokio::main]
async fn main() -> Result<(), Box<dyn Error>> {
    let cli = Cli::parse();

    // You can check the value provided by positional arguments, or option arguments
    if let Some(name) = cli.name.as_deref() {
        println!("Value for name: {name}");
    }

    if let Some(config_path) = cli.config.as_deref() {
        println!("Value for config: {}", config_path.display());
    }

    // You can see how many times a particular flag or argument occurred
    // Note, only flags can have multiple occurrences
    match cli.debug {
        0 => println!("Debug mode is off"),
        1 => println!("Debug mode is kind of on"),
        2 => println!("Debug mode is on"),
        _ => println!("Don't be crazy"),
    }

    // You can check for the existence of subcommands, and if found use their
    // matches just as you would the top level cmd
    match &cli.command {
        Some(Commands::Test { list }) => {
            if *list {
                println!("Printing testing lists...");
            } else {
                println!("Not printing testing lists...");
            }
        }
        None => {}
    }

    // FIXME: allow setting the URL as an option
    let url = "http://[::1]:50051";
    let mut client = ViewClient::connect(url).await?;

    // FIXME: allow setting these values as options. ENV vars should be slurped
    // up from the environment. Instead of slurping up everything, only slurp up
    // an "allowlist" of known ENV vars we want to feed into the server. This
    // allowlist could be defined in settings.yml or even as an additive list of
    // flags. We could do the additive list of flags first and then move to
    // settings.yml in the future if there are just too many flags.
    let mut env_vars: HashMap<String, String> = HashMap::new();
    let req = ViewRequest {
        config_path: "".to_string(),
        config: "".to_string(),
        env_vars: env_vars,
        shell_pid: "".to_string(),
    };

    let request = tonic::Request::new(req);
    let response = client.get_view(request).await?;

    println!("Response: {:?}", response.get_ref().view);

    Ok(())
}
