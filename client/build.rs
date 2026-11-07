use std::error::Error;

fn main() -> Result<(), Box<dyn Error>> {
    tonic_prost_build::configure()
        .build_server(false)
        .out_dir("src/grpc_generated") // you can change the generated code's location
        .compile_protos(
            &["proto/melby_client.proto"],
            &["proto"], // specify the root location to search proto dependencies
        )?;

    Ok(())
}
