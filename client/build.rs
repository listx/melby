use std::error::Error;

fn main() -> Result<(), Box<dyn Error>> {
    tonic_build::compile_protos("proto/melby_client.proto")?;
    tonic_build::configure()
    .build_server(false)
    .out_dir("src/grpc_generated")  // you can change the generated code's location
    .compile(
        &["proto/melby_client.proto"],
        &["proto"], // specify the root location to search proto dependencies
    ).unwrap();

    Ok(())
}
