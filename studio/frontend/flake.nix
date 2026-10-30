{
  inputs.miso.url = "github:dmjio/miso/efae40b82c1452891f78e79c139d6421983878a6";
  outputs = inputs:
    inputs.miso.inputs.flake-utils.lib.eachDefaultSystem (system: {
      devShells.wasm = inputs.miso.outputs.devShells.${system}.wasm;
    });
}
