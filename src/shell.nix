{ pkgs ? import <nixpkgs> {} }:
let
  # Add precompiled library to rustc search path
  rustflags = with pkgs.lib.lists; foldr (x: acc: ''-L ${x}/lib '' + acc) "" [
    # add libraries here (e.g. pkgs.libvmi)
  ];
in
pkgs.mkShell rec {
  nativeBuildInputs = [ pkgs.pkg-config ];
  buildInputs = with pkgs; [
    clang
    rustup
    llvmPackages_latest.bintools
  ];
  RUSTC_VERSION = pkgs.lib.readFile ./rust-toolchain;
  # https://github.com/rust-lang/rust-bindgen#environment-variables
  LIBCLANG_PATH = pkgs.lib.makeLibraryPath [ pkgs.llvmPackages_latest.libclang.lib ];
  shellHook = ''
    export PATH=$PATH:''${CARGO_HOME:-~/.cargo}/bin
    export PATH=$PATH:''${RUSTUP_HOME:-~/.rustup}/toolchains/$RUSTC_VERSION-x86_64-unknown-linux-gnu/bin/
    export RUSTFLAGS="-A dead_code ${rustflags}"
  '';
  PKG_CONFIG_PATH="${pkgs.openssl.dev}/lib/pkgconfig:${pkgs.alsa-lib.dev}/lib/pkgconfig:${pkgs.udev.dev}/lib/pkgconfig";
  # Add glibc, clang, glib and other headers to bindgen search path
  BINDGEN_EXTRA_CLANG_ARGS =
  # Includes with normal include path
  (builtins.map (a: ''-I"${a}/include"'') [
    # add dev libraries here (e.g. pkgs.libvmi.dev)
    pkgs.glibc.dev
  ])
  # Includes with special directory paths
  ++ [
    ''-I"${pkgs.llvmPackages_latest.libclang.lib}/lib/clang/${pkgs.llvmPackages_latest.libclang.version}/include"''
    ''-I"${pkgs.glib.dev}/include/glib-2.0"''
    ''-I${pkgs.glib.out}/lib/glib-2.0/include/''
  ];
}
