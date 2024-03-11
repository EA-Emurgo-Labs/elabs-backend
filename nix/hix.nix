{ pkgs, ... }: 
  let 
    startDb = pkgs.writeScriptBin "startDb" ''
      initdb -D $PGDATA

      pg_ctl                                                  \
        -D $PGDATA                                            \
        -l $PGDATA/postgres.log                               \
        -o "-c unix_socket_directories='$PGDATA'"             \
        -o "-c listen_addresses='*'"                          \
        -o "-c log_destination='stderr'"                      \
        -o "-c logging_collector=on"                          \
        -o "-c log_directory='log'"                           \
        -o "-c log_filename='postgresql-%Y-%m-%d_%H%M%S.log'" \
        -o "-c log_min_messages=info"                         \
        -o "-c log_min_error_statement=info"                  \
        -o "-c log_connections=on"                            \
        start

      
    '';
in {
  # name = "project-name";
  compiler-nix-name = "ghc928"; # Version of GHC to use

  # Cross compilation support:
  # crossPlatforms = p: pkgs.lib.optionals pkgs.stdenv.hostPlatform.isx86_64 ([
  #   p.mingwW64
  #   p.ghcjs
  # ] ++ pkgs.lib.optionals pkgs.stdenv.hostPlatform.isLinux [
  #   p.musl64
  # ]);

  # Tools to include in the development shell
  shell = {
    tools = {
      cabal = "3.10.1.0";
      hlint = "3.6.1";
      haskell-language-server = { version = "2.0.0.0"; index-state = "2023-06-05T06:39:32Z"; };
    };

    buildInputs = with pkgs; [
      jq
      haskellPackages.cabal-fmt
      haskellPackages.fourmolu
      nixpkgs-fmt
      gnumake
      postgresql_14
      startDb
    ];



    # Start and terminate postgresql gracefully
    shellHook = ''
      mkdir -p $PWD/.db
      export PGDATA=$PWD/.db
      trap \
        "
          # Stop PostgreSQL
          pg_ctl -D $PGDATA stop
        " \
      EXIT


    '';
  };
}
