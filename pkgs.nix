nixpkgs: pkgs: with pkgs;
[
  servant
  servant-client
  servant-multipart
  open-union
  http-client
  http-client-tls
  http-client-overrides
  (callPackage ./nix/telegram-types.nix {})
  time
]
