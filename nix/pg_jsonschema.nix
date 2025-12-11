{
  buildPgrxExtension,
  postgresql_18,
  fetchFromGitHub,
  cargo-pgrx,
}:

(buildPgrxExtension {
  pname = "pg_jsonschema";
  version = "0.3.3";
  postgresql = postgresql_18;
  src = fetchFromGitHub {
    owner = "supabase";
    repo = "pg_jsonschema";
    rev = "cabd2eaa015c2add2fb7d41aeb7271e7c18b2b5e";
    hash = "sha256-zTYEFkz0cpLFG+6k3ZUWwJ//184QfwTF9keh3WebPPw=";
  };
  doCheck = false; # Linker errors on macOS durnig tests
  cargoHash = "sha256-LutCrn4HRFyh+NkPNH2Zi9ko+Ickv0geaAQXYw0AzTw=";
  # 0.16.0 is available in nixpkgs and compatible with 0.16.1
  inherit cargo-pgrx;
}).overrideAttrs
  (old: {
    meta = (old.meta or { }) // {
      broken = false;
    };
  })
