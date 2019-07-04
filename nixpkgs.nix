let
  fetchNixpkgs = { rev, sha256, outputSha256 ? null, system ? builtins.currentSystem }:
    builtins.fetchTarball {
      url = "https://github.com/NixOS/nixpkgs/archive/${rev}.tar.gz";
      sha256 = outputSha256;
    };

in

fetchNixpkgs {
  rev          = "971b731fc18c86569211a460ef62e1d8001799e9";  # 2019-05-19 master
  sha256       = "0985ddibjbawjg33k0pp2z68ccl9ndfjpw5zhp9cnbb6srdrs464";
  outputSha256 = "1b8xjrrwb8fz92bcrqvfvfg7gwn40ss12by2ka4yclcxk5yylmw0";
}
