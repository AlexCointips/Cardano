{ system, compiler, flags, pkgs, hsPkgs, pkgconfPkgs, ... }:
  {
    flags = { development = false; };
    package = {
      specVersion = "1.10";
      identifier = { name = "small-steps"; version = "0.1.0.0"; };
      license = "BSD-3-Clause";
      copyright = "";
      maintainer = "formal.methods@iohk.io";
      author = "IOHK Formal Methods Team";
      homepage = "https://github.com/input-output-hk/cardano-chain";
      url = "";
      synopsis = "Small step semantics";
      description = "";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs.base)
          (hsPkgs.containers)
          (hsPkgs.cryptonite)
          (hsPkgs.free)
          (hsPkgs.hedgehog)
          (hsPkgs.tasty-hunit)
          (hsPkgs.lens)
          (hsPkgs.mtl)
          (hsPkgs.transformers)
          ];
        };
      tests = {
        "doctests" = {
          depends = [
            (hsPkgs.base)
            (hsPkgs.containers)
            (hsPkgs.data-default)
            (hsPkgs.free)
            (hsPkgs.hedgehog)
            (hsPkgs.tasty-hunit)
            (hsPkgs.lens)
            (hsPkgs.mtl)
            (hsPkgs.sequence)
            (hsPkgs.transformers)
            (hsPkgs.doctest)
            (hsPkgs.small-steps)
            ];
          build-tools = [
            (hsPkgs.buildPackages.doctest-discover or (pkgs.buildPackages.doctest-discover))
            ];
          };
        "examples" = {
          depends = [
            (hsPkgs.base)
            (hsPkgs.containers)
            (hsPkgs.hedgehog)
            (hsPkgs.tasty)
            (hsPkgs.tasty-hedgehog)
            (hsPkgs.tasty-expected-failure)
            (hsPkgs.small-steps)
            ];
          };
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchgit {
      url = "https://github.com/input-output-hk/cardano-ledger-specs";
      rev = "40b5c148f16672d1d97492419c757c01ba0acae8";
      sha256 = "0i4lf4yj9p2vncasn44mrdbbyblsrnirnlczpywaz7sxqwlinf20";
      });
    postUnpack = "sourceRoot+=/byron/semantics/executable-spec; echo source root reset to \$sourceRoot";
    }