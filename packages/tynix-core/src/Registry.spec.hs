{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Monad (forM, forM_)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.IO qualified as TextIO
import Driver (Analysis (..), analyzeFile)
import Indexed (validateProgramIndexedTypes)
import Kind (validateProgramKinds)
import Parser (parseProgram)
import Pretty (renderScheme)
import Syntax (Program (programAliases, programExpr))
import System.Directory (doesFileExist, getCurrentDirectory)
import System.FilePath (takeDirectory, (</>))
import Test.Hspec
import TestSupport (expectRight, source, withTempTree)

main :: IO ()
main = hspec spec

spec :: Spec
spec = describe "bundled registry" $ do
  it "parses and validates the bundled declaration files" $ do
    root <- findBundledRegistryRoot =<< getCurrentDirectory
    forM_ bundledDeclarationFiles $ \relative -> do
      input <- TextIO.readFile (root </> relative)
      program <- expectRight (parseProgram (root </> relative) input)
      programExpr program `shouldBe` Nothing
      validateProgramKinds (programAliases program) program `shouldSatisfy` isRight
      validateProgramIndexedTypes program `shouldBe` Right ()

  it "lets projects reuse bundled workspace declarations" $ do
    root <- findBundledRegistryRoot =<< getCurrentDirectory
    registry <- loadRegistry root workspaceRegistryFiles
    withTempTree
      ( registry
          <> [ ( "tynix.config.tynix",
                 source
                   [ "{",
                     "  name = \"demo\";",
                     "  sourceDir = ./src;",
                     "  entry = ./src/main.tynix;",
                     "  declarationDir = ./types;",
                     "  builtins = true;",
                     "}"
                   ]
               ),
               ( "flake.nix",
                 source
                   [ "{",
                     "  description = \"demo flake\";",
                     "  inputs = {",
                     "    nixpkgs = { url = \"github:NixOS/nixpkgs\"; };",
                     "    flake-utils = { url = \"github:numtide/flake-utils\"; };",
                     "  };",
                     "  outputs = inputs: {",
                     "    formatter = {",
                     "      x86_64-linux = 1;",
                     "      aarch64-linux = 1;",
                     "      x86_64-darwin = 1;",
                     "      aarch64-darwin = 1;",
                     "    };",
                     "    devShells = {",
                     "      x86_64-linux = { default = 1; };",
                     "      aarch64-linux = { default = 1; };",
                     "      x86_64-darwin = { default = 1; };",
                     "      aarch64-darwin = { default = 1; };",
                     "    };",
                     "  };",
                     "}"
                   ]
               ),
               ( "src/main.tynix",
                 "let cfg = import ../tynix.config.tynix; flake = import ../flake.nix; in [cfg.builtins flake.description]"
               )
             ]
      )
      ( \tmp -> do
          analysis <- analyzeFile (tmp </> "src/main.tynix") >>= expectRight
          renderedRoot analysis `shouldBe` "Tuple [ Bool String ]"
      )

  it
    "lets projects load bundled workspace declarations via declarationPacks without copying"
    $ do
      root <- findBundledRegistryRoot =<< getCurrentDirectory
      let workspacePack = root </> "registry/workspace"
      withTempTree
        [ ( "tynix.config.tynix",
            source
              [ "{",
                "  name = \"demo\";",
                "  declarationPacks = [ \"" <> Text.pack workspacePack <> "\" ];",
                "}"
              ]
          ),
          ( "flake.nix",
            source
              [ "{",
                "  description = \"demo flake\";",
                "  inputs = {",
                "    nixpkgs = { url = \"github:NixOS/nixpkgs\"; };",
                "    flake-utils = { url = \"github:numtide/flake-utils\"; };",
                "  };",
                "  outputs = inputs: {",
                "    formatter = {",
                "      x86_64-linux = 1;",
                "      aarch64-linux = 1;",
                "      x86_64-darwin = 1;",
                "      aarch64-darwin = 1;",
                "    };",
                "    devShells = {",
                "      x86_64-linux = { default = 1; };",
                "      aarch64-linux = { default = 1; };",
                "      x86_64-darwin = { default = 1; };",
                "      aarch64-darwin = { default = 1; };",
                "    };",
                "  };",
                "}"
              ]
          ),
          ( "src/main.tynix",
            "let cfg = import ../tynix.config.tynix; flake = import ../flake.nix; in [cfg.declarationPacks flake.description]"
          )
        ]
        ( \tmp -> do
            analysis <- analyzeFile (tmp </> "src/main.tynix") >>= expectRight
            renderedRoot analysis `shouldBe` "Tuple [ (List TynixProjectPath) String ]"
        )

  it "lets projects reuse bundled nixpkgs aliases inside ambient declarations" $ do
    root <- findBundledRegistryRoot =<< getCurrentDirectory
    registry <-
      loadRegistry
        root
        [ "registry/ecosystem/nixpkgs-lib.d.tynix",
          "registry/ecosystem/nixpkgs-pkgs.d.tynix"
        ]
    withTempTree
      ( registry
          <> [ ( "types.d.tynix",
                 source
                   [ "declare \"./lib.nix\" { default :: NixpkgsLib; };",
                     "declare \"./pkgs.nix\" { default :: NixpkgsPkgs; };"
                   ]
               ),
               ( "lib-surface.tynix",
                 "let lib = import ./lib.nix; in lib.strings.concatStringsSep"
               ),
               ( "pkgs-surface.tynix",
                 "let pkgs = import ./pkgs.nix; in pkgs.writeShellScriptBin"
               )
             ]
      )
      ( \tmp -> do
          libAnalysis <- analyzeFile (tmp </> "lib-surface.tynix") >>= expectRight
          pkgsAnalysis <- analyzeFile (tmp </> "pkgs-surface.tynix") >>= expectRight
          renderedRoot libAnalysis `shouldBe` "String -> List String -> String"
          let renderedPkgs = renderedRoot pkgsAnalysis
          Text.isInfixOf "String -> String ->" renderedPkgs `shouldBe` True
          Text.isInfixOf "NixpkgsDerivation" renderedPkgs `shouldBe` True
      )

  it
    "lets projects load bundled ecosystem aliases via declarationPacks without copying"
    $ do
      root <- findBundledRegistryRoot =<< getCurrentDirectory
      let ecosystemPack = root </> "registry/ecosystem"
      withTempTree
        [ ( "tynix.config.tynix",
            source
              [ "{",
                "  name = \"demo\";",
                "  declarationPacks = [ \"" <> Text.pack ecosystemPack <> "\" ];",
                "}"
              ]
          ),
          ( "types.d.tynix",
            source
              [ "declare \"./lib.nix\" { default :: NixpkgsLib; };",
                "declare \"./pkgs.nix\" { default :: NixpkgsPkgs; };"
              ]
          ),
          ( "lib-surface.tynix",
            "let lib = import ./lib.nix; in lib.strings.concatStringsSep"
          ),
          ( "pkgs-surface.tynix",
            "let pkgs = import ./pkgs.nix; in pkgs.writeShellScriptBin"
          )
        ]
        ( \tmp -> do
            libAnalysis <- analyzeFile (tmp </> "lib-surface.tynix") >>= expectRight
            pkgsAnalysis <- analyzeFile (tmp </> "pkgs-surface.tynix") >>= expectRight
            renderedRoot libAnalysis `shouldBe` "String -> List String -> String"
            let renderedPkgs = renderedRoot pkgsAnalysis
            Text.isInfixOf "String -> String ->" renderedPkgs `shouldBe` True
            Text.isInfixOf "NixpkgsDerivation" renderedPkgs `shouldBe` True
        )

  it "lets projects reuse bundled flake ecosystem aliases" $ do
    root <- findBundledRegistryRoot =<< getCurrentDirectory
    registry <- loadRegistry root registryFiles
    withTempTree
      ( registry
          <> [ ( "types.d.tynix",
                 source
                   [ "declare \"./flake-utils.nix\" { default :: NixFlakeUtilsFlake; };",
                     "declare \"./nixpkgs-flake.nix\" { default :: NixpkgsFlake; };",
                     "declare \"./home-manager.nix\" { default :: HomeManagerFlake; };",
                     "declare \"./nix-darwin.nix\" { default :: NixDarwinFlake; };",
                     "declare \"./flake-parts.nix\" { default :: FlakePartsLib; };"
                   ]
               ),
               ( "flake-utils-surface.tynix",
                 "let value = import ./flake-utils.nix; in value.lib.eachDefaultSystem"
               ),
               ( "nixpkgs-flake-surface.tynix",
                 "let value = import ./nixpkgs-flake.nix; in value.lib.optional"
               ),
               ( "home-manager-surface.tynix",
                 "let value = import ./home-manager.nix; in value.lib.homeManagerConfiguration"
               ),
               ( "nix-darwin-surface.tynix",
                 "let value = import ./nix-darwin.nix; in value.lib.darwinSystem"
               ),
               ( "flake-parts-surface.tynix",
                 "let value = import ./flake-parts.nix; in value.mkFlake"
               )
             ]
      )
      ( \tmp -> do
          flakeUtilsAnalysis <-
            analyzeFile (tmp </> "flake-utils-surface.tynix") >>= expectRight
          nixpkgsFlakeAnalysis <-
            analyzeFile (tmp </> "nixpkgs-flake-surface.tynix") >>= expectRight
          homeManagerAnalysis <-
            analyzeFile (tmp </> "home-manager-surface.tynix") >>= expectRight
          nixDarwinAnalysis <-
            analyzeFile (tmp </> "nix-darwin-surface.tynix") >>= expectRight
          flakePartsAnalysis <-
            analyzeFile (tmp </> "flake-parts-surface.tynix") >>= expectRight
          let renderedFlakeUtils = renderedRoot flakeUtilsAnalysis
              renderedNixpkgsFlake = renderedRoot nixpkgsFlakeAnalysis
              renderedNixDarwin = renderedRoot nixDarwinAnalysis
          Text.isInfixOf "NixpkgsSystem ->" renderedFlakeUtils `shouldBe` True
          Text.isSuffixOf "-> NixFlakeOutputs" renderedFlakeUtils `shouldBe` True
          Text.isInfixOf "forall" renderedNixpkgsFlake `shouldBe` True
          Text.isInfixOf "Bool ->" renderedNixpkgsFlake `shouldBe` True
          Text.isInfixOf "List" renderedNixpkgsFlake `shouldBe` True
          renderedRoot homeManagerAnalysis
            `shouldBe` "HomeManagerConfigurationArgs -> HomeManagerConfiguration"
          renderedNixDarwin `shouldBe` "NixDarwinEvalConfigArgs -> NixosConfiguration"
          renderedRoot flakePartsAnalysis
            `shouldBe` "FlakePartsMkFlakeArgs -> FlakePartsModule -> NixFlakeOutputs"
      )

  it "lets projects reuse bundled community flake aliases" $ do
    root <- findBundledRegistryRoot =<< getCurrentDirectory
    registry <- loadRegistry root registryFiles
    withTempTree
      ( registry
          <> [ ( "types.d.tynix",
                 source
                   [ "declare \"./devenv.nix\" { default :: DevenvFlake; };",
                     "declare \"./treefmt-nix.nix\" { default :: TreefmtNixFlake; };",
                     "declare \"./pre-commit-hooks.nix\" { default :: PreCommitHooksFlake; };",
                     "declare \"./crane.nix\" { default :: CraneFlake; };",
                     "declare \"./deploy-rs.nix\" { default :: DeployRsFlake; };",
                     "declare \"./nixvim.nix\" { default :: NixvimFlake; };",
                     "declare \"./sops-nix.nix\" { default :: SopsNixFlake; };",
                     "declare \"./agenix.nix\" { default :: AgenixFlake; };",
                     "declare \"./disko.nix\" { default :: DiskoFlake; };",
                     "declare \"./colmena.nix\" { default :: ColmenaFlake; };"
                   ]
               ),
               ( "devenv-surface.tynix",
                 "let value = import ./devenv.nix; in value.lib.mkShell"
               ),
               ( "treefmt-surface.tynix",
                 "let value = import ./treefmt-nix.nix; in value.lib.evalModule"
               ),
               ( "pre-commit-surface.tynix",
                 "let value = import ./pre-commit-hooks.nix; in value.lib.x86_64-linux.run"
               ),
               ( "crane-surface.tynix",
                 "let value = import ./crane.nix; in value.lib.x86_64-linux.buildPackage"
               ),
               ( "deploy-surface.tynix",
                 "let value = import ./deploy-rs.nix; in value.lib.x86_64-linux.deployChecks"
               ),
               ( "nixvim-surface.tynix",
                 "let value = import ./nixvim.nix; in value.homeManagerModules.default"
               ),
               ( "sops-surface.tynix",
                 "let value = import ./sops-nix.nix; in value.nixosModules.default"
               ),
               ( "agenix-surface.tynix",
                 "let value = import ./agenix.nix; in value.packages.x86_64-linux.default"
               ),
               ( "disko-surface.tynix",
                 "let value = import ./disko.nix; in value.nixosModules.disko"
               ),
               ( "colmena-surface.tynix",
                 "let value = import ./colmena.nix; in value.lib.makeHive"
               )
             ]
      )
      ( \tmp -> do
          devenvAnalysis <- analyzeFile (tmp </> "devenv-surface.tynix") >>= expectRight
          treefmtAnalysis <- analyzeFile (tmp </> "treefmt-surface.tynix") >>= expectRight
          preCommitAnalysis <-
            analyzeFile (tmp </> "pre-commit-surface.tynix") >>= expectRight
          craneAnalysis <- analyzeFile (tmp </> "crane-surface.tynix") >>= expectRight
          deployAnalysis <- analyzeFile (tmp </> "deploy-surface.tynix") >>= expectRight
          nixvimAnalysis <- analyzeFile (tmp </> "nixvim-surface.tynix") >>= expectRight
          sopsAnalysis <- analyzeFile (tmp </> "sops-surface.tynix") >>= expectRight
          agenixAnalysis <- analyzeFile (tmp </> "agenix-surface.tynix") >>= expectRight
          diskoAnalysis <- analyzeFile (tmp </> "disko-surface.tynix") >>= expectRight
          colmenaAnalysis <- analyzeFile (tmp </> "colmena-surface.tynix") >>= expectRight
          let renderedTreefmt = renderedRoot treefmtAnalysis
              renderedPreCommit = renderedRoot preCommitAnalysis
              renderedCrane = renderedRoot craneAnalysis
              renderedAgenix = renderedRoot agenixAnalysis
          Text.isInfixOf "pkgs :: NixpkgsPkgs" (renderedRoot devenvAnalysis)
            `shouldBe` True
          renderedTreefmt `shouldBe` "NixpkgsPkgs -> dynamic -> TreefmtNixEvalResult"
          renderedPreCommit `shouldBe` "PreCommitHooksRunArgs -> PreCommitHooksRunResult"
          renderedCrane `shouldBe` "CraneBuildArgs -> NixpkgsDerivation"
          renderedRoot deployAnalysis `shouldBe` "dynamic -> dynamic"
          renderedRoot nixvimAnalysis `shouldBe` "dynamic"
          renderedRoot sopsAnalysis `shouldBe` "dynamic"
          renderedAgenix `shouldBe` "NixpkgsPackage"
          renderedRoot diskoAnalysis `shouldBe` "dynamic"
          renderedRoot colmenaAnalysis `shouldBe` "dynamic -> dynamic"
      )

registryFiles :: [FilePath]
registryFiles =
  [ "registry/ecosystem/nixpkgs-lib.d.tynix",
    "registry/ecosystem/nixpkgs-pkgs.d.tynix",
    "registry/ecosystem/flake-ecosystem.d.tynix",
    "registry/ecosystem/community-flakes.d.tynix",
    "registry/ecosystem/nixos-modules.d.tynix"
  ]

bundledDeclarationFiles :: [FilePath]
bundledDeclarationFiles =
  [ "registry/workspace/builtins.d.tynix",
    "registry/workspace/flake.d.tynix",
    "registry/workspace/tynix.config.d.tynix"
  ]
    <> registryFiles

workspaceRegistryFiles :: [FilePath]
workspaceRegistryFiles =
  [ "registry/workspace/builtins.d.tynix",
    "registry/workspace/flake.d.tynix",
    "registry/workspace/tynix.config.d.tynix"
  ]

loadRegistry :: FilePath -> [FilePath] -> IO [(FilePath, Text)]
loadRegistry root files =
  forM files $ \relative -> do
    content <- TextIO.readFile (root </> relative)
    pure (relative, content)

bundledRegistryMarker :: FilePath
bundledRegistryMarker = "registry/workspace/builtins.d.tynix"

findBundledRegistryRoot :: FilePath -> IO FilePath
findBundledRegistryRoot start = go start
  where
    go dir = do
      marker <- doesFileExist (dir </> bundledRegistryMarker)
      let parent = takeDirectory dir
      if marker
        then pure dir
        else
          if parent == dir
            then
              expectationFailure
                ("could not find " <> bundledRegistryMarker <> " above " <> start)
                >> fail "missing bundled registry"
            else go parent

isRight :: Either a b -> Bool
isRight (Right _) = True
isRight _ = False

renderedRoot :: Analysis -> Text
renderedRoot = maybe "" renderScheme . analysisRoot
