{-# LANGUAGE OverloadedStrings #-}

-- | End-to-end behaviour of the richer type system: Hindley-Milner with
-- effect polymorphism and higher-rank types, effects, linearity, capture
-- tracking, opaque (phantom) types, dependent arrows, kind annotations, and
-- hygienic macros.
--
-- Each case runs the whole driver on a small program, so the parser, the
-- macro expander, the kind checker, and the type checker all take part.
module Main (main) where

import Control.Monad (void)
import Data.Text (Text)
import Data.Text qualified as Text
import Driver (Analysis (..), analyzeText, compileText)
import Pretty (renderScheme)
import Test.Hspec
import TestSupport (expectLeftContaining, expectRight, source)

main :: IO ()
main = hspec spec

-- | The rendered type of the root expression.
rootOf :: Text -> IO (Maybe Text)
rootOf input = do
  analysis <- analyzeText "main.tynix" input >>= expectRight
  pure (renderScheme <$> analysisRoot analysis)

-- | Expect the program to be rejected with a message containing @needle@.
rejects :: FilePath -> Text -> String -> Expectation
rejects path input needle = analyzeText path input >>= (`expectLeftContaining` needle)

accepts :: Text -> Expectation
accepts input = analyzeText "main.tynix" input >>= void . expectRight

spec :: Spec
spec = do
  describe "Hindley-Milner" $ do
    it "generalizes effect variables alongside type variables" $
      rootOf "let twice = f: x: f (f x); in twice"
        `shouldReturn` Just "forall t0 e0. (t0 -> t0 ! e0) -> t0 -> t0 ! e0"

    it "generalizes the fields of a rec attribute set" $
      accepts "rec { id = x: x; a = id 1; b = id \"s\"; }"

    it "lets a record meet its own row through an instantiated signature" $
      accepts
        ( source
            [ "lib: with lib; let",
              "  walk = cond: f: set: let",
              "    recurse = path: set: let",
              "      g = name: value: if isAttrs value && cond value",
              "        then { ${name} = recurse (path ++ [ name ]) value; }",
              "        else f (path ++ [ name ]) name value;",
              "    in mapAttrs' g set;",
              "  in recurse [ ] set;",
              "  mapAttrs' = f: set: foldl' (a: b: a // b) { } (mapAttrsToList f set);",
              "in walk (as: as._type == \"param\")"
            ]
        )

    it "checks higher-rank arguments against every instance" $ do
      let program body =
            source
              [ "let",
                "  apply :: (forall a. a -> a) -> { n :: Int; s :: String; };",
                "  apply = f: { n = f 1; s = f \"x\"; };",
                "in apply (" <> body <> ")"
              ]
      accepts (program "x: x")
      rejects "main.tynix" (program "x: x + 1") "TC0013"

  describe "effects" $ do
    it "infers latent effects from the builtins a function calls" $
      rootOf "msg: builtins.trace msg msg" `shouldReturn` Just "forall t0. t0 -> t0 ! {Trace}"

    it "rejects an effect a signature does not admit" $
      rejects
        "main.tynix"
        (source ["let", "  f :: String -> String ! {};", "  f = msg: builtins.trace msg msg;", "in f"])
        "[TC0023] effect `Trace` is not allowed here"

    it "lets tryEval discharge Throw" $
      accepts
        ( source
            [ "let",
              "  f :: Int -> Bool ! {};",
              "  f = x: (builtins.tryEval (throw \"no\")).success;",
              "in f"
            ]
        )

    it "keeps effect variables rigid in signatures" $ do
      accepts (source ["let", "  app :: forall a b e. (a -> b ! e) -> a -> b ! e;", "  app = f: x: f x;", "in app"])
      rejects
        "main.tynix"
        (source ["let", "  app :: forall a b e. (a -> b ! e) -> a -> b ! {};", "  app = f: x: f x;", "in app"])
        "TC0023"

    it "treats arrows without an effect row as untracked" $
      accepts (source ["let", "  f :: String -> String;", "  f = msg: builtins.trace msg msg;", "in f"])

    it "forbids impure builtins in flakes" $ do
      rejects "flake.tynix" "{ outputs = _: { home = builtins.getEnv \"HOME\"; }; }" "TC0024"
      rejects "flake.tynix" "{ outputs = _: builtins.currentSystem; }" "TC0024"
      accepts "{ home = builtins.getEnv \"HOME\"; }"

  describe "linear types" $ do
    let linear body = source ["let", "  f :: Int %1 -> Int;", "  f = " <> body <> ";", "in f"]
    it "accepts a binder consumed exactly once on every branch" $
      accepts (linear "x: if true then x else x + 1")

    it "rejects branches that disagree" $
      rejects "main.tynix" (linear "x: if true then x else 0") "is not used exactly once on every branch"

    it "counts uses inside closures and unrestricted calls as many" $ do
      rejects "main.tynix" (linear "x: (y: x) 1") "is used more than once"
      rejects "main.tynix" (linear "x: builtins.length [x]") "TC0026"

    it "lets a linear argument flow into a linear function" $
      accepts
        ( source
            [ "let",
              "  id1 :: Int %1 -> Int;",
              "  id1 = x: x;",
              "  use :: Int %1 -> Int;",
              "  use = x: id1 x;",
              "in use"
            ]
        )

  describe "capture tracking" $ do
    let program captures =
          source
            [ "let",
              "  mk :: (String -> String ! { Fetch }) -> String ->" <> captures <> " String ! { Fetch };",
              "  mk = fetch: url: fetch url;",
              "in mk"
            ]
    it "accepts a closure that captures only what its type lists" $
      accepts (program "{fetch}")

    it "rejects a closure that captures an unlisted capability" $
      rejects "main.tynix" (program "{}") "[TC0025] closure captures `fetch`"

  describe "opaque and phantom types" $ do
    let program body =
          source
            [ "opaque type Id t = String;",
              "type User = { name :: String; };",
              "type Pkg = { pname :: String; };",
              "let",
              "  user :: Id User;",
              "  user = \"u1\" as Id User;",
              "  pkgName :: Id Pkg -> String;",
              "  pkgName = id: id as String;",
              "in " <> body
            ]
    it "keeps phantom parameters apart" $
      rejects "main.tynix" (program "pkgName user") "Id User vs Id Pkg"

    it "converts to and from the representation only with a cast" $ do
      accepts (program "pkgName (\"p\" as Id Pkg)")
      rejects "main.tynix" (program "pkgName \"p\"") "TC0013"

    it "hides the representation's fields" $
      rejects
        "main.tynix"
        (source ["opaque type Secret = { value :: String; };", "let s = { value = \"x\"; } as Secret; in s.value"])
        "TC0027"

  describe "dependent types" $ do
    it "substitutes singleton arguments into the codomain" $ do
      rootOf "builtins.genList (i: i) 3" `shouldReturn` Just "Vec 3 Int"
      rootOf "builtins.getAttr \"a\" { a = 1; b = \"s\"; }" `shouldReturn` Just "1"
      rootOf "builtins.length [ 1 2 3 ]" `shouldReturn` Just "3"

    it "degrades type operators whose arguments are known but wide" $ do
      accepts "k: builtins.getAttr k { a = 1; b = 2; } + 1"
      accepts "k: (x :: AttrsOf Int): builtins.getAttr k x + 1"
      accepts "k: (builtins.getAttr k { pkg = { outPath = \"/nix\"; }; }).outPath"

    it "checks a dependent signature with the binder as a singleton" $
      accepts
        ( source
            [ "let",
              "  replicate :: forall a. (n :: Nat) -> a -> Vec n a;",
              "  replicate = n: x: builtins.genList (_: x) n;",
              "in replicate 2 \"x\""
            ]
        )

  describe "higher-kinded types" $ do
    it "honours explicit kind annotations" $ do
      accepts "type App (f :: Type -> Type) a = f a; let xs :: App List Int; xs = [ 1 ]; in xs"
      rejects "main.tynix" "type Bad (f :: Type) = f Int; let x :: Bad Int; x = 1; in x" "TK0001"

  describe "macros" $ do
    let enumMacro =
          [ "macro enum {",
            "  ( $( $tag:ident ),* ) => ({ $( $tag = stringify!($tag); )* });",
            "};"
          ]
    it "expands repetitions" $
      rootOf (source (enumMacro <> ["enum!(red, green)"]))
        `shouldReturn` Just "{\n  green :: \"green\";\n  red :: \"red\";\n}"

    it "renames the binders a template introduces" $ do
      let program =
            source
              [ "macro swap {",
                "  ($a:expr, $b:expr) => (let tmp = $a; in [ $b tmp ]);",
                "};",
                "let tmp = 1; in swap!(2, tmp)"
              ]
      rootOf program `shouldReturn` Just "Vec 2 (1 | 2)"
      compiled <- compileText "main.tynix" program >>= expectRight
      compiled `shouldSatisfy` Text.isInfixOf "[ tmp tmp'1_"

    it "checks typed arguments at the call site" $
      rejects
        "main.tynix"
        (source ["macro unless {", "  ($c :: Bool, $body:expr) => (if $c then null else $body);", "};", "unless!(\"yes\", 1)"])
        "\"yes\" vs Bool"

    it "type-checks templates at their definition" $
      rejects
        "main.tynix"
        (source ["macro bad {", "  ($x :: Int) => ($x + \"s\");", "};", "1"])
        "[TX0004] rule 1 of macro `bad` does not type-check"

    it "leaves shell variables in template strings alone" $
      accepts
        ( source
            [ "macro script {",
              "  ($name:string) => ({ name = $name; buildCommand = ''mkdir -p $out''; });",
              "};",
              "script!(\"hello\")"
            ]
        )

    it "renames inherited bindings in a template let" $
      rootOf
        ( source
            [ "macro names {",
              "  ($x:expr) => (let inherit (builtins) attrNames; in attrNames $x);",
              "};",
              "let attrNames = 1; in names!({ a = 1; })"
            ]
        )
        `shouldReturn` Just "List String"

    it "reports invocations no rule matches" $
      rejects "main.tynix" (source (enumMacro <> ["enum!(1 + 2)"])) "TX0001"

    it "rejects templates that depend on the call site's scope" $ do
      rejects "main.tynix" (source ["macro m {", "  ($x:expr) => (lib.id $x);", "};", "1"]) "TX0003"
      rejects "main.tynix" (source ["macro m {", "  ($x:expr) => (with $x; a);", "};", "1"]) "TX0003"

    it "pins globals so the call site cannot hijack them" $ do
      let program =
            source
              [ "macro strs {",
                "  ($xs:expr) => (map toString $xs);",
                "};",
                "let map = f: xs: 0; in strs!([ 1 2 ])"
              ]
      rootOf program `shouldReturn` Just "List String"

    it "stops runaway recursion" $
      rejects "main.tynix" (source ["macro loop {", "  ($x:expr) => (loop!($x));", "};", "loop!(1)"]) "TX0005"

    it "rejects metavariables outside templates" $
      rejects "main.tynix" "$x" "TX0006"

    it "parses type ascriptions" $
      rootOf "(1 :: Int)" `shouldReturn` Just "Int"
