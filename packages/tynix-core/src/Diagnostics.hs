{-# LANGUAGE OverloadedStrings #-}

-- | Stable diagnostic identifiers.
--
-- Each user-visible error produced by the parser, kind checker, type
-- checker, or driver is tagged with a code from this module so consumers
-- (LSP, CLI, documentation) can refer to a stable handle that survives
-- copywriting changes.
--
-- The codes are namespaced by phase:
--
--   * @TPxxx@ — parser
--   * @TKxxx@ — kind checker
--   * @TCxxx@ — type checker (semantic)
--   * @TDxxx@ — driver / project / IO
--
-- Per [`docs/diagnostics.md`](../../../docs/diagnostics.md), codes are
-- considered stable once assigned. To deprecate a code, leave its entry in
-- the documentation and stop emitting it; do not reuse the number.
module Diagnostics
  ( DiagnosticCode (..),
    allDiagnosticCodes,
    diagnosticCodeText,
    withCode,
  )
where

-- | Stable identifier carried in every diagnostic message.
data DiagnosticCode
  = -- Parser (TP)
    TP0001DanglingDirective
  | TP0002DuplicateDirectiveSameLine
  | TP0003DuplicateDirectivePerLine
  | TP0004ParseError
  | -- Kind checker (TK)
    TK0001KindMismatch
  | TK0002KindOccursCheck
  | TK0003KindMustBeType
  | TK0004MissingAliasPlaceholder
  | -- Type checker (TC)
    TC0001UnboundName
  | TC0002DuplicateAttribute
  | TC0003DuplicateSignature
  | TC0004DuplicateBinding
  | TC0005MissingBindingForSignature
  | TC0006UnusedExpectedDirective
  | TC0007DuplicatePatternBinding
  | TC0008SelectOnUnknown
  | TC0009MissingField
  | TC0010DynamicKeyMissingField
  | TC0011DynamicKeyTypeMismatch
  | TC0012DynamicKeyNotStringLike
  | TC0013TypeMismatch
  | TC0014RecordMismatch
  | TC0015InvalidCast
  | TC0016OccursCheckFailed
  | TC0017MissingPlaceholder
  | TC0018NotCallable
  | TC0019NotComparable
  | TC0020NotConcatenable
  | TC0021NotUpdatable
  | TC0022DynamicLetBinding
  | TC0023EffectNotAllowed
  | TC0024ImpureInPureEval
  | TC0025CaptureNotAllowed
  | TC0026LinearityViolation
  | TC0027OpaqueType
  | -- Macro expansion (TX)
    TX0001NoMatchingRule
  | TX0002InvalidMacroPattern
  | TX0003HygieneViolation
  | TX0004InvalidTemplate
  | TX0005ExpansionLimit
  | TX0006StrayMacroVariable
  | -- Driver / project (TD)
    TD0001ReadFailed
  | TD0002DuplicateAmbientDeclaration
  | TD0003DuplicateAmbientEntry
  | TD0004ConfigDecodeError
  | TD0005ConfigBadList
  | TD0006ConfigBadItem
  | TD0007DeclarationOnlyCompile
  | TD0008DeclarationOnlyEmit
  deriving (Bounded, Enum, Eq, Ord, Show)

-- | Every code in the catalogue.
--
-- Tooling and tests enumerate this rather than restating the list, so a code
-- added to 'DiagnosticCode' is automatically held to the same rules as the
-- rest: a unique identifier, an entry in @docs/diagnostics.md@, and at least
-- one place that emits it.
allDiagnosticCodes :: [DiagnosticCode]
allDiagnosticCodes = [minBound .. maxBound]

-- | Render a code as the short identifier embedded in messages.
diagnosticCodeText :: DiagnosticCode -> String
diagnosticCodeText code = case code of
  TP0001DanglingDirective -> "TP0001"
  TP0002DuplicateDirectiveSameLine -> "TP0002"
  TP0003DuplicateDirectivePerLine -> "TP0003"
  TP0004ParseError -> "TP0004"
  TK0001KindMismatch -> "TK0001"
  TK0002KindOccursCheck -> "TK0002"
  TK0003KindMustBeType -> "TK0003"
  TK0004MissingAliasPlaceholder -> "TK0004"
  TC0001UnboundName -> "TC0001"
  TC0002DuplicateAttribute -> "TC0002"
  TC0003DuplicateSignature -> "TC0003"
  TC0004DuplicateBinding -> "TC0004"
  TC0005MissingBindingForSignature -> "TC0005"
  TC0006UnusedExpectedDirective -> "TC0006"
  TC0007DuplicatePatternBinding -> "TC0007"
  TC0008SelectOnUnknown -> "TC0008"
  TC0009MissingField -> "TC0009"
  TC0010DynamicKeyMissingField -> "TC0010"
  TC0011DynamicKeyTypeMismatch -> "TC0011"
  TC0012DynamicKeyNotStringLike -> "TC0012"
  TC0013TypeMismatch -> "TC0013"
  TC0014RecordMismatch -> "TC0014"
  TC0015InvalidCast -> "TC0015"
  TC0016OccursCheckFailed -> "TC0016"
  TC0017MissingPlaceholder -> "TC0017"
  TC0018NotCallable -> "TC0018"
  TC0019NotComparable -> "TC0019"
  TC0020NotConcatenable -> "TC0020"
  TC0021NotUpdatable -> "TC0021"
  TC0022DynamicLetBinding -> "TC0022"
  TC0023EffectNotAllowed -> "TC0023"
  TC0024ImpureInPureEval -> "TC0024"
  TC0025CaptureNotAllowed -> "TC0025"
  TC0026LinearityViolation -> "TC0026"
  TC0027OpaqueType -> "TC0027"
  TX0001NoMatchingRule -> "TX0001"
  TX0002InvalidMacroPattern -> "TX0002"
  TX0003HygieneViolation -> "TX0003"
  TX0004InvalidTemplate -> "TX0004"
  TX0005ExpansionLimit -> "TX0005"
  TX0006StrayMacroVariable -> "TX0006"
  TD0001ReadFailed -> "TD0001"
  TD0002DuplicateAmbientDeclaration -> "TD0002"
  TD0003DuplicateAmbientEntry -> "TD0003"
  TD0004ConfigDecodeError -> "TD0004"
  TD0005ConfigBadList -> "TD0005"
  TD0006ConfigBadItem -> "TD0006"
  TD0007DeclarationOnlyCompile -> "TD0007"
  TD0008DeclarationOnlyEmit -> "TD0008"

-- | Prefix a message with the diagnostic code in the canonical
-- @[Txxxxx] message@ shape used across the CLI and LSP.
withCode :: DiagnosticCode -> String -> String
withCode code msg = "[" <> diagnosticCodeText code <> "] " <> msg
