# mmzk-env — Project Structure

## Core parsing

- `Data.Env.TypeParser` — `TypeParser` typeclass: `parseType :: String -> Either String a` and `parseMissing :: Either String a`. Instances for `String`, `Text`, `Int`, `Bool`, `Maybe a`, numeric types, etc. `parseMissing` defaults to `Left "missing required"` for most types; `Maybe` overrides to `Right Nothing`.
- `Data.Env.TypeParserW` — `TypeParserW p a`: witness-parameterised parser. `Solo` delegates to `TypeParser`. Useful for custom per-field parsing logic.
- `Data.Env.EnumParser` — `EnumParser` newtype; `deriving TypeParser via (EnumParser MyEnum)` for any `Bounded + Enum + Show` type.
- `Data.Env.ParseError` — `ParseError [FieldError]`, `FieldError { errField, errMessage }`, `renderParseError`.

## Schema parsing

- `Data.Env.RecordParser` — `RecordParser`: plain (non-witness) schema parsing via `Generic`. `parseRecord :: Map String String -> Either ParseError a`.
- `Data.Env.RecordParserW` — **Main schema API**. `Col c a` type family (`Col 'Dec a = String -> Either String a`; `Col 'Res a = a`). Schema record fields in `'Dec` are parser functions; `'Res` holds resolved values. Key exports: `typeParser`, `orElse`, `fromTypeParserW`, `parseRecordW`, `HasDefaultSchema`/`defaultSchema`.
- `Data.Env.ExtractFields` — `ExtractFields`: generically extracts field names from a record type; drives camelCase→UPPER_SNAKE_CASE env var lookup.

## Top-level interface

- `Data.Env` — `EnvSchemaW`: `validateEnvW schema` takes the schema value and reads from the environment. `validateEnvWDefault` auto-derives the schema from `TypeParser`. `EnvSchema`: plain (non-witness) variant.

## Witnesses (legacy / bridge)

These implement `TypeParserW` and remain valid for use with `fromTypeParserW`:

- `Data.Env.Witness.DefaultString` — `DefaultString (s :: Symbol) a`: parses `a` defaulting to the type-level string `s`.
- `Data.Env.Witness.DefaultNum` — `DefaultNum (n :: Nat) a`: parses `a` defaulting to the type-level natural `n`.
- `Data.Env.Witness.DefaultBool` — `DefaultBool (b :: Bool) Bool`: lenient bool parser (accepts `"true"`, `"1"`, `"t"`, etc.) defaulting to `b`.

## Key design

`Col 'Dec a = String -> Either String a` — each field in the schema record IS its parser function. `defaultSchema @MyConfig` auto-derives all fields from `TypeParser`. Override individual fields with `typeParser @T \`orElse\` value` or `fromTypeParserW @WitnessType`. Pass the schema value to `validateEnvW`; different callers can pass different schemas without needing different types.
