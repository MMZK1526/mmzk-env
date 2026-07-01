# Revision history for mmzk-env


## 0.5.0.0 -- 2026-07-01

### Value-level schema redesign (breaking)

The witness-column system has been replaced with a value-level schema
pattern. Schema records now use the `Col` column family instead of
`Column`/`Di`:

```haskell
-- Before:
data Config c = Config
  { port :: Di (DefaultNum 5432) c Int
  , host :: Di (DefaultString "localhost") c Text }

-- After:
data Config c = Config
  { port :: Col c Int
  , host :: Col c Text }
```

`Col 'Dec a = String -> Either String a` — each field in the declaration
record holds a parser function. `Col 'Res a = a` — unchanged.

`validateEnvW` now takes the schema value explicitly instead of a type
application:

```haskell
-- Before:
validateEnvW @(Config 'Dec)

-- After:
validateEnvW myConfig         -- myConfig :: Config 'Dec
validateEnvWDefault @Config   -- auto-derive from TypeParser instances
```

`parseRecordW` likewise takes the schema value:

```haskell
-- Before:
parseRecordW @(Config 'Dec) envMap

-- After:
parseRecordW myConfig envMap
```

`Column`, `Di`, and the old type-application-only `parseRecordW`/`validateEnvW`
are removed.

### New exports in `Data.Env.RecordParserW`

* `Col` — type family replacing `Column`/`Di`.
* `typeParser @T` — builds the standard `String -> Either String T` parser
  from the `TypeParser T` instance.
* `f \`orElse\` d` — sets the fallback value when the env variable is
  absent or empty; designed for infix: `typeParser @Int \`orElse\` 5432`.
* `fromTypeParserW @W` — bridges an existing `TypeParserW` witness into a
  field function.
* `HasDefaultSchema` / `defaultSchema @T` — generically derives a
  `T 'Dec` value whose every field uses `typeParser`.
* `validateEnvWDefault @T` — shorthand for
  `validateEnvW (defaultSchema @T)`.

### `TypeParser` and `TypeParserW` unchanged

`TypeParser`, `TypeParserW`, and all witness modules (`DefaultNum`,
`DefaultString`, `DefaultBool`) are unchanged. Witnesses remain usable
via `fromTypeParserW`.


## 0.4.0.0 -- 2026-04-16

**Breaking changes:**

* `validateEnv`, `validateEnvWith`, `validateEnvW`, `validateEnvWWith`, `parseRecord`, and `parseRecordW` now return `Either ParseError a` instead of `Either String a`. Code that matches on `Left err` where `err :: String` must be updated to use `renderParseError err` (or pattern-match on `ParseError`/`FieldError`).

* `TypeParser` gains a new method `parseMissing :: Either String a` (default: `Left "missing required environment variable"`). Custom `TypeParser` instances that handled the empty-string case inside `parseType` to supply a default **must** move that logic to `parseMissing` — `parseType ""` is no longer called for absent environment variables.

* `TypeParserW` gains a new method `parseMissingW :: Proxy p -> Either String a` (default: `parseTypeW proxy ""`). Custom `TypeParserW` instances that relied on `parseTypeW proxy ""` being called for absent variables are unaffected by the default; however, the same caveat as `parseMissing` applies if an instance overrides `parseTypeW` for the empty-string case.

**New exports:**

* `Data.Env.ParseError` is now an exposed module exporting `ParseError(..)`, `FieldError(..)`, `renderParseError`, and `renderFieldError`. All are also re-exported from `Data.Env`.

**Improved error messages:**

* All field failures are now collected in a single pass (no short-circuiting on the first error). `ParseError` is a list of `FieldError` values, one per failing field, in field-declaration order.

* Missing required environment variables now produce `"missing required environment variable"` instead of a gigaparsec parse error on an empty string.

* Base type parsers (`String`, `Int`, `Word`, `Bool`, numeric fixed-width types) now use `P.label` to produce readable expected-descriptions (e.g. `expected an integer`) instead of raw token expectations.

* `renderParseError` formats errors with the field name on its own header line followed by the detail indented below; multiple failures are numbered.

* `EnumParser` error messages now read `invalid value "x"; expected one of: A, B, C` instead of the previous format.


## 0.3.0.0 -- 2026-03-22

* Fixed `DefaultBool` to follow the proper witness pattern:
  * Changed from a newtype with `TypeParser` to a phantom type with `TypeParserW`.
  * Added second type parameter (consistent with `DefaultNum` and `DefaultString`) enabling use with the `Di` type alias.
* `DefaultBool` now accepts `True`, `False`, `true`, `false`, `T`, `F`, `t`, `f`, `1`, `0`.


## 0.2.1.1 -- 2026-03-22

* Added `DefaultBool` for `Bool` fields with a type-level default value.
* Added `DataKinds` to default extensions.


## 0.2.1.0 -- 2025-11-30

* Add the missing `validateEnvW` and `validateEnvWWith` functions for validating environment variables with witness types.

* Add `DefaultString` witness type for providing default string values.

* Add more texts and examples.


## 0.2.0.0 -- 2025-11-29

* Witnessed record parsing (witness types carry type-level information that determines parsing behaviour):
  * Add `RecordParserW` for witnessed record parsing.
  * Add `TypeParserW` for witnessed type parsing.
  * Add `DefaultNum` witness type for providing default numeric values.

* Add convenient `Maybe` result variants for parsers.

* Add runnable example executables in `app/`:
  * `quickstart-example`: Basic environment validation example.
  * `enum-example`: Enumerated type parsing example.
  * `newtype-example`: Custom parser with newtype wrapper example.
  * `witness-example`: Witness types with default values example.

* Add more tests.

* Heavy refactoring.


## 0.1.2.0 -- 2025-11-16

* Add support for parsing `Text`.


## 0.1.1.1 -- 2025-09-29

* Fix typo in the cabal file.


## 0.1.1.0 -- 2025-09-28

* Added support for parsing enumerated types using `EnumParser`.
  * Updated documentation to include examples of enum parsing.


## 0.1.0.0 -- 2025-08-03

* First version. Released on an unsuspecting world.
