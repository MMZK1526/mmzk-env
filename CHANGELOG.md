# Revision history for mmzk-env


## 0.2.1.0 -- Unreleased

* Add `validateEnvW` and `validateEnvWWith` functions for validating environment variables with witness types.

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
