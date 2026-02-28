# Changelog

## 2.0.0

### New Features
- Reject invalid integers: leading zeros (`i03e`, `i-03e`) return `{error, leading_zero}`
- Reject negative zero (`i-0e`) returning `{error, negative_zero}`
- Dictionary keys are now sorted as raw strings (not alphanumerics) as required by BEP 3

### Types & Specs
- Added type `bencodable/0` — recursive type for all encodable values
- Added type `encode_result/0` — `{ok, binary()}`
- Added type `decode_result/0` — `{ok, bencodable(), binary()} | {error, ...}`
- Added `-spec` for `encode/1` and `decode/1`
- Exported all types via `-export_type`

### Documentation
- Comprehensive EDoc module documentation with shell-style examples
- Function documentation for `encode/1` and `decode/1` with supported types and error cases
- README rewritten: Type Mapping table, API examples, Error Handling, Torrent file example
- Added Elixir usage examples and installation instructions
- Added Specification Compliance section referencing BEP 3 with info-hash (SHA-1) note
- Added `CHANGELOG.md`

### Build & Packaging
- Added `rebar3_ex_doc` for hex.pm documentation generation
- Added hex.pm metadata (description, licenses, links) in `rebar.config`
- Version set to `{vsn, "git"}` for Git-tag-based versioning
- Added `doc/` to `.gitignore`

### Tests
- Added tests for invalid integers: leading zeros, negative zero
- Added tests for dictionary key sorting
- Added tests for nested structures, empty structures, negative integers
- Tests refactored to use `lists:foreach/2` for data-driven test cases
- Total: 16 tests


## 1.0.0

- Initial public release
- Encode and decode all four Bencoding types: integers, byte strings, lists, dictionaries
- Dictionary keys are sorted as raw strings as required by BEP 3
- Invalid integers are rejected: leading zeros (`i03e`) and negative zero (`i-0e`)
- Type specs and exported types: `bencodable/0`, `encode_result/0`, `decode_result/0`
- Full documentation with examples for Erlang and Elixir
- hex.pm / hexdocs.pm ready

