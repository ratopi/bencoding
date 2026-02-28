# bencoding

An encoder/decoder for the [Bencoding](https://www.bittorrent.org/beps/bep_0003.html) data format in pure Erlang.

Bencoding is the serialization format used by the [BitTorrent](https://www.bittorrent.org/) protocol
for torrent files and tracker communication.


## Installation

### Erlang (rebar3)

Add `bencoding` to your `deps` in `rebar.config`:

```erlang
{deps, [bencoding]}.
```

### Elixir (Mix)

Add `bencoding` to your `deps` in `mix.exs`:

```elixir
defp deps do
  [
    {:bencoding, "~> 1.0"}
  ]
end
```

See [hex.pm/packages/bencoding](https://hex.pm/packages/bencoding) for the latest version info.


## Type Mapping

Bencoding defines four data types. This library maps them to Erlang types as follows:

| Bencoding Type | Erlang Type | Example (Bencoded) | Example (Erlang) |
|---|---|---|---|
| Integer | `integer()` | `i42e` | `42` |
| Byte String | `binary()` | `4:spam` | `<<"spam">>` |
| List | `list()` | `l4:spami42ee` | `[<<"spam">>, 42]` |
| Dictionary | `map()` | `d3:cow3:mooe` | `#{<<"cow">> => <<"moo">>}` |

**Notes:**
- Bencoded strings are decoded as **binaries** (`<<...>>`), not as Erlang charlists.
- Dictionary keys are always byte strings (binaries).
- Dictionary keys are encoded in **sorted order** (sorted as raw strings, not alphanumerics), as required by BEP 3.
- Encoding and decoding are **roundtrip-safe**: `decode(encode(Value))` yields the original value.


## API

The library exports two functions:

### `bencoding:encode/1`

Encodes an Erlang term into a bencoded binary.

```erlang
%% Encode a string
{ok, <<"4:spam">>} = bencoding:encode(<<"spam">>).

%% Encode an integer
{ok, <<"i42e">>} = bencoding:encode(42).

%% Encode a list
{ok, <<"l4:spam4:eggse">>} = bencoding:encode([<<"spam">>, <<"eggs">>]).

%% Encode a dictionary (keys are automatically sorted)
{ok, <<"d3:cow3:moo4:spam4:eggse">>} = bencoding:encode(#{
    <<"spam">> => <<"eggs">>,
    <<"cow">> => <<"moo">>
}).

%% Encode an empty dictionary
{ok, <<"de">>} = bencoding:encode(#{}).

%% Encode nested structures
{ok, Encoded} = bencoding:encode(#{
    <<"name">> => <<"example">>,
    <<"files">> => [<<"a.txt">>, <<"b.txt">>],
    <<"size">> => 1024
}),
<<"d5:filesl5:a.txt5:b.txte4:name7:example4:sizei1024ee">> = Encoded.
```

**Returns:** `{ok, Binary}` on success.


### `bencoding:decode/1`

Decodes a bencoded binary into an Erlang term.

```erlang
%% Decode a string
{ok, <<"spam">>, <<>>} = bencoding:decode(<<"4:spam">>).

%% Decode an integer
{ok, 42, <<>>} = bencoding:decode(<<"i42e">>).

%% Decode a list
{ok, [<<"spam">>, <<"eggs">>], <<>>} = bencoding:decode(<<"l4:spam4:eggse">>).

%% Decode a dictionary
{ok, #{<<"cow">> := <<"moo">>}, <<>>} = bencoding:decode(<<"d3:cow3:mooe">>).
```

**Returns:** `{ok, Value, Rest}` on success, where `Rest` is the remaining (not yet decoded) binary data.

The `Rest` value is useful when decoding data that may have trailing content:

```erlang
{ok, 42, <<"extra">>} = bencoding:decode(<<"i42eextra">>).
```


## Error Handling

The decoder detects and rejects certain invalid inputs as defined by the Bencoding specification:

```erlang
%% Negative zero is not allowed
{error, negative_zero} = bencoding:decode(<<"i-0e">>).

%% Leading zeros are not allowed
{error, leading_zero} = bencoding:decode(<<"i03e">>).
{error, leading_zero} = bencoding:decode(<<"i-03e">>).
```

Other malformed inputs (e.g. missing terminators, invalid characters) will result in
a `function_clause` error or a `badmatch` error.


## Practical Example: Reading a Torrent File

```erlang
%% Read and decode a .torrent file
{ok, FileContent} = file:read_file("example.torrent"),
{ok, Torrent, <<>>} = bencoding:decode(FileContent),

%% Access the info dictionary
Info = maps:get(<<"info">>, Torrent),

%% Get the file name
Name = maps:get(<<"name">>, Info),
io:format("Torrent name: ~s~n", [Name]),

%% Get the announce URL
Announce = maps:get(<<"announce">>, Torrent),
io:format("Tracker: ~s~n", [Announce]).
```


## Using from Elixir

The library can be used directly from Elixir without any wrapper:

```elixir
# Encoding
{:ok, encoded} = :bencoding.encode("spam")
# => {:ok, "4:spam"}

# Encoding a map (keys are sorted automatically)
{:ok, encoded} = :bencoding.encode(%{"cow" => "moo", "spam" => "eggs"})
# => {:ok, "d3:cow3:moo4:spam4:eggse"}

# Decoding
{:ok, value, rest} = :bencoding.decode("d3:cow3:mooe")
# => {:ok, %{"cow" => "moo"}, ""}

# Reading a torrent file
{:ok, content} = File.read("example.torrent")
{:ok, torrent, ""} = :bencoding.decode(content)
announce = Map.get(torrent, "announce")

# Error handling
{:error, :negative_zero} = :bencoding.decode("i-0e")
{:error, :leading_zero} = :bencoding.decode("i03e")
```


## Specification Compliance

This library follows the Bencoding specification as defined in
[BEP 3: The BitTorrent Protocol Specification](https://www.bittorrent.org/beps/bep_0003.html):

- Integers are encoded as `i<integer in base 10>e`. Integers have no size limitation.
- Byte strings are encoded as `<length>:<contents>`
- Lists are encoded as `l<elements>e`
- Dictionaries are encoded as `d<pairs>e` with keys sorted as raw strings (not alphanumerics), as required by BEP 3
- Leading zeros in integers are rejected (`i03e` is invalid, `i0e` is valid)
- Negative zero (`i-0e`) is rejected

**Note for torrent client developers:** BEP 3 states that the info-hash
(the SHA-1 hash of the bencoded `info` dictionary) must be computed from
the bencoded form as found in the `.torrent` file. Clients must
either reject invalid metainfo files or extract the info substring directly.
They must not perform a decode-encode roundtrip on invalid data.


## License

MIT — see [LICENSE](LICENSE) for details.


## Issues

If you find a bug or miss a feature, please open an issue:
https://github.com/ratopi/bencoding/issues
