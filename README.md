# bencoding

A simple encoder/decoder for decoding and encoding data in the bencoding style


## Mapping

* Bencode-integer will be converted to Erlang integers.
* Bencode-strings will be converted to Erlang binary strings.
* Bencode-list will be converted to Erlang lists.
* Bencode-dictionaries will be converted to Erlang maps. (#{})


## Use

Get it via hex.pm!   Just add

	{deps, [bencoding]}.

to your rebar.config.

See https://hex.pm/packages/bencoding for info about the hex package.


### Decoding

Use bencoding:decode/1 to decode any bencoded content.

Example: Reading a torrent file:

	{ok, F} = file:read_file("test.torrent").
	{ok, M, _} = bencoding:decode(F).
	M.

M holds the result.


### Encoding

Use bencdoing:encode/1 to encode (with M from above):

	{ok, B} = bencoding:encode(M).
	B.
