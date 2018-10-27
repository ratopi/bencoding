# bencoding

A simple encoder/decoder for decoding and encoding data in the bencoding style

## Mapping

* Bencode-integer will be converted to Erlang integers.
* Bencode-strings will be converted to Erlang binary strings.
* Bencode-list will be converted to Erlang lists.
* Bencode-dictionaries will be converted to Erlang maps. (#{})
