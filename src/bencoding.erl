%%%-------------------------------------------------------------------
%%% @author ratopi@abwesend.de
%%% @copyright (C) 2016-2019, Ralf Th. Pietsch
%%% @doc
%%% Use decode to decode a binary containing an bencoded string.
%%% Use encode to generate a binary containing an bencoded string.
%%% @end
%%% Created : 2016-12-29 17:43
%%%-------------------------------------------------------------------
-module(bencoding).
-author("ratopi@abwesend.de").

%% API
-export([encode/1, decode/1]).

encode(<<String/binary>>) ->
	Len = integer_to_binary(byte_size(String)),
	{ok, <<Len/binary, $:, String/binary>>};

encode(N) when is_integer(N) ->
	Bin = integer_to_binary(N),
	{ok, <<$i, Bin/binary, $e>>};

encode(L) when is_list(L) ->
	encode_list(start, L);

encode(M) when is_map(M) ->
	encode_dictionary(start, M).


decode(<<L, Rest/binary>>) when L >= $0, L =< $9 ->
	decode_string({len, L - $0}, Rest);

decode(<<$i, Rest/binary>>) ->
	decode_int({positive, 0}, Rest);

decode(<<$l, Rest/binary>>) ->
	decode_list([], Rest);

decode(<<$d, Rest/binary>>) ->
	decode_dictionary(#{}, Rest).

%%
%% Internal Functions
%%

encode_list(start, L) ->
	encode_list([$l], L);

encode_list(Output, []) ->
	{ok, list_to_binary(lists:reverse([$e | Output]))};

encode_list(Output, [H | T]) ->
	{ok, Element} = encode(H),
	encode_list([Element | Output], T).


encode_dictionary(start, M) ->
	encode_dictionary({[$d], maps:keys(M)}, M);

encode_dictionary({Output, []}, _) ->
	{ok, list_to_binary(lists:reverse([$e | Output]))};

encode_dictionary({Output, [Key | Keys]}, Map) ->
	{ok, EncodedKey} = encode(Key),
	{ok, EncodedValue} = encode(maps:get(Key, Map)),
	encode_dictionary(
		{
			[EncodedValue, EncodedKey, Output],
			Keys
		},
		Map
	).

%%
%%
%%

decode_int({positive, N}, <<$e, Rest/binary>>) ->
	{ok, N, Rest};

decode_int({negative, N}, <<$e, Rest/binary>>) ->
	{ok, -N, Rest};

decode_int({positive, 0}, <<$-, Rest/binary>>) ->
	decode_int({negative, 0}, Rest);

decode_int({PosNeg, N}, <<L, Rest/binary>>) when L >= $0, L =< $9 ->
	decode_int({PosNeg, 10 * N + (L - $0)}, Rest).


decode_list(List, <<$e, Rest/binary>>) ->
	{ok, lists:reverse(List), Rest};

decode_list(List, <<Rest/binary>>) ->
	{ok, Element, NextRest} = decode(Rest),
	decode_list([Element | List], NextRest).


decode_dictionary(Dictionary, <<$e, Rest/binary>>) ->
	{ok, Dictionary, Rest};

decode_dictionary(Dictionary, <<Rest/binary>>) ->
	{ok, Key, NextRest} = decode(Rest),
	{ok, Value, NextNextRest} = decode(NextRest),
	decode_dictionary(maps:put(Key, Value, Dictionary), NextNextRest).


decode_string({len, L}, <<N, Rest/binary>>) when N >= $0 andalso N =< $9 ->
	decode_string({len, (L * 10 + (N - $0))}, Rest);

decode_string({len, L}, <<$:, Rest/binary>>) ->
	decode_string({text, [], L}, Rest);

decode_string({text, Text, 0}, <<Rest/binary>>) ->
	{ok, lists:reverse(Text), Rest};

decode_string({text, Text, L}, <<Char, Rest/binary>>) ->
	decode_string({text, [Char | Text], L - 1}, Rest).
