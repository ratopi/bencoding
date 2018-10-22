%%%-------------------------------------------------------------------
%%% @author ratopi@abwesend.de
%%% @copyright (C) 2016, Ralf Th. Pietsch
%%% @doc
%%%
%%% @end
%%% Created : 29. Dez 2016 17:43
%%%-------------------------------------------------------------------
-module(bencoding).
-author("ratopi@abwesend.de").

%% API
-export([encoding/1, decode/1]).

encoding(_) ->
	nok.


decode(<<L, Rest/binary>>) when L >= $0, L =< $9 ->
	decode_string({len, L - $0}, Rest);

decode(<<$i, Rest/binary>>) ->
	decode_int({positive, 0}, Rest);

decode(<<$l, Rest/binary>>) ->
	decode_list([], Rest);

decode(<<$d, Rest/binary>>) ->
	decode_dictionary(#{}, Rest).


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
