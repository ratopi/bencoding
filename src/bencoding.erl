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


decode(<<L, Tail/binary>>) when L >= $0, L =< $9 ->
	decode_string({len, L - $0}, Tail);

decode(<<$i, Tail/binary>>) ->
	decode_int({positive, 0}, Tail);

decode(<<$l, Tail/binary>>) ->
	decode_list([], Tail);

decode(<<$d, Tail/binary>>) ->
	decode_dictionary(#{}, Tail).


decode_int({positive, N}, <<$e, Tail/binary>>) ->
	{ok, N, Tail};

decode_int({negative, N}, <<$e, Tail/binary>>) ->
	{ok, -N, Tail};

decode_int({positive, 0}, <<$-, Tail/binary>>) ->
	decode_int({negative, 0}, Tail);

decode_int({PosNeg, N}, <<L, Tail/binary>>) when L >= $0, L =< $9 ->
	decode_int({PosNeg, 10 * N + (L - $0)}, Tail).


decode_list(List, <<$e, Tail/binary>>) ->
	{ok, lists:reverse(List), Tail};

decode_list(List, <<Tail/binary>>) ->
	{ok, Element, NextTail} = decode(Tail),
	decode_list([Element | List], NextTail).


decode_dictionary(Dictionary, <<$e, Tail/binary>>) ->
	{ok, Dictionary, Tail};

decode_dictionary(Dictionary, <<Tail/binary>>) ->
	{ok, Key, NextTail} = decode(Tail),
	{ok, Value, NextNextTail} = decode(NextTail),
	decode_dictionary(maps:put(Key, Value, Dictionary), NextNextTail).


decode_string({len, L}, <<N, Tail/binary>>) when N >= $0 andalso N =< $9 ->
	decode_string({len, (L * 10 + (N - $0))}, Tail);

decode_string({len, L}, <<$:, Tail/binary>>) ->
	decode_string({text, [], L}, Tail);

decode_string({text, Text, 0}, <<Tail/binary>>) ->
	{ok, lists:reverse(Text), Tail};

decode_string({text, Text, L}, <<Char, Tail/binary>>) ->
	decode_string({text, [Char | Text], L - 1}, Tail).
