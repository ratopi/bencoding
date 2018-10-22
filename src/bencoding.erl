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
-export([encoding/1, decoding/1]).

encoding(_) ->
	nok.

decoding(<<$i, Tail/binary>>) ->
	decode_int(0, Tail);
decoding(<<$l, Tail/binary>>) ->
	decode_list([], Tail);
decoding(<<$d, Tail/binary>>) ->
	decode_dictionary({dictionary}, Tail).

decode_int(N, <<$e, Tail/binary>>) ->
	{N, Tail};
decode_int(N, <<$0, Tail/binary>>) ->
	decode_int(N * 10, Tail);
decode_int(N, <<$1, Tail/binary>>) ->
	decode_int(N * 10 + 1, Tail);
decode_int(N, <<$2, Tail/binary>>) ->
	decode_int(N * 10 + 2, Tail);
decode_int(N, <<$3, Tail/binary>>) ->
	decode_int(N * 10 + 3, Tail);
decode_int(N, <<$4, Tail/binary>>) ->
	decode_int(N * 10 + 4, Tail);
decode_int(N, <<$5, Tail/binary>>) ->
	decode_int(N * 10 + 5, Tail);
decode_int(N, <<$6, Tail/binary>>) ->
	decode_int(N * 10 + 6, Tail);
decode_int(N, <<$7, Tail/binary>>) ->
	decode_int(N * 10 + 7, Tail);
decode_int(N, <<$8, Tail/binary>>) ->
	decode_int(N * 10 + 8, Tail);
decode_int(N, <<$9, Tail/binary>>) ->
	decode_int(N * 10 + 9, Tail).

decode_list(List, <<$e, Tail/binary>>) ->
	{List, Tail}; % TODO: reverse List !
decode_list(List, <<Tail/binary>>) ->
	decode_list([decode(Tail) | List], Tail).
