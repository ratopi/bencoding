%%%-------------------------------------------------------------------
%%% @author Ralf Thomas Pietsch <ratopi@abwesend.de>
%%% @copyright (C) 2018, Ralf Thomas Pietsch
%%% @doc
%%%
%%% @end
%%% Created : 22. Okt 2018 22:22
%%%-------------------------------------------------------------------
-module(bencoding_SUITE).
-author("Ralf Thomas Pietsch <ratopi@abwesend.de>").

-include_lib("common_test/include/ct.hrl").

%% API
-export([init_per_suite/1, end_per_suite/1, init_per_testcase/2, end_per_testcase/2, all/0]).
-export([decode_string/1, decode_int/1, decode_list/1, decode_dictionary/1]).

all() -> [
	decode_string,
	decode_int,
	decode_list,
	decode_dictionary
].

% ---

init_per_suite(Config) ->
	Config.


end_per_suite(_Config) ->
	ok.

init_per_testcase(_, Config) ->
	Config.


end_per_testcase(_, _Config) ->
	ok.

% ---

decode_string(_Config) ->
	{ok, "test", <<>>} = bencoding:decode(<<"4:test">>),
	{ok, "krimskram", <<>>} = bencoding:decode(<<"9:krimskram">>).

decode_int(_Config) ->
	{ok, 1, <<>>} = bencoding:decode(<<"i1e">>),
	{ok, -3, <<>>} = bencoding:decode(<<"i-3e">>).

decode_list(_Config) ->
	{ok, [], <<>>} = bencoding:decode(<<"le">>),
	{ok, ["spam", "eggs"], <<>>} = bencoding:decode(<<"l4:spam4:eggse">>).

decode_dictionary(_Config) ->
	{ok, Map, <<>>} = bencoding:decode(<<"d3:cow3:mooe">>),
	"moo" = maps:get("cow", Map).
