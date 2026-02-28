%%%-------------------------------------------------------------------
%%% @author Ralf Thomas Pietsch <ratopi@abwesend.de>
%%% @copyright (C) 2016-2026, Ralf Thomas Pietsch
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
-export([
	encode_string/1, encode_int/1, encode_list/1, encode_dictionary/1,
	decode_string/1, decode_int/1, decode_int_errors/1, decode_list/1, decode_dictionary/1,
	roundtrip/1, nested_structures/1, encode_dictionary_sorted/1,
	wikipedia_examples/1, mixed_list/1, decode_with_rest/1, large_integers/1
]).

all() -> [
	encode_string,
	encode_int,
	encode_list,
	encode_dictionary,

	decode_string,
	decode_int,
	decode_int_errors,
	decode_list,
	decode_dictionary,

	roundtrip,
	nested_structures,
	encode_dictionary_sorted,
	wikipedia_examples,
	mixed_list,
	decode_with_rest,
	large_integers
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

% --- encode tests

encode_string(_Config) ->
	lists:foreach(
		fun({Input, Expected}) ->
			{ok, Expected} = bencoding:encode(Input)
		end,
		[
			{<<"">>, <<"0:">>},
			{<<"test">>, <<"4:test">>},
			{<<"hello world">>, <<"11:hello world">>}
		]
	).

encode_int(_Config) ->
	lists:foreach(
		fun({Input, Expected}) ->
			{ok, Expected} = bencoding:encode(Input)
		end,
		[
			{0, <<"i0e">>},
			{1, <<"i1e">>},
			{-3, <<"i-3e">>},
			{42, <<"i42e">>},
			{-42, <<"i-42e">>}
		]
	).

encode_list(_Config) ->
	lists:foreach(
		fun({Input, Expected}) ->
			{ok, Expected} = bencoding:encode(Input)
		end,
		[
			{[], <<"le">>},
			{[<<"spam">>, <<"eggs">>], <<"l4:spam4:eggse">>},
			{[1, 2, 3], <<"li1ei2ei3ee">>}
		]
	).

encode_dictionary(_Config) ->
	lists:foreach(
		fun({Input, Expected}) ->
			{ok, Expected} = bencoding:encode(Input)
		end,
		[
			{#{}, <<"de">>},
			{#{<<"cow">> => <<"moo">>}, <<"d3:cow3:mooe">>}
		]
	).

% --- decode tests

decode_string(_Config) ->
	lists:foreach(
		fun({Input, Expected}) ->
			{ok, Expected, <<>>} = bencoding:decode(Input)
		end,
		[
			{<<"0:">>, <<"">>},
			{<<"4:test">>, <<"test">>},
			{<<"9:krimskram">>, <<"krimskram">>},
			{<<"7:bencode">>, <<"bencode">>}
		]
	).

decode_int(_Config) ->
	lists:foreach(
		fun({Input, Expected}) ->
			{ok, Expected, <<>>} = bencoding:decode(Input)
		end,
		[
			{<<"i0e">>, 0},
			{<<"i1e">>, 1},
			{<<"i-3e">>, -3},
			{<<"i42e">>, 42},
			{<<"i-42e">>, -42}
		]
	).

decode_int_errors(_Config) ->
	lists:foreach(
		fun({Input, ExpectedError}) ->
			{error, ExpectedError} = bencoding:decode(Input)
		end,
		[
			{<<"i-0e">>, negative_zero},
			{<<"i03e">>, leading_zero},
			{<<"i00e">>, leading_zero},
			{<<"i-03e">>, leading_zero}
		]
	).

decode_list(_Config) ->
	lists:foreach(
		fun({Input, Expected}) ->
			{ok, Expected, <<>>} = bencoding:decode(Input)
		end,
		[
			{<<"le">>, []},
			{<<"l4:spam4:eggse">>, [<<"spam">>, <<"eggs">>]},
			{<<"li1ei2ei3ee">>, [1, 2, 3]}
		]
	).

decode_dictionary(_Config) ->
	lists:foreach(
		fun({Input, ExpectedMap}) ->
			{ok, Map, <<>>} = bencoding:decode(Input),
			ExpectedMap = Map
		end,
		[
			{<<"de">>, #{}},
			{<<"d3:cow3:mooe">>, #{<<"cow">> => <<"moo">>}},
			{<<"d7:meaningi42e4:wiki7:bencodee">>, #{<<"meaning">> => 42, <<"wiki">> => <<"bencode">>}}
		]
	).

% --- roundtrip: encode then decode should yield the original value

roundtrip(_Config) ->
	lists:foreach(
		fun(Value) ->
			{ok, Encoded} = bencoding:encode(Value),
			{ok, Value, <<>>} = bencoding:decode(Encoded)
		end,
		[
			%% strings
			<<"hello">>,
			<<"">>,
			%% integers
			42,
			-100,
			0,
			%% lists
			[<<"a">>, <<"b">>],
			[],
			%% dictionaries
			#{<<"key">> => <<"value">>},
			#{}
		]
	).

% --- nested structures

nested_structures(_Config) ->
	lists:foreach(
		fun(Value) ->
			{ok, Encoded} = bencoding:encode(Value),
			{ok, Value, <<>>} = bencoding:decode(Encoded)
		end,
		[
			%% list in list
			[<<"a">>, [<<"b">>, <<"c">>]],
			%% dictionary in dictionary
			#{<<"outer">> => #{<<"inner">> => <<"value">>}},
			%% dictionary in list
			[#{<<"a">> => <<"b">>}],
			%% list in dictionary
			#{<<"list">> => [<<"x">>, <<"y">>]},
			%% integers in list
			[1, 2, 3],
			%% integer value in dictionary
			#{<<"num">> => 99}
		]
	).

% --- dictionary keys must be sorted lexicographically

encode_dictionary_sorted(_Config) ->
	lists:foreach(
		fun({Input, Expected}) ->
			{ok, Expected} = bencoding:encode(Input)
		end,
		[
			{#{<<"zoo">> => <<"z">>, <<"apple">> => <<"a">>, <<"mid">> => <<"m">>},
				<<"d5:apple1:a3:mid1:m3:zoo1:ze">>},
			{#{<<"b">> => <<"y">>, <<"a">> => <<"x">>},
				<<"d1:a1:x1:b1:ye">>}
		]
	).

% --- examples from wikipedia article

wikipedia_examples(_Config) ->
	lists:foreach(
		fun({Input, Expected}) ->
			{ok, Expected, <<>>} = bencoding:decode(Input)
		end,
		[
			{<<"l7:bencodei-20ee">>, [<<"bencode">>, -20]},
			{<<"d7:meaningi42e4:wiki7:bencodee">>, #{<<"meaning">> => 42, <<"wiki">> => <<"bencode">>}},
			{<<"i42e">>, 42},
			{<<"i-42e">>, -42},
			{<<"7:bencode">>, <<"bencode">>}
		]
	).

% --- mixed type lists

mixed_list(_Config) ->
	lists:foreach(
		fun(Value) ->
			{ok, Encoded} = bencoding:encode(Value),
			{ok, Value, <<>>} = bencoding:decode(Encoded)
		end,
		[
			[<<"hello">>, 42, [<<"nested">>], #{<<"k">> => <<"v">>}],
			[<<"a">>, -1, [], #{}],
			[#{<<"x">> => [1, 2]}, <<"end">>]
		]
	).

% --- decode should return remaining data

decode_with_rest(_Config) ->
	lists:foreach(
		fun({Input, ExpectedValue, ExpectedRest}) ->
			{ok, ExpectedValue, ExpectedRest} = bencoding:decode(Input)
		end,
		[
			{<<"i42eextra">>, 42, <<"extra">>},
			{<<"2:hirest">>, <<"hi">>, <<"rest">>},
			{<<"lemore">>, [], <<"more">>},
			{<<"detail">>, #{}, <<"tail">>}
		]
	).

% --- large integers

large_integers(_Config) ->
	lists:foreach(
		fun(Value) ->
			{ok, Encoded} = bencoding:encode(Value),
			{ok, Value, <<>>} = bencoding:decode(Encoded)
		end,
		[
			999999999999,
			-999999999999,
			1000000000000000,
			-1000000000000000
		]
	).
