-module(gen_wp_example).

-behaviour(gen_wp).

-export([
	start_link/1
	]).
-export([
	init/1
	]).

-include_lib("eunit/include/eunit.hrl").

-record(s, {
	arg :: any()
	}).

start_link(Arg) ->
	gen_wp:start_link(?MODULE, Arg).

init(Arg) ->
	{ok, #s{
		arg = Arg
	}}.

