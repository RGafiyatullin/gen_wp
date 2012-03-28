-module(gen_wp_example).

-behaviour(gen_wp).

-export([
	start_link/1
	]).
-export([
	init/1,
	handle_cast/2,
	handle_call/3,
	handle_fork_cast/3,
	handle_fork_call/4
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

handle_cast( sync_cast, State ) ->
	{ noreply, State };

handle_cast( fork_cast, State ) ->
	{ fork, fork_cast, State };

handle_cast( Message, State ) ->
	{ stop, { badarg, Message }, State }.

handle_call( sync_call, _ReplyTo, State ) ->
	{ reply, sync_reply, State };

handle_call( fork_call, _ReplyTo, State ) ->
	{ fork, fork_call, State };

handle_call( Request, _ReplyTo, State ) ->
	{ stop, { badarg, Request }, badarg, State }.

handle_fork_cast( _Arg, fork_cast, _WP ) ->
	{ noreply, ok }.

handle_fork_call( _Arg, fork_call, _ReplyTo, _WP ) ->
	{ reply, fork_reply, ok }.


basic_test() ->
	{ ok, E } = gen_wp_example:start_link( a ),
	ok = gen_wp:cast( E, sync_cast ),
	sync_reply = gen_wp:call( E, sync_call, infinity ),
	ok = gen_wp:cast( E, fork_cast ),
	fork_reply = gen_wp:call( E, fork_call, infinity ).

