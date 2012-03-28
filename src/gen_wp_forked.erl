-module(gen_wp_forked).

-behaviour(gen_server).

-export([
	start_link/3
]).
-export([
	init/1,
	handle_call/3,
	handle_cast/2,
	handle_info/2,
	terminate/2,
	code_change/3
]).

-include("gen_wp_types.hrl").

-record(s_cast, {
	wp :: pid(),
	mod :: module_spec(),
	arg :: term(),
	msg :: term()
	}).

-record(s_call, {
	wp :: pid(),
	mod :: module_spec(),
	arg :: term(),
	req :: term(),
	reply_to :: term()
	}).

start_link( WP, { Mod, Arg }, { cast, ForkMessage } ) ->
	gen_server:start_link( ?MODULE, { cast, WP, { Mod, Arg }, ForkMessage }, [] );

start_link( WP, { Mod, Arg }, { call, ReplyTo, ForkRequest } ) ->
	gen_server:start_link( ?MODULE, { call, WP, { Mod, Arg }, ReplyTo, ForkRequest } ).

init({ cast, WP, { Mod, Arg }, ForkMessage }) ->
	gen_server:cast( self(), 'gen_wp_forked.process' ),
	{ ok, #s_cast{
		wp = WP,
		mod = Mod,
		arg = Arg,
		msg = ForkMessage
	} };
init({ call, WP, { Mod, Arg }, ReplyTo, ForkRequest }) ->
	gen_server:cast( self(), 'gen_wp_forked.process' ),
	{ ok, #s_call{
		wp = WP,
		mod = Mod,
		arg = Arg,
		req = ForkRequest,
		reply_to = ReplyTo
	} }.

handle_call( Request, _From, State ) ->
	{ stop, { bad_arg, Request}, State}.

handle_cast( 'gen_wp_forked.process', State = #s_cast{} ) ->
	{ stop, not_implemented, State };

handle_cast( 'gen_wp_forked.process', State = #s_call{} ) ->
	{ stop, not_implemented, State };

handle_cast( Request, State ) ->
	{ stop, { bad_arg, Request }, State }.

handle_info( Message, State ) ->
	{ stop, { bad_arg, Message }, State }.

terminate( _Reason, _State ) ->
	ok.

code_change( _OldVsn, State, _Extra ) ->
	{ ok, State }.

