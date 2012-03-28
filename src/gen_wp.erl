-module(gen_wp).

-behaviour(gen_server).

-export([
	behaviour_info/1
	]).
-export([
	start_link/2,
	start_link/3,
	start_link/4,
	call/2,
	call/3,
	cast/2,
	reply/2
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

-record(s, {
	mod :: module_spec(),
	arg :: any(),
	mod_state :: any(),
	fork_sup :: pid()
	}).

%%% Declare behaviour
behaviour_info(callbacks) ->
	[
		{ init, 1 },
		{ handle_cast, 2 }
	].

%%% API

-spec start_link( module_spec(), module_arg() ) -> {ok, pid()}.
-spec start_link( module_spec(), module_arg(), [term()] ) -> {ok, pid()}.
-spec start_link( atom(), module_spec(), module_arg(), [term()] ) -> {ok, pid()}.

start_link( Mod, Arg ) ->
	start_link( Mod, Arg, [] ).

start_link( Mod, Arg, Opts ) ->
	gen_server:start_link(?MODULE, { main, Mod, Arg }, Opts).

start_link( Reg, Mod, Arg, Opts ) ->
	gen_server:start_link(Reg, ?MODULE, { main, Mod, Arg }, Opts).


call( Server, Request ) ->
	gen_server:call( Server, { 'gen_wp.call', Request } ).

call( Server, Request, Timeout ) ->
	gen_server:call( Server, { 'gen_wp.call', Request }, Timeout ).

cast( Server, Message ) ->
	gen_server:cast( Server, { 'gen_wp.cast', Message } ).

reply( { 'gen_wp.reply_to', ReplyTo }, ReplyWith ) ->
	gen_server:reply( ReplyTo, ReplyWith ).

%%% Behave as gen_server

init({ main, Mod, Arg }) ->
	{ok, ForkSup} = gen_wp_fork_sup:start_link( self(), Mod, Arg ),
	case catch Mod:init( Arg ) of
		{ ok, ModState } ->
			{ ok, #s{
				mod = Mod,
				arg = Arg,
				mod_state = ModState,
				fork_sup = ForkSup
			} };

		{ ok, ModState, Timeout } ->
			{ ok, #s{
				mod = Mod,
				arg = Arg,
				mod_state = ModState,
				fork_sup = ForkSup
			}, Timeout };

		Else ->
			{ stop, { bad_return_value, Else } }
	end.

handle_call( { 'gen_wp.call', Request }, From, State = #s{
		mod = Mod,
		mod_state = ModState
	} ) ->
	case Mod:handle_call( Request, { 'gen_wp.reply_to', From }, ModState ) of
		{ noreply, NModState } ->
			{ noreply, State #s{ mod_state = NModState } };
		{ reply, ReplyWith, NModState } ->
			{ reply, ReplyWith, State #s{ mod_state = NModState } };
		{ stop, Reason, ReplyWith, NModState } ->
			{ stop, Reason, ReplyWith, State #s{ mod_state = NModState } };
		{ stop, Reason, NModState } ->
			{ stop, Reason, State #s{ mod_state = NModState } }
	end;

handle_call( Request, _From, State = #s{} ) ->
	{ stop, { bad_arg, Request }, State }.

handle_cast( { 'gen_wp.cast', Message }, State = #s{
		mod = Mod,
		mod_state = ModState,
		fork_sup = ForkSup
	} ) ->
	case Mod:handle_cast( Message, ModState ) of
		{ noreply, NModState } ->
			{ noreply, State #s{ mod_state = NModState } };

		{ noreply, NModState, Timeout } ->
			{ noreply, State #s{ mod_state = NModState }, Timeout };

		{ fork, ForkMessage, NModState } ->
			{ ok, _Child } = fork_handle_cast( ForkSup, ForkMessage ),
			{ noreply, NModState };

		{ stop, Reason, NModState } ->
			{ stop, Reason, State #s{ mod_state = NModState } }
	end;

handle_cast( Message, State = #s{} ) ->
	{ stop, { bad_arg, Message }, State }.

handle_info( Message, State = #s{} ) ->
	{ stop, { bad_arg, Message }, State }.

terminate( _Reason, _State ) ->
	ok.

code_change( _OldVsn, State, _Extra ) ->
	{ ok, State }.


%%% Internals

fork_handle_cast( ForkSup, ForkMessage ) ->
	{ok, _Child} = gen_wp_fork_sup:start_child_cast( ForkSup, ForkMessage ).

% fork_handle_call( ForkSup, ReplyTo, ForkRequest ) ->
% 	{ok, _Child} = gen_wp_fork_sup:start_child_call( ForkSup, ReplyTo, ForkRequest ).

