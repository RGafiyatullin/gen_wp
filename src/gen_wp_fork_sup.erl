-module(gen_wp_fork_sup).

-behaviour(supervisor).

%% API
-export([
	start_link/1,
	start_child/2
]).

%% supervisor callbacks
-export([ init/1 ]).

%% ===================================================================
%% API
%% ===================================================================

-spec start_link( WP :: pid() ) -> { ok, pid() }.
start_link( WP ) ->
    supervisor:start_link( ?MODULE, { WP } ).

start_child( Sup, ForkMessage ) ->
	supervisor:start_child( Sup, [ ForkMessage ] ).

%% ===================================================================
%% supervisor callbacks
%% ===================================================================

init( { WP } ) ->
	{ok, {
		{ simple_one_for_one, 1, 1 }, [
			{ fork_worker,
				{ gen_wp_forked, start_link, [ WP ] }, temporary, brutal_kill, worker, [ gen_wp_forked ]}
		]}
	}.
