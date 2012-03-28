-ifndef(gen_wp_types_hrl).
-define(gen_wp_types_hrl, included).

-type module_arg() :: term().
-type module_spec() :: atom() | {atom(), [any()]}.

-type mod_state() :: term().
-type stop_reason() :: term().
-type init_ret() ::
	{ ok, mod_state() } |
	{ ok, mod_state(), timeout() } |
	{ stop, stop_reason() }.

-type handle_cast_ret() ::
	{ noreply, mod_state() } |
	{ noreply, mod_state(), timeout() } |
	{ stop, stop_reason(), mod_state() } |
	{ fork, Message :: term(), mod_state() }.

-type handle_call_ret() :: 'describe me!!!!'().


-endif. % gen_wp_types_hrl
