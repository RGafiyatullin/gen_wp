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
	{ fork, Message :: term(), mod_state() } |
	{ fork, Message :: term(), mod_state(), timeout() }.

-type handle_call_ret() ::
	{ noreply, mod_state() } |
	{ noreply, mod_state(), timeout() } |
	{ reply, ReplyWith :: term(), mod_state() } |
	{ reply, ReplyWith :: term(), mod_state(), timeout() } |
	{ stop, stop_reason(), mod_state() } |
	{ stop, stop_reason(), ReplyWith :: term(), mod_state() } |
	{ fork, Request :: term(), mod_state() } |
	{ fork, Request :: term(), mod_state(), timeout() }.

-type handle_info_ret() ::
	{ noreply, mod_state() } |
	{ noreply, mod_state(), timeout() } |
	{ noreply, mod_state(), hibernate } |
	{ stop, stop_reason(), mod_state() }.

-type terminate_ret() :: 
	ignore.

-type code_change_ret() ::
	{ ok, NewState :: mod_state() } |
	{ error, Reason :: term()}.

-type handle_fork_cast_ret() ::
	{ noreply, Result :: term() }.

-type handle_fork_call_ret() ::
	{ reply, ReplyWith :: term(), Result :: term() } |
	{ noreply, Result :: term() }.

-type handle_child_forked_ret() ::
	ok.

-type handle_child_terminated_ret() ::
	ok.

-endif. % gen_wp_types_hrl
