-ifndef(gen_worker_pool_spec_hrl).
-define(gen_worker_pool_spec_hrl, included).

-include("gen_wp_types.hrl").

-spec init( Arg :: term() ) ->init_ret().
-spec handle_cast( Message :: term(), State :: mod_state() ) -> handle_cast_ret().
-spec handle_call( Request :: term(), ReplyTo :: term(), State :: mod_state() ) -> handle_call_ret().
-spec handle_info( Message :: term(), State :: mod_state() ) -> handle_info_ret().
-spec terminate( Reason :: stop_reason(), State :: mod_state() ) -> terminate_ret().
-spec code_change( OldVsn :: term(), State :: mod_state(), Extra :: term() ) -> code_change_ret().
-spec handle_fork_cast( Arg :: term(), Msg :: term(), WP :: pid() ) -> handle_fork_cast_ret().
-spec handle_fork_call( Arg :: term(), Req :: term(), ReplyTo :: term(), WP :: pid() ) -> handle_fork_call_ret().
-spec handle_child_forked( Task :: term(), Child :: pid(), ModState :: mod_state() ) -> handle_child_forked_ret().
-spec handle_child_terminated( StopReason :: term(), Task :: term(), Child :: pid(), ModState :: mod_state() ) -> handle_child_terminated_ret().

-endif. % gen_worker_pool_spec_hrl