-ifndef(gen_worker_pool_spec_hrl).
-define(gen_worker_pool_spec_hrl).

-include("gen_wp_spec.hrl").

-spec init( Arg :: term() ) -> init_ret().
-spec handle_cast( Message :: term(), State :: mod_state() ) -> handle_cast_ret().
-spec handle_call( Request :: term(), ReplyTo :: term(), State :: mod_state() ) -> handle_call_ret().

-endif. % gen_worker_pool_spec_hrl
