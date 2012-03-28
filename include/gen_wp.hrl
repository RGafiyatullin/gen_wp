-ifndef(gen_wp_hrl).
-define(gen_wp_hrl, included).

-record(gwp_ctx, {
	pending_tasks = queue:new() :: queue(),
	ref_to_pid  = dict:new() :: dict(),
	max_pool_size = infinity :: infinity | integer()
	}).

-endif. % gen_wp_hrl
