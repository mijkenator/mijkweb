%% Types.
-type kv() :: {Name::binary(), Value::binary()}.
-type kvlist() :: [kv()].

-define(SESSION_AGE, 1200).
-define(SHARD_COUNT, 10).
-define(LOCK_AGE, 5). % lock age , seconds
-define(LOCK_SLEEP, 200). %lock sleep, millisec
-defined(LOCK_TIMEOUT, 1). %lock timeout, sec