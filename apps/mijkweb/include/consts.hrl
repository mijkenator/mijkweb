%% Types.
-type kv() :: {Name::binary(), Value::binary()}.
-type kvlist() :: [kv()].

-define(SESSION_AGE, 1200).