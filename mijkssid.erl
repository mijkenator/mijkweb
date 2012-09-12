-module(mijkssid).
-export([get_cookie/1]).

get_cookie({Pid, DynData}) ->
    file:write_file("/tmp/tslog", io_lib:fwrite("~p ~p ~p ~n", [now(), Pid, DynData]), [append]),
    "AASSAASSASASASASAS".