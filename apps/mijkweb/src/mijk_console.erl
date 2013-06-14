-module(mijk_console).

-export([
    h/0,
    clear/0,
    console_log_level/1,
    console_add_trace/2,
    cld/0, clw/0, cle/0
]).

clear() -> lager:clear_all_traces().

cld() -> lager:set_loglevel(lager_console_backend, debug).
clw() -> lager:set_loglevel(lager_console_backend, warning).
cle() -> lager:set_loglevel(lager_console_backend, error).
console_log_level(Level) -> lager:set_loglevel(lager_console_backend, Level).

console_add_trace(Module, LLevel) -> lager:trace_console([{module, Module}], LLevel). 

% lager:trace_file("log/error.log", [{module, mymodule}, warning])
% lager:trace_file("logs/mkh.error", [{module, mijk_statist},{module, mijk_statist_worker}], error).%
% lager:trace_file("/home/ashim/work/mijkweb/rel/mijkweb/log/mkh.log", [{module, mijk_statist}], debug).

h() -> io:format("mijk_console interactive help: ~n").
