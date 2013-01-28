-module(mijkweb_response).

-export([
    json_ok_response/1,
    json_error_response/1,
    json_error_response/2
]).

-spec json_error_response({integer(), binary()})   -> binary().
json_error_response({ErrorCode, ErrorDescription}) ->
    json_error_response({ErrorCode, ErrorDescription}, <<"unknown">>).

-spec json_error_response({integer(), binary()}, binary()) -> binary().
json_error_response({ErrorCode, ErrorDescription}, Type) ->
    jiffy:encode({[{<<"type">>, Type}, {<<"status">>, <<"error">>}, 
        {<<"errors">>, [[ErrorCode, ErrorDescription]]  }]}).
  
-spec json_ok_response(any()) -> binary().
json_ok_response(Response)    -> jiffy:encode(Response).

