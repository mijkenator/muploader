-module(muploader_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    Dispatch = cowboy_router:compile([
        {'_', [
            {"/muploader/uploader/[...]", muploader_handler, []}
            ,{"/muploader/api/[...]", muploader_api_handler, []}
            ,{"/[...]", cowboy_static, {dir, "../../priv_dir/html", [{mimetypes, cow_mimetypes, all}]}}
        ]}
    ]),
    {ok, _} = cowboy:start_http(http, 50, [{port, 3035}], [
        {env, [{dispatch, Dispatch}]}
    ]),
    ex_reloader:start(),
    muploader_sup:start_link().

stop(_State) ->
    ok.
