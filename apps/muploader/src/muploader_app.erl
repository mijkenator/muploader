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
            {"/muploader/uploader/[...]"    , muploader_handler, []}
            ,{"/muploader/api/[...]"        , muploader_api_handler, []}
            ,{"/collection/[...]"           , muploader_collection_handler, []}
            ,{"/item/collection/[...]"      , collection_item_handler, []}
            ,{"/bmcollection/[...]"         , muploader_bmcollection_handler, []}
            ,{"/item/bmcollection/[...]"    , bmcollection_item_handler, []}
            ,{"/blog/category/[...]"        , blog_category_handler, []}
            ,{"/blog/post/[...]"            , blog_post_handler, []}
            ,{"/mbd/upload/[...]"           , mbd_upload_handler, []}
            ,{"/mbd/ms/upload/[...]"        , mbd_ms_upload_handler, []}
            ,{"/mbd/bg/upload/[...]"        , mbd_bg_upload_handler, []}
            ,{"/mbd/post/upload/[...]"      , mbd_post_upload_handler, []}
            ,{"/mbd/mobile/upload/[...]"    , mbd_mobile_upload_handler, []}
            ,{"/user/img/[...]"             , user_handler, []}
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
