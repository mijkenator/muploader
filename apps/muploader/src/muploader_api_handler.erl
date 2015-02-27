-module(muploader_api_handler).

-export([init/3]).
-export([handle/2]).
-export([terminate/3]).

init(_Type, Req, []) ->
	{ok, Req, undefined}.

handle(Req, State) ->
    lager:debug("MAH HANDLE: ~p ~p", [Req, State]),
	{Method, Req2} = cowboy_req:method(Req),
    lager:debug("MAH HANDLE1: ~p", [Method]),
	HasBody = cowboy_req:has_body(Req2),
    lager:debug("MAH HANDLE2: ~p", [HasBody]),
	{ok, Req3} = request_processor(Method, HasBody, Req2),
	{ok, Req3, State}.

request_processor(<<"POST">>, true, Req) ->
    try
        lager:debug("MAH RP1-0", []),
        {ok, PostVals, Req2} = cowboy_req:body_qs(Req),
        lager:debug("MAH RP1-1 ~p", [PostVals]),
        Request = proplists:get_value(<<"request">>, PostVals),
        {JSON} = jiffy:decode(Request),
        lager:debug("MAH RP1-2 ~p", [JSON]),
        Command = proplists:get_value(<<"type">>, JSON),
        lager:debug("MAH RP1-3 ~p", [Command]),
        reply(command_apply(Command, JSON), Req2)
    catch
        Error:Reason -> error_response(Error, Reason, Req)
    end;
request_processor(<<"POST">>, false, Req) ->
	cowboy_req:reply(400, [], <<"Missing body.">>, Req);
request_processor(_, _, Req) ->
	cowboy_req:reply(405, Req).%% Method not allowed.

reply(Response, Req) ->
	cowboy_req:reply(200, [
		{<<"content-type">>, <<"text/plain; charset=utf-8">>}
	], Response, Req).

error_response(Error, Reason, Req) ->
    lager:error("MAH RP error: ~p ~p", [Error, Reason]),
    reply(jiffy:encode({[{<<"type">>, <<"unknown">>}, {<<"status">>, <<"failed">>}, {<<"error">>, [666, <<"infernal server error">>]}]}), Req).

terminate(Reason, _Req, _State) ->
    lager:debug("MAH TERMINATE: ~p", [Reason]),
	ok.

command_apply(<<"remove_file">> = Type, JSON) ->
    FileName = proplists:get_value(<<"fn">>, JSON),
    case check_filename(FileName) of
        true ->
           ok = file:delete(FileName),
           jiffy:encode({[{<<"type">>, Type}, {<<"status">>, <<"ok">>}]});   
        _ -> throw({error, badfn, Type})
    end;
command_apply(_,_) -> <<"unknown command">>. 


-spec check_filename(binary()) -> true | false.
check_filename(FileName) ->
   TempDirectory = muploader_utils:get_tmp_dir(),
   L = size(TempDirectory) - 1,
   <<T:L/binary, "/">> = TempDirectory,
   case filename:dirname(FileName) of
        T  -> true
        ;_ -> false
   end.
