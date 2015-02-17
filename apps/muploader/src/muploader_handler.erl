-module(muploader_handler).

-export([init/3]).
-export([handle/2, save_file/2]).
-export([terminate/3]).

init(_Type, Req, []) ->
	{ok, Req, undefined}.

handle(Req, State) ->
    lager:debug("PH HANDLE: ~p ~p", [Req, State]),

    {ok, Headers, Req2} = cowboy_req:part(Req),
    {file, FileInpName, Filename, ContentType, _TE} = cow_multipart:form_data(Headers),
    lager:debug("Received file ~p (~p) of content-type ~p", [Filename, FileInpName, ContentType]),
    SaveFileName = get_file_name(Filename, FileInpName),
    lager:debug("temporary file name: ~p", [SaveFileName]),
    save_file(Req2, SaveFileName),
    %Pid = spawn(?MODULE, save_file, [Req2, SaveFileName]),
    %lager:debug("spawned: ~p", [Pid]),

    {ok, Req4} = reply(jiffy:encode({[{<<"type">>, <<"upload">>}, {<<"status">>, <<"ok">>}, {<<"filename">>, SaveFileName}]}), Req2),   
    lager:debug("PH HANDLE4: ~p", [Req4]),


    {ok, Req4, State}.

save_file(Req, FileName) ->
    lager:debug("SAVEFILE1", []),
    {ok, IoDevice} = file:open(FileName, [write, binary]),
    lager:debug("SAVEFILE2", []),
    {ok, _, _}     = get_part_body(Req, IoDevice),
    lager:debug("SAVEFILE3", []),
    file:close(IoDevice),
    lager:debug("SAVEFILE DONE.").


-spec get_file_name(binary(), binary()) -> binary().
get_file_name(UploadFileName, _InputName) ->
    TempDirectory = <<"/tmp/">>,
    TN1 = <<TempDirectory/binary, UploadFileName/binary>>,
    case filelib:is_regular(TN1) of
        true  -> get_file_name(modify_ufn(UploadFileName), _InputName);
        false -> TN1
    end.

-spec modify_ufn(binary()) -> binary().
modify_ufn(UploadFileName) ->
    RN  = filename:rootname(UploadFileName),
    RN1 = case re:run(RN, "_c_(\\d+)$", []) of
        {match,[_,{B,L}]} ->
              L1 = B - 3,
              <<H:L1/binary,"_c_",N:L/binary,_/binary>> = RN,
              Extra = integer_to_binary(binary_to_integer(N) + 1),
              <<H/binary, "_c_", Extra/binary>>
        ;_ -> <<RN/binary, "_c_1">>
    end,
    EX = filename:extension(UploadFileName),
    <<RN1/binary, EX/binary>>.

get_part_body(Req, IoDevice) ->
    lager:debug("GPB PH HANDLE1: ", []),
    case cowboy_req:part_body(Req) of
        {ok, D, R }  -> 
            ok = file:write(IoDevice, D),
            {ok, D, R};
        {more, A, R} -> 
            lager:debug("more", []),
            ok = file:write(IoDevice, A),
            get_part_body(R, IoDevice);
        M ->
            lager:debug("LALALALALA ~p", [M]), 
            M
    end.

reply(Response, Req) ->
	cowboy_req:reply(200, [
		{<<"content-type">>, <<"text/plain; charset=utf-8">>}
	], Response, Req).

terminate(Reason, _Req, _State) ->
    lager:debug("PH TERMINATE: ~p", [Reason]),
	ok.

error_response(Error, Reason, Req) ->
    lager:error("PH RP error: ~p ~p", [Error, Reason]),
    reply(jiffy:encode({[{<<"type">>, <<"unknown">>}, {<<"status">>, <<"failed">>}, {<<"error">>, [666, <<"infernal server error">>]}]}), Req).


