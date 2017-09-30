-module(mbd_ms_upload_handler).

-export([init/3]).
-export([handle/2, save_file/2]).
-export([terminate/3, get_file_name/2]).

init(_Type, Req, []) ->
	{ok, Req, undefined}.
handle(Req, State) ->
    lager:debug("MBDMSU HANDLE: ~p ~p", [Req, State]),

    {MKHCookie, Req1} = cowboy_req:cookie(<<"MIJKSSID">>, Req),
    lager:debug("MBDMSU HANDLE MKHCookie: ~p", [MKHCookie]),
    {AccountID, IsAdmin} = try rpc:call('edapi@127.0.0.1', mijkweb_session, check_session_data, [ellimcd, MKHCookie]) of
	    {ok, [_,_,SData]} -> {proplists:get_value(<<"accountid">>, SData, 0), proplists:get_value(<<"is_admin">>, SData, 0)}
	    ;_ -> {0,0}
    catch
	    _:_ -> {0,0}
    end,
    lager:debug("MBDMSU HANDLE AccountID: ~p", [{AccountID,IsAdmin}]),
    case rpc:call('edapi@127.0.0.1', model_service_user, get_mbd_ms_uploads_count, [AccountID]) of
        true ->
            {Ret, Req2} = multipart(Req1, [], AccountID),
            lager:debug("BCH multipart ret: ~p", [Ret]),
            XParams = [{<<"mkh_account_id">>, AccountID}, {<<"mkh_account_isadmin">>, IsAdmin}], 
            save_upload(Ret ++ XParams),

            Imgs = [R || {Key, <<"/opt/mybestday", R/binary>>}<- Ret, Key =:= <<"mkh_mbd_img">>],

            {ok, Req4} = reply(jiffy:encode({[{<<"type">>, <<"main_slider_upload">>}, {<<"status">>, <<"ok">>}, 
                {<<"images">>, 	Imgs}
            ]}), Req2),   
            lager:debug("PH HANDLE4: ~p", [Req4]),
            {ok, Req4, State};
        _ ->
            {ok, Req5} = reply(jiffy:encode({[{<<"type">>, <<"main_slider_upload">>}, {<<"status">>, <<"failed">>}, 
                {<<"images">>, 	<<"reached limit">>}
            ]}), Req1),   
            lager:debug("PH HANDLE4: ~p", [Req5]),
            {ok, Req5, State}
    end.

save_upload(Params) ->
    AccountID = proplists:get_value(<<"mkh_account_id">>, Params, 0),  
    IsAdmin   = proplists:get_value(<<"mkh_account_isadmin">>, Params, 0),

    case proplists:get_value(<<"pref_resolution">>, Params, <<>>) of
        <<>>    -> ok;
        PrefRes -> 
                lager:debug("MS_SPR :: ~p", [{AccountID, PrefRes}]),
                PRRet = rpc:call('edapi@127.0.0.1', model_service_user, set_pref_resolution, [AccountID, PrefRes]),
                lager:debug("MS_SPR ret :: ~p", [PRRet]),
                ok
    end,

    lager:debug("AID ~p, IsAdm ~p",[AccountID, IsAdmin]),
    Fn = fun({R, OrderNumber}) ->
        case {AccountID, IsAdmin} of
            {Aid, _} when is_integer(Aid), Aid>0 ->
                lager:debug("SAVE ms upload :: ~p", [{AccountID, R, <<"">>}]),
                CcR = rpc:call('edapi@127.0.0.1', model_service_user, save_ms_upload, [AccountID, R, <<>>, OrderNumber]),
                lager:debug("SAve ms upload Ret: ~p", [CcR]);
            _ -> ok
        end
    end,
    OrderNumber = proplists:get_value(<<"order_number">>, Params, 0),
    lists:foreach(Fn, [{R,OrderNumber} || {Key, <<"/opt/mybestday", R/binary>>}<- Params, Key =:= <<"mkh_mbd_img">>]),
    ok.



multipart(Req, A, AccountID) ->
    UID = case AccountID of
        AID when is_integer(AID),AID>0 -> list_to_binary(integer_to_list(AID)++"/")
        ;_ -> <<"">>
    end,
    case cowboy_req:part(Req) of
        {ok, Headers, Req2} ->
            {Req4,AP} = case cow_multipart:form_data(Headers) of
                {data, FieldName} ->
                    {ok, Body, Req3} = cowboy_req:part_body(Req2),
                    {Req3, [{FieldName, Body}]};
                {file, FileInpName, Filename, _CType, _CTransferEncoding} ->
		    lager:debug("mp BCH FNL: ~p ~p", [FileInpName, Filename]),
		    case FileInpName of
                <<"file",_/binary>> ->
                    SaveFileName1 = get_file_name(<<UID/binary,Filename/binary>>, FileInpName),
                    lager:debug("temporary file name: ~p", [SaveFileName1]),
                    Req3 = save_file(Req2, SaveFileName1),
                    {Req3 ,[{<<"mkh_mbd_img">>, SaveFileName1}]};
                _ -> 
                    SaveFileName3 = get_file_name(<<UID/binary,Filename/binary>>, FileInpName),
                    lager:debug("temporary file name: ~p", [SaveFileName3]),
                    Req3 = save_file(Req2, SaveFileName3),
                    {Req3 ,[]}
                end
            end,
            multipart(Req4, A++AP, AccountID);
        {done, Req2} -> {A, Req2}
    end.


save_file(Req, FileName) ->
    lager:debug("SAVEFILE1: ~p", [FileName]),
    D1 = filename:dirname(FileName),
    D2 = <<D1/binary, "/p_i">>,
    file:make_dir(D1),
    file:make_dir(D2),
    file:change_mode(D1, 8#777),
    file:change_mode(D2, 8#777),
    {ok, IoDevice} = file:open(FileName, [write, binary]),
    lager:debug("SAVEFILE2", []),
    {ok, _, Req2}     = get_part_body(Req, IoDevice),
    lager:debug("SAVEFILE3", []),
    file:close(IoDevice),
    lager:debug("SAVEFILE DONE."),
    muploader_utils:tinyfile(FileName),
    Req2.

-spec get_file_name(binary(), binary()) -> binary().
get_file_name(UploadFileName0, InputName) ->
    UploadFileName = re:replace(UploadFileName0, "\\s", "", [global, {return, binary}]),
    lager:debug("GFN0: ~p ~p", [UploadFileName, InputName]),
    TempDirectory = muploader_utils:get_tmp_dir(mbdms, InputName),
    lager:debug("GFN: ~p ~p", [InputName, TempDirectory]),
    TN1 = <<TempDirectory/binary, UploadFileName/binary>>,
    case filelib:is_regular(TN1) of
        true  -> get_file_name(modify_ufn(UploadFileName), InputName);
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

%error_response(Error, Reason, Req) ->
%    lager:error("PH RP error: ~p ~p", [Error, Reason]),
%    reply(jiffy:encode({[{<<"type">>, <<"unknown">>}, {<<"status">>, <<"failed">>}, {<<"error">>, [666, <<"infernal server error">>]}]}), Req).




