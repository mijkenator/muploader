-module(mbd_mobile_upload_handler).

-export([init/3]).
-export([handle/2, save_file/2]).
-export([terminate/3]).

init(_Type, Req, []) ->
	{ok, Req, undefined}.
handle(Req, State) ->
    lager:debug("MBDMUH HANDLE: ~p ~p", [Req, State]),

    {MKHCookie, Req1} = cowboy_req:cookie(<<"MBDMUA">>, Req),
    lager:debug("MBDMUH HANDLE MBDMUACookie: ~p", [MKHCookie]),

    {ok, AccountID, Pid, Eid} = rpc:call('edapi@127.0.0.1', model_event, check_mobile_upload, [MKHCookie]),

    lager:debug("MBDMUH HANDLE AccountID: ~p", [{AccountID,Pid,Eid}]),
    
    {Ret, Req2} = multipart(Req1, []),
    lager:debug("MBDMUH multipart ret: ~p", [Ret]),
    Type = case cowboy_req:path(Req) of
        {<<"/mbd/mobile/upload/new">>, _} -> <<"new">>
        ;_ -> <<"edit">>
    end,
    XParams = [{<<"mkh_account_id">>, AccountID}, {<<"pid">>, Pid}, {<<"eid">>, Eid}, {<<"upload_type">>, Type}], 
    save_upload(Ret ++ XParams),

    Img = case proplists:get_value(<<"mkh_mbd_img">>, Ret) of
	<<"/opt/mybestday", R/binary>> -> R
	;R -> R
    end,

    {ok, Req4} = reply(jiffy:encode({[{<<"type">>, Type}, {<<"status">>, <<"ok">>}, 
		{<<"image">>, 	Img}
	]}), Req2),   
    lager:debug("PH HANDLE4: ~p", [Req4]),
    {ok, Req4, State}.

save_upload(Params) ->
    [Img] = [proplists:get_value(X, Params, <<>>) || X <- [<<"mkh_mbd_img">>]],
    FI = case Img of
	    <<"/opt/mybestday", R/binary>> -> R
	    ;_ -> Img
    end,

    AccountID = proplists:get_value(<<"mkh_account_id">>, Params, 0),  
    Type      = proplists:get_value(<<"upload_type">>, Params, 0),
    Caption   = proplists:get_value(<<"caption">>, Params, 0),
    Pid       = proplists:get_value(<<"pid">>, Params, 0),
    Eid       = proplists:get_value(<<"eid">>, Params, 0),
    Ordnum    = proplists:get_value(<<"order_number">>, Params, 0),
    lager:debug("MBDMUH SU ~p",[{AccountID, FI, Type, Caption, Pid, Eid}]),
    
    case {AccountID, Type} of
        {Aid, _} when is_integer(Aid), Aid>0 ->
            lager:debug("SAVE mbd mobile post upload :: ~p", [{AccountID, FI, Type, Caption, Eid, Pid}]),
            CcR = rpc:call('edapi@127.0.0.1', model_event, upload_post, [AccountID, FI, Type, Caption, Eid, Pid, Ordnum]),
            lager:debug("SAve mbd mobile post upload Ret: ~p", [CcR]);
        _ -> ok
    end,
    ok.



multipart(Req, A) ->
    case cowboy_req:part(Req) of
        {ok, Headers, Req2} ->
            {Req4,AP} = case cow_multipart:form_data(Headers) of
                {data, FieldName} ->
                    {ok, Body, Req3} = cowboy_req:part_body(Req2),
                    {Req3, [{FieldName, Body}]};
                {file, FileInpName, Filename, _CType, _CTransferEncoding} ->
		    lager:debug("mp BCH FNL: ~p ~p", [FileInpName, Filename]),
		    case FileInpName of
			 FIN when FIN=:=<<"mbd_upload">>;FIN=:=<<"file">> ->
			    SaveFileName1 = get_file_name(Filename, FileInpName),
			    lager:debug("temporary file name: ~p", [SaveFileName1]),
			    Req3 = save_file(Req2, SaveFileName1),
			    {Req3 ,[{<<"mkh_mbd_img">>, SaveFileName1}]};
			_ -> 
			    SaveFileName3 = get_file_name(Filename, FileInpName),
			    lager:debug("temporary file name: ~p", [SaveFileName3]),
			    Req3 = save_file(Req2, SaveFileName3),
			    {Req3 ,[]}
		    end
            end,
            multipart(Req4, A++AP);
        {done, Req2} -> {A, Req2}
    end.


save_file(Req, FileName) ->
    lager:debug("SAVEFILE1: ~p", [FileName]),
    {ok, IoDevice} = file:open(FileName, [write, binary]),
    lager:debug("SAVEFILE2", []),
    {ok, _, Req2}     = get_part_body(Req, IoDevice),
    lager:debug("SAVEFILE3", []),
    file:close(IoDevice),
    lager:debug("SAVEFILE DONE."),
    %muploader_utils:tinyfile(FileName),
    Req2.

-spec get_file_name(binary(), binary()) -> binary().
get_file_name(UploadFileName0, InputName) ->
    UploadFileName = re:replace(UploadFileName0, "\\s", "", [global, {return, binary}]),
    lager:debug("GFN0: ~p ~p", [UploadFileName, InputName]),
    TempDirectory = muploader_utils:get_tmp_dir(mbdpost, InputName),
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





