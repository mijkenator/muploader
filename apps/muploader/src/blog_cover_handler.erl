-module(blog_cover_handler).

-export([init/3]).
-export([handle/2, save_file/2]).
-export([terminate/3]).

init(_Type, Req, []) ->
	{ok, Req, undefined}.
handle(Req, State) ->
    lager:debug("BCH HANDLE: ~p ~p", [Req, State]),

    {MKHCookie, Req1} = cowboy_req:cookie(<<"MIJKSSID">>, Req),
    lager:debug("BCH HANDLE MKHCookie: ~p", [MKHCookie]),
    {AccountID, IsAdmin} = try rpc:call('edapi@127.0.0.1', mijkweb_session, check_session_data, [ellimcd, MKHCookie]) of
	    {ok, [_,_,SData]} -> {proplists:get_value(<<"accountid">>, SData, 0), proplists:get_value(<<"is_admin">>, SData, 0)}
	    ;_ -> {0,0}
    catch
	    _:_ -> {0,0}
    end,
    lager:debug("BCH HANDLE AccountID: ~p", [{AccountID,IsAdmin}]),
    XParams = [{<<"mkh_account_id">>, AccountID}, {<<"mkh_account_isadmin">>, IsAdmin}], 
    
    {Ret, Req2} = multipart(Req1, [], AccountID),
    lager:debug("BCH multipart ret: ~p", [Ret]),
    Type = case cowboy_req:path(Req) of
        {<<"/blog/cover/create">>, _} -> 
            create_track(Ret++XParams),
            <<"blog-cover-create">>;
        {<<"/blog/cover/upload">>, _} ->
            create_track(Ret++XParams),
            <<"blog-cover-update">>
        ;_ -> <<"unk_upload">>
    end,

    Img = case proplists:get_value(<<"mkh_image">>, Ret) of
	<<"/opt/mybestday", R/binary>> -> R
	;R -> R
    end,

    {ok, Req4} = reply(jiffy:encode({[{<<"type">>, Type}, {<<"status">>, <<"ok">>}, 
		{<<"image">>, 	Img}
	]}), Req2),   
    lager:debug("PH HANDLE4: ~p", [Req4]),
    {ok, Req4, State}.

create_track(Params) ->
    [Img, Id] = [proplists:get_value(X, Params, <<>>) || X <- [<<"mkh_image">>, <<"id">>]],
    FI = case Img of
	    <<"/opt/mybestday", R/binary>> -> R
	    ;_ -> Img
    end,

    AccountID = proplists:get_value(<<"mkh_account_id">>, Params, 0),  
    IsAdmin   = proplists:get_value(<<"mkh_account_isadmin">>, Params, 0),
    lager:debug("AID ~p, IsAdm ~p",[AccountID, IsAdmin]),
    
    case {AccountID, IsAdmin} of
        {Aid, _} when is_integer(Aid), Aid>0 ->
            lager:debug("User CreateTrack:: ~p", [{FI, Id}]),
            CcR = rpc:call('edapi@127.0.0.1', model_mbd_blog, blog_cover, [FI, Id, AccountID]),
            lager:debug("User CreateTrack Ret: ~p", [CcR]);
        _ -> ok
    end,
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
                {file, FileInpName, Filename0, _CType, _CTransferEncoding} ->
            Filename = muploader_utils:change_file_name(Filename0),
		    lager:debug("mp BCH FNL ----> : ~p ~p ~p", [FileInpName, Filename, Filename0]),
		    case FileInpName of
			 FIN when FIN=:=<<"image">>;FIN=:=<<"cover">> ->
			    %SaveFileName1 = get_file_name(Filename, FileInpName),
                SaveFileName1 = get_file_name(<<UID/binary,Filename/binary>>, FileInpName),
			    lager:debug("temporary file name: ~p", [SaveFileName1]),
			    Req3 = save_file(Req2, SaveFileName1),
			    {Req3 ,[{<<"mkh_image">>, SaveFileName1}]};
			_ -> 
			    %SaveFileName3 = get_file_name(Filename, FileInpName),
                SaveFileName3 = get_file_name(<<UID/binary,Filename/binary>>, FileInpName),
			    lager:debug("temporary file name: ~p", [SaveFileName3]),
			    Req3 = save_file(Req2, SaveFileName3),
			    {Req3 ,[]}
		    end
            end,
            multipart(Req4, A++AP, AccountID);
        {done, Req2} -> {A, Req2}
    end.


%save_file(Req, FileName) ->
%    lager:debug("SAVEFILE1: ~p", [FileName]),
%    {ok, IoDevice} = file:open(FileName, [write, binary]),
%    lager:debug("SAVEFILE2", []),
%    {ok, _, Req2}     = get_part_body(Req, IoDevice),
%    lager:debug("SAVEFILE3", []),
%    file:close(IoDevice),
%    lager:debug("SAVEFILE DONE."),
%    %muploader_utils:tinyfile(FileName),
%    Req2.
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
    %muploader_utils:tinyfile(FileName),
    Req2.

-spec get_file_name(binary(), binary()) -> binary().
get_file_name(UploadFileName0, InputName) ->
    UploadFileName = re:replace(UploadFileName0, "\\s", "", [global, {return, binary}]),
    %UploadFileName = muploader_utils:change_file_name(UploadFileName0),
    lager:debug("GFN0: ~p ~p", [UploadFileName, InputName]),
    TempDirectory = muploader_utils:get_tmp_dir(blog_cover, InputName),
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


