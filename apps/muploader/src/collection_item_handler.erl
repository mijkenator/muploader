-module(collection_item_handler).

-export([init/3]).
-export([handle/2, save_file/2]).
-export([terminate/3]).

init(_Type, Req, []) ->
	{ok, Req, undefined}.
handle(Req, State) ->
    lager:debug("MCH HANDLE: ~p ~p", [Req, State]),

    {MKHCookie, Req1} = cowboy_req:cookie(<<"MIJKSSID">>, Req),
    lager:debug("CIH HANDLE MKHCookie: ~p", [MKHCookie]),
    {AccountID, IsAdmin} = try rpc:call('edapi@127.0.0.1', mijkweb_session, check_session_data, [ellimcd, MKHCookie]) of
	{ok, [_,_,SData]} -> {proplists:get_value(<<"accountid">>, SData, 0), proplists:get_value(<<"is_admin">>, SData, 0)}
	;_ -> {0,0}
    catch
	_:_ -> {0,0}
    end,
    lager:debug("CIH HANDLE AccountID: ~p", [{AccountID,IsAdmin}]),
    XParams = [{<<"mkh_account_id">>, AccountID}, {<<"mkh_account_isadmin">>, IsAdmin}], 
    
    {Ret, Req2} = multipart(Req1, []),
    lager:debug("MCH multipart ret: ~p", [Ret]),
    Type = case cowboy_req:path(Req) of
 	{<<"/item/collection/create">>, _} -> 
		create_collection_item(Ret++XParams),
		<<"collection-create-item">>;
	{<<"/item/collection/update">>, _} ->
		update_collection_item(Ret++XParams),
		<<"collection-update-item">>
	;_ -> <<"unk_upload">>
    end,

    FrontImg = case proplists:get_value(<<"mkh_front_img">>, Ret) of
	<<"/opt/mwd_admin", R/binary>> -> R
	;R -> R
    end,
    BackImg = case proplists:get_value(<<"mkh_back_img">>, Ret) of
	<<"/opt/mwd_admin", R1/binary>> -> R1
	;R1 -> R1
    end,

    {ok, Req4} = reply(jiffy:encode({[{<<"type">>, Type}, {<<"status">>, <<"ok">>}, 
		{<<"front_image">>, 			FrontImg}, 
		{<<"back_image">>, 			BackImg}
	]}), Req2),   
    lager:debug("PH HANDLE4: ~p", [Req4]),
    {ok, Req4, State}.

update_collection_item(Params) ->
    [ID,CID,Style,F,Color,Link,Sil,Pr,Hl,N,TL,WS,SL,IsD0,IT,IDN,CIDN,FI0,BI0,UI0,Desc,SCU] = [proplists:get_value(X, Params, undefined) || X <- [
        					  <<"dress[itemId]">>,
        					  <<"dress[collectionId]">>,
        					  <<"dress[style]">>,
        					  <<"dress[fabric]">>,
        					  <<"dress[color]">>,
          					  <<"dress[link]">>,<<"dress[silhouette]">>,
        					  <<"dress[price]">>,<<"dress[hemline]">>,
						  <<"dress[neckline]">>,
						  <<"dress[trainLength]">>,
						  <<"dress[waistStyle]">>,
						  <<"dress[sleeveLenght]">>,
						  <<"dress[isDeleted]">>,
						  <<"imageType">>,
						  <<"itemId">>,
						  <<"collectionId">>,
						  <<"mkh_front_img">>,
						  <<"mkh_back_img">>,
						  <<"mkh_unk_img">>,
						  <<"dress[description]">>,
						  <<"dress[scu]">>
						]],
	
    {RID, RCID, RFI, RBI} = case IT of
	undefined -> 
	    FI = case FI0 of
		<<"/opt/mwd_admin", R/binary>> -> R
		;_ -> FI0
	    end,
	    BI = case BI0 of
		<<"/opt/mwd_admin", T/binary>> -> T
		;_ -> BI0
	    end,
	    {ID, CID, FI, BI};
	<<"front">> -> 
	    FI = case UI0 of
		<<"/opt/mwd_admin", R/binary>> -> R
		;_ -> UI0
	    end,
	   {IDN, CIDN, FI, undefined};
	<<"back">>  ->
	    BI = case UI0 of
		<<"/opt/mwd_admin", R/binary>> -> R
		;_ -> UI0
	    end,
	   {IDN, CIDN, undefined, BI}
    end,

    IsD = case IsD0 of
	<<"1">> -> 1
	;_ 	-> 0
    end,
    AccountID = proplists:get_value(<<"mkh_account_id">>, Params, 0),  
    IsAdmin   = proplists:get_value(<<"mkh_account_isadmin">>, Params, 0),
    lager:debug("AID ~p, IsAdm ~p",[AccountID, IsAdmin]),
    case {AccountID, IsAdmin} of
	{_, 1} ->
	    lager:debug("PreUCI: ~p", [{RID,Style,F,Color,Link,Sil,Pr,Hl,N,TL,SL,WS,IsD,RFI,RBI,Desc,SCU}]),
	    CcR = rpc:call('edapi@127.0.0.1', model_collection, update_item, [RID,Style,F,Color,Link,Sil,Pr,Hl,N,TL,SL,WS,IsD,RFI,RBI,Desc,SCU]),
	    lager:debug("UpdateCollectionItem Ret: ~p", [CcR]);
	{Aid, _} when is_integer(Aid), Aid>0 ->
	    case rpc:call('edapi@127.0.0.1', model_collection, is_designer_item, [Aid, RID]) of
		true ->
    		    lager:debug("AID ~p, IsAdm ~p CID: ~p ID: ~p ACCESS GRANTED",[AccountID, IsAdmin, RCID, RID]),
		    lager:debug("PreUCI: ~p", [{RID,Style,F,Color,Link,Sil,Pr,Hl,N,TL,SL,WS,IsD,RFI,RBI,Desc,SCU}]),
		    CcR = rpc:call('edapi@127.0.0.1', model_collection, update_item, [RID,Style,F,Color,Link,Sil,Pr,Hl,N,TL,SL,WS,IsD,RFI,RBI,Desc,SCU]),
		    lager:debug("UpdateCollectionItem Ret: ~p", [CcR])
		;_   ->
    		    lager:debug("AID ~p, IsAdm ~p CID: ~p ACCESS DENIED",[AccountID, IsAdmin, RCID])
	    end;
	_ -> ok
    end,
    %lager:debug("PreUCI: ~p", [{RID,Style,F,Color,Link,Sil,Pr,Hl,N,TL,SL,WS,IsD,RFI,RBI,Desc,SCU}]),
    %CcR = rpc:call('edapi@127.0.0.1', model_collection, update_item, [RID,Style,F,Color,Link,Sil,Pr,Hl,N,TL,SL,WS,IsD,RFI,RBI,Desc,SCU]),
    %lager:debug("UpdateCollectionItem Ret: ~p", [CcR]),
    ok.

create_collection_item(Params) ->
    [CID0,Style,F,Color,Link,Sil,Pr,Hl,N,TL,WS,SL,FI0,BI0,Desc,SCU,CID1] = [proplists:get_value(X, Params, <<>>) || X <- [
        					  <<"dress[collectionId]">>,
        					  <<"dress[style]">>,
        					  <<"dress[fabric]">>,
        					  <<"dress[color]">>,
          					  <<"dress[link]">>,<<"dress[silhouette]">>,
        					  <<"dress[price]">>,<<"dress[hemline]">>,
						  <<"dress[neckline]">>,
						  <<"dress[trainLength]">>,
						  <<"dress[waistStyle]">>,
						  <<"dress[sleeveLength]">>,
						  <<"mkh_front_img">>,
						  <<"mkh_back_img">>,
						  <<"dress[description]">>,
						  <<"dress[scu]">>,
						  <<"collectionId">>
						]],
    CID = case CID0 of
	<<>> -> CID1
	;_   -> CID0
    end,
    FI = case FI0 of
	<<"/opt/mwd_admin", R/binary>> -> R
	;_ -> FI0
    end,
    BI = case BI0 of
	<<"/opt/mwd_admin", T/binary>> -> T
	;_ -> BI0
    end,

    AccountID = proplists:get_value(<<"mkh_account_id">>, Params, 0),  
    IsAdmin   = proplists:get_value(<<"mkh_account_isadmin">>, Params, 0),
    lager:debug("AID ~p, IsAdm ~p",[AccountID, IsAdmin]),

    case {AccountID, IsAdmin} of
	{_, 1} ->
	    lager:debug("CreateCollection Params:: ~p", [{CID,Style,F,Color,Link,Sil,Pr,Hl,N,TL,WS,SL,FI,BI,Desc,SCU}]),
	    CcR = rpc:call('edapi@127.0.0.1', model_collection, add_item, [CID,Style,F,Color,Link,Sil,Pr,Hl,N,TL,SL,WS,FI,BI,Desc,SCU]),
	    lager:debug("CreateCollection Ret: ~p", [CcR]);
	{Aid, _} when is_integer(Aid), Aid>0 ->
	    case rpc:call('edapi@127.0.0.1', model_collection, is_designer_collection, [Aid, CID]) of
		true ->
    		    lager:debug("AID ~p, IsAdm ~p CID: ~p ACCESS GRANTED",[AccountID, IsAdmin, CID]),
		    lager:debug("CreateCollection Params:: ~p", [{CID,Style,F,Color,Link,Sil,Pr,Hl,N,TL,WS,SL,FI,BI,Desc,SCU}]),
		    CcR = rpc:call('edapi@127.0.0.1', model_collection, add_item, [CID,Style,F,Color,Link,Sil,Pr,Hl,N,TL,SL,WS,FI,BI,Desc,SCU]),
		    lager:debug("CreateCollection Ret: ~p", [CcR])
		;_   ->
    		    lager:debug("AID ~p, IsAdm ~p CID: ~p ACCESS DENIED",[AccountID, IsAdmin, CID])
	    end;
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
		    lager:debug("mp FNL: ~p ~p", [FileInpName, Filename]),
		    case FileInpName of
			<<"dress[front]">> ->
			    SaveFileName1 = get_file_name(Filename, FileInpName),
			    lager:debug("temporary file name: ~p", [SaveFileName1]),
			    Req3 = save_file(Req2, SaveFileName1),
			    {Req3 ,[{<<"mkh_front_img">>, SaveFileName1}]};
			<<"dress[back]">> ->
			    SaveFileName2 = get_file_name(Filename, FileInpName),
			    lager:debug("temporary file name: ~p", [SaveFileName2]),
			    Req3 = save_file(Req2, SaveFileName2),
			    {Req3 ,[{<<"mkh_back_img">>, SaveFileName2}]};
			<<"file">> ->
			    SaveFileName3 = get_file_name(Filename, FileInpName),
			    lager:debug("temporary file name: ~p", [SaveFileName3]),
			    Req3 = save_file(Req2, SaveFileName3),
			    {Req3 ,[{<<"mkh_unk_img">>, SaveFileName3}]};
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
    muploader_utils:tinyfile(FileName),
    Req2.


-spec get_file_name(binary(), binary()) -> binary().
get_file_name(UploadFileName0, InputName) ->
    %UploadFileName = re:replace(UploadFileName0, "\\s", "", [global, {return, binary}]),
    UploadFileName = muploader_utils:change_file_name(UploadFileName0),
    lager:debug("GFN0: ~p ~p", [UploadFileName, InputName]),
    TempDirectory = muploader_utils:get_tmp_dir(InputName),
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

-spec preview_file(binary()) -> binary().
preview_file(FileName) ->
    RootName = filename:rootname(FileName),
    ExtName  = filename:extension(FileName), 
    lager:debug("PF: ~p ~p", [RootName, ExtName]),
    case muploader_utils:is_supported_image_format(ExtName) of
        true -> muploader_utils:create_preview(<<RootName/binary, "_mu_pr.png">>, <<RootName/binary,ExtName/binary>>), <<RootName/binary,"_mu_pr.png">>
        ;_   -> <<"muploader_error">>
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



