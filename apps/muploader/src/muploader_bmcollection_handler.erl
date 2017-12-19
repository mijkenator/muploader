-module(muploader_bmcollection_handler).

-export([init/3]).
-export([handle/2, save_file/2]).
-export([terminate/3]).

init(_Type, Req, []) ->
	{ok, Req, undefined}.
handle(Req, State) ->
    lager:debug("MCH HANDLE: ~p ~p", [Req, State]),
    
    {Ret, Req2} = multipart(Req, []),
    lager:debug("MCH multipart ret: ~p", [Ret]),
    Type = case cowboy_req:path(Req) of
 	{<<"/bmcollection/create">>, _} -> 
		create_collection(Ret),
		<<"collection-create">>;
	{<<"/bmcollection/update">>, _} ->
		update_collection(Ret),
		<<"collection-update">>
	;_ -> <<"unk_upload">>
    end,

    LogoFileName = case proplists:get_value(<<"mkh_logo_file">>, Ret) of
	<<"/opt/mwd_admin", R/binary>> -> R
	;R -> R
    end,
    PosterFileName = case proplists:get_value(<<"mkh_poster_file">>, Ret) of
	<<"/opt/mwd_admin", R1/binary>> -> R1
	;R1 -> R1
    end,
    PosterLogoName = case proplists:get_value(<<"mkh_poster_logo_file">>, Ret) of
	<<"/opt/mwd_admin", R2/binary>> -> R2
	;R2 -> R2
    end,
    PosterSeasonName = case proplists:get_value(<<"mkh_poster_season_file">>, Ret) of
	<<"/opt/mwd_admin", R3/binary>> -> R3
	;R3 -> R3
    end,
    PosterCollectionName = case proplists:get_value(<<"mkh_poster_collection_file">>, Ret) of
	<<"/opt/mwd_admin", R4/binary>> -> R4
	;R4 -> R4
    end,

    {ok, Req4} = reply(jiffy:encode({[{<<"type">>, Type}, {<<"status">>, <<"ok">>}, 
		{<<"logo">>, 			LogoFileName}, 
		{<<"poster">>, 			PosterFileName},
		{<<"posterLogo">>, 		PosterLogoName},
		{<<"posterSeason">>, 		PosterSeasonName},
		{<<"posterCollection">>, 	PosterCollectionName}
	]}), Req2),   
    lager:debug("PH HANDLE4: ~p", [Req4]),
    {ok, Req4, State}.

update_collection(Params) ->
    [CID,CN,D,L,SS,LS,P,Se,Y,Logo0,Poster0,PLF0,PSF0,PCF0] = [proplists:get_value(X, Params, undefined) || X <- [
        					  <<"collection[id]">>,
        					  <<"collection[collectionName]">>,
        					  <<"collection[designer]">>,
        					  <<"collection[link]">>,
          					  <<"collection[sSize]">>,<<"collection[lSize]">>,
        					  <<"collection[acp]">>,<<"collection[season]">>,
						  <<"collection[year]">>, 
						  <<"mkh_logo_file">>, <<"mkh_poster_file">>,
						  <<"mkh_poster_logo_file">>,
						  <<"mkh_poster_season_file">>,
						  <<"mkh_poster_collection_file">>]],
    Logo = case Logo0 of
	<<"/opt/mwd_admin", R/binary>> -> R
	;_ -> Logo0
    end,
    Poster = case Poster0 of
	<<"/opt/mwd_admin", T/binary>> -> T
	;_ -> Poster0
    end,
    PLF = case PLF0 of
	<<"/opt/mwd_admin", Q/binary>> -> Q
	;_ -> PLF0
    end,
    PSF = case PSF0 of
	<<"/opt/mwd_admin", Q1/binary>> -> Q1
	;_ -> PSF0
    end,
    PCF = case PCF0 of
	<<"/opt/mwd_admin", Q2/binary>> -> Q2
	;_ -> PCF0
    end,
    lager:debug("PreCCR: ~p", [{CID,CN,D,L,SS,LS,P,Se,Y,Logo,Poster,PLF,PSF,PCF}]),
    CcR = rpc:call('edapi@127.0.0.1', model_bmcollection, update_collection, [CID,CN,D,L,SS,LS,P,Se,Y,Logo,Poster,PLF,PSF,PCF]),
    lager:debug("UpdateCollection Ret: ~p", [CcR]),
    ok.

create_collection(Params) ->
%[{<<"mkh_save_file_name">>,<<"/tmp/20151213_165122_c_8.JPG">>},{<<"mkh_save_file_name">>,<<"/tmp/20151213_165122_c_9.JPG">>},{<<"collection[name]">>,<<"asdsd">>},{<<"collection[designer]">>,<<"sdfsd">>},{<<"collection[link]">>,<<"asdasd212">>},{<<"collection[smallestSize]">>,<<"02">>},{<<"collection[largestSize]">>,<<"06">>},{<<"collection[price]">>,<<"$250 - $500 ">>},{<<"collection[season]">>,<<"Fall">>},{<<"collection[year]">>,<<"2016">>}]
    [CN,D,L,SS,LS,P,Se,Y,Logo0,Poster0,PLF0,PSF0,PCF0] = [proplists:get_value(X, Params, <<>>) || X <- [
        					  <<"collection[collectionName]">>,
        					  <<"collection[designer]">>,
        					  <<"collection[link]">>,
          					  <<"collection[sSize]">>,<<"collection[lSize]">>,
        					  <<"collection[acp]">>,<<"collection[season]">>,
						  <<"collection[year]">>, <<"mkh_logo_file">>, <<"mkh_poster_file">>,
						  <<"mkh_poster_logo_file">>,
						  <<"mkh_poster_season_file">>,
						  <<"mkh_poster_collection_file">>]],
    Logo = case Logo0 of
	<<"/opt/mwd_admin", R/binary>> -> R
	;_ -> Logo0
    end,
    Poster = case Poster0 of
	<<"/opt/mwd_admin", T/binary>> -> T
	;_ -> Poster0
    end,
    PLF = case PLF0 of
	<<"/opt/mwd_admin", Q/binary>> -> Q
	;_ -> PLF0
    end,
    PSF = case PSF0 of
	<<"/opt/mwd_admin", Q1/binary>> -> Q1
	;_ -> PSF0
    end,
    PCF = case PCF0 of
	<<"/opt/mwd_admin", Q2/binary>> -> Q2
	;_ -> PCF0
    end,
    CcR = rpc:call('edapi@127.0.0.1', model_bmcollection, create_collection, [CN,D,L,SS,LS,P,Se,Y,Logo,Poster,PLF,PSF,PCF]),
    lager:debug("CreateCollection Ret: ~p", [CcR]),
    ok.

multipart(Req, A) ->
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
			<<"collection[logo]">> ->
			    SaveFileName1 = get_file_name(Filename, FileInpName),
			    lager:debug("temporary file name: ~p", [SaveFileName1]),
			    Req3 = save_file(Req2, SaveFileName1),
			    {Req3 ,[{<<"mkh_logo_file">>, SaveFileName1}]};
			<<"collection[poster]">> ->
			    SaveFileName2 = get_file_name(Filename, FileInpName),
			    lager:debug("temporary file name: ~p", [SaveFileName2]),
			    Req3 = save_file(Req2, SaveFileName2),
			    {Req3 ,[{<<"mkh_poster_file">>, SaveFileName2}]};
			<<"collection[posterLogo]">> ->
			    SaveFileName3 = get_file_name(Filename, FileInpName),
			    lager:debug("temporary file name: ~p", [SaveFileName3]),
			    Req3 = save_file(Req2, SaveFileName3),
			    {Req3 ,[{<<"mkh_poster_logo_file">>, SaveFileName3}]};
			<<"collection[posterSeason]">> ->
			    SaveFileName4 = get_file_name(Filename, FileInpName),
			    lager:debug("temporary file name: ~p", [SaveFileName4]),
			    Req3 = save_file(Req2, SaveFileName4),
			    {Req3 ,[{<<"mkh_poster_season_file">>, SaveFileName4}]};
			<<"collection[posterCollection]">> ->
			    SaveFileName5 = get_file_name(Filename, FileInpName),
			    lager:debug("temporary file name: ~p", [SaveFileName5]),
			    Req3 = save_file(Req2, SaveFileName5),
			    {Req3 ,[{<<"mkh_poster_collection_file">>, SaveFileName5}]};
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
    UploadFileName = re:replace(UploadFileName0, "\\s", "", [global, {return, binary}]),
    %UploadFileName = muploader_utils:change_file_name(UploadFileName0),
    lager:debug("GFN0: ~p ~p", [UploadFileName, InputName]),
    TempDirectory = muploader_utils:get_tmp_dir(bm,InputName),
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



