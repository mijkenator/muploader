-module(muploader_collection_handler).

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
 	{<<"/collection/create">> ,_} -> 
		create_collection(Ret),
		<<"collection-create">>
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

    {ok, Req4} = reply(jiffy:encode({[{<<"type">>, Type}, {<<"status">>, <<"ok">>}, {<<"logo">>, LogoFileName}, {<<"poster">>, PosterFileName}]}), Req2),   
    lager:debug("PH HANDLE4: ~p", [Req4]),
    {ok, Req4, State}.

create_collection(Params) ->
%[{<<"mkh_save_file_name">>,<<"/tmp/20151213_165122_c_8.JPG">>},{<<"mkh_save_file_name">>,<<"/tmp/20151213_165122_c_9.JPG">>},{<<"collection[name]">>,<<"asdsd">>},{<<"collection[designer]">>,<<"sdfsd">>},{<<"collection[link]">>,<<"asdasd212">>},{<<"collection[smallestSize]">>,<<"02">>},{<<"collection[largestSize]">>,<<"06">>},{<<"collection[price]">>,<<"$250 - $500 ">>},{<<"collection[season]">>,<<"Fall">>},{<<"collection[year]">>,<<"2016">>}]
    [CN,D,L,SS,LS,P,Se,Y,Logo0,Poster0] = [proplists:get_value(X, Params, <<>>) || X <- [
        					  <<"collection[name]">>,
        					  <<"collection[designer]">>,
        					  <<"collection[link]">>,
          					  <<"collection[smallestSize]">>,<<"collection[largestSize]">>,
        					  <<"collection[price]">>,<<"collection[season]">>,
						  <<"collection[year]">>, <<"mkh_logo_file">>, <<"mkh_poster_file">>]],
    Logo = case Logo0 of
	<<"/opt/mwd_admin", R/binary>> -> R
	;_ -> Logo0
    end,
    Poster = case Poster0 of
	<<"/opt/mwd_admin", T/binary>> -> T
	;_ -> Poster0
    end,
    CcR = rpc:call('edapi@127.0.0.1', model_collection, create_collection, [CN,D,L,SS,LS,P,Se,Y,Logo,Poster]),
    lager:debug("CreateCollection Ret: ~p", [CcR]),
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

%handle(Req, State) ->
%    lager:debug("MCH HANDLE: ~p ~p", [Req, State]),
%
%    {ok, Headers, Req2} = cowboy_req:part(Req),
%    {file, FileInpName, Filename, ContentType, _TE} = cow_multipart:form_data(Headers),
%    lager:debug("Received file ~p (~p) of content-type ~p", [Filename, FileInpName, ContentType]),
%    SaveFileName = get_file_name(Filename, FileInpName),
%    lager:debug("temporary file name: ~p", [SaveFileName]),
%    save_file(Req2, SaveFileName),
%
%    Type = case cowboy_req:path(Req2) of
% 	{<<"/collection/create">> ,_} -> 
%		parse_req(Headers, Req2),	
%		<<"collection-create">>
%	;_ -> <<"unk_upload">>
%    end,
%
%    {ok, Req4} = reply(jiffy:encode({[{<<"type">>, Type}, {<<"status">>, <<"ok">>}, {<<"filename">>, SaveFileName}]}), Req2),   
%    lager:debug("PH HANDLE4: ~p", [Req4]),
%
%
%    {ok, Req4, State}.

save_file(Req, FileName) ->
    lager:debug("SAVEFILE1: ~p", [FileName]),
    {ok, IoDevice} = file:open(FileName, [write, binary]),
    lager:debug("SAVEFILE2", []),
    {ok, _, Req2}     = get_part_body(Req, IoDevice),
    lager:debug("SAVEFILE3", []),
    file:close(IoDevice),
    lager:debug("SAVEFILE DONE."),
    Req2.


-spec get_file_name(binary(), binary()) -> binary().
get_file_name(UploadFileName, InputName) ->
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


