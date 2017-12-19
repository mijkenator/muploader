-module(blog_category_handler).

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
    
    {Ret, Req2} = multipart(Req1, []),
    lager:debug("BCH multipart ret: ~p", [Ret]),
    Type = case cowboy_req:path(Req) of
        {<<"/blog/category/create">>, _} -> 
            create_blog_category(Ret++XParams),
            <<"blog-category-create">>;
        {<<"/blog/category/update">>, _} ->
            update_blog_category(Ret++XParams),
            <<"blog-category-update">>
        ;_ -> <<"unk_upload">>
    end,

    Img = case proplists:get_value(<<"mkh_blog_category_image">>, Ret) of
	<<"/opt/mwd_admin", R/binary>> -> R
	;R -> R
    end,

    {ok, Req4} = reply(jiffy:encode({[{<<"type">>, Type}, {<<"status">>, <<"ok">>}, 
		{<<"image">>, 	Img}
	]}), Req2),   
    lager:debug("PH HANDLE4: ~p", [Req4]),
    {ok, Req4, State}.

create_blog_category(Params) ->
    [Img, Title] = [proplists:get_value(X, Params, <<>>) || X <- [
        <<"mkh_blog_category_image">>, <<"category[title]">>]],
    FI = case Img of
	    <<"/opt/mwd_admin", R/binary>> -> R
	    ;_ -> Img
    end,

    AccountID = proplists:get_value(<<"mkh_account_id">>, Params, 0),  
    IsAdmin   = proplists:get_value(<<"mkh_account_isadmin">>, Params, 0),
    lager:debug("AID ~p, IsAdm ~p",[AccountID, IsAdmin]),
    
    case {AccountID, IsAdmin} of
        {_, 1} ->
            lager:debug("CreateBlogCategory:: ~p", [{FI, Title}]),
            CcR = rpc:call('edapi@127.0.0.1', model_blog, create_blog_category, [Title, FI]),
            lager:debug("CreateBlogCategory Ret: ~p", [CcR]);
        {Aid, _} when is_integer(Aid), Aid>0 ->
            lager:debug("AID ~p, IsAdm ~p CID: ~p CREATE BLOG CATEGORY ACCESS DENIED",[AccountID, IsAdmin]);
        _ -> ok
    end,
    ok.


update_blog_category(Params) ->
    [Img, Title, IsDeleted, Cid] = [proplists:get_value(X, Params, undefined) || X <- 
        [<<"mkh_blog_category_image">>, <<"category[title]">>, <<"category[is_deleted]">>, <<"id">>]],

    FI = case Img of
	    <<"/opt/mwd_admin", R/binary>> -> R
	    ;_ -> Img
    end,
    IsD = case IsDeleted of
        <<>> -> 0
        ;_   -> IsDeleted
    end,

    AccountID = proplists:get_value(<<"mkh_account_id">>, Params, 0),  
    IsAdmin   = proplists:get_value(<<"mkh_account_isadmin">>, Params, 0),
    lager:debug("AID ~p, IsAdm ~p",[AccountID, IsAdmin]),
    
    case {AccountID, IsAdmin} of
        {_, 1} ->
            lager:debug("UpdateBlogCategory:: ~p", [{FI, Title, IsD}]),
            CcR = rpc:call('edapi@127.0.0.1', model_blog, update_blog_category, [Cid, Title, FI, IsD]),
            lager:debug("UpdateBlogCategory Ret: ~p", [CcR]);
        {Aid, _} when is_integer(Aid), Aid>0 ->
            lager:debug("AID ~p, IsAdm ~p CID: ~p Update BLOG CATEGORY ACCESS DENIED",[AccountID, IsAdmin]);
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
                {file, FileInpName, Filename0, _CType, _CTransferEncoding} ->
            Filename = muploader_utils:change_file_name(Filename0),
		    lager:debug("mp BCH FNL: ~p ~p ~p", [FileInpName, Filename, Filename0]),
		    case FileInpName of
			 FIN when FIN=:=<<"category[blog_image]">>;FIN=:=<<"blog_image">> ->
			    SaveFileName1 = get_file_name(Filename, FileInpName),
			    lager:debug("temporary file name: ~p", [SaveFileName1]),
			    Req3 = save_file(Req2, SaveFileName1),
			    {Req3 ,[{<<"mkh_blog_category_image">>, SaveFileName1}]};
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
    %UploadFileName = muploader_utils:change_file_name(UploadFileName0),
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


