-module(muploader_utils).

-export([
    get_tmp_dir/0
    ,get_tmp_dir/1
    ,get_tmp_dir/2
    ,is_supported_image_format/1
    ,create_preview/2
    ,get_preview_options/0
    ,tinyfile/1
]).


tinyfile(<<"/opt/mwd_admin/images/blog_category", _/binary>>) -> ok;
tinyfile(<<"/opt/mwd_admin/images/blog_post", _/binary>>)     -> ok;
tinyfile(<<"/opt/mybestday/images/u/posts", _/binary>> = FileName) when is_binary(FileName) ->
    %Fun = fun() ->
    %	lager:debug("MU TINY MBD600 start ~p", [FileName]),
	%    process_flag(trap_exit, true),
	%    os:cmd("/home/ubuntu/work/tinify/tf_mbd.py '"++ binary_to_list(FileName) ++"' 780"),
    % 	lager:debug("MU TINY MBD600 end ~p", [FileName])
    %end,
    %spawn(Fun);
    ok; %now optimizing on remote
tinyfile(<<"/opt/mybestday/images/u/slide", _/binary>> = FileName) when is_binary(FileName) ->
    %Fun = fun() ->
    % 	lager:debug("MU TINY MBD2000 start ~p", [FileName]),
	%    process_flag(trap_exit, true),
	%    os:cmd("/home/ubuntu/work/tinify/tf_mbd.py '"++ binary_to_list(FileName) ++"' 2000"),
    % 	lager:debug("MU TINY MBD2000 end ~p", [FileName])

    %    %identify /opt/mybestday/images/u/01.jpg | awk '{print $3}'
    %end,
    %spawn(Fun);
    ok; %now optimizing on remote
tinyfile(<<"/opt/mybestday/images/u/bg", _/binary>> = FileName) when is_binary(FileName) -> ok;
tinyfile(<<"/opt/mybestday/images/u", _/binary>> = FileName) when is_binary(FileName) ->
    Fun = fun() ->
    	lager:debug("MU TINY MBD180 start ~p", [FileName]),
	    process_flag(trap_exit, true),
	    os:cmd("/home/ubuntu/work/tinify/tf_mbd180.py '"++ binary_to_list(FileName) ++"'"),
    	lager:debug("MU TINY MBD180 end ~p", [FileName])
    end,
    spawn(Fun);
tinyfile(<<"/opt/mwd_admin/images/", _/binary>> = FileName) when is_binary(FileName) ->
    Fun = fun() ->
    	lager:debug("MU TINY start ~p", [FileName]),
	    process_flag(trap_exit, true),
	    os:cmd("/home/ubuntu/work/tinify/tf.py '"++ binary_to_list(FileName) ++"'"),
    	lager:debug("MU TINY end ~p", [FileName])
    end,
    spawn(Fun);
tinyfile(_) -> ok.


get_tmp_dir() -> <<"/tmp/">>.

get_tmp_dir(Name) -> get_tmp_dir(old, Name).
get_tmp_dir(bm,<<"collection[logo]">>)             -> <<"/opt/mwd_admin/images/bmcollection_logos/">>;
get_tmp_dir(bm,<<"collection[poster]">>)           -> <<"/opt/mwd_admin/images/bmcollection_logos/">>;
get_tmp_dir(bm,<<"collection[logoPoster]">>)       -> <<"/opt/mwd_admin/images/bmcollection_logos/">>;
get_tmp_dir(bm,<<"collection[posterLogo]">>)       -> <<"/opt/mwd_admin/images/bmcollection_logos/">>;
get_tmp_dir(bm,<<"collection[posterSeason]">>)     -> <<"/opt/mwd_admin/images/bmcollection_logos/">>;
get_tmp_dir(bm,<<"collection[posterCollection]">>) -> <<"/opt/mwd_admin/images/bmcollection_logos/">>;
get_tmp_dir(bm,<<"dress[back]">>) 			        -> <<"/opt/mwd_admin/images/bmcollection_items/">>;
get_tmp_dir(bm,<<"dress[front]">>) 		            -> <<"/opt/mwd_admin/images/bmcollection_items/">>;
get_tmp_dir(bm,<<"file">>)		 		            -> <<"/opt/mwd_admin/images/bmcollection_items/">>;

get_tmp_dir(mbd,<<"file">>)		 		            -> <<"/opt/mybestday/images/u/">>;
get_tmp_dir(mbd,<<"mbd_upload">>)		 		    -> <<"/opt/mybestday/images/u/">>;
get_tmp_dir(mbdms,_)		             		    -> <<"/opt/mybestday/images/u/slide/">>;
get_tmp_dir(mbdbg,_)		             		    -> <<"/opt/mybestday/images/u/bg/">>;
get_tmp_dir(mbdpost,_)		             		    -> <<"/opt/mybestday/images/u/posts/">>;

get_tmp_dir(_,<<"collection[logo]">>)             -> <<"/opt/mwd_admin/images/collection_logos/">>;
get_tmp_dir(_,<<"collection[poster]">>)           -> <<"/opt/mwd_admin/images/collection_logos/">>;
get_tmp_dir(_,<<"collection[logoPoster]">>)       -> <<"/opt/mwd_admin/images/collection_logos/">>;
get_tmp_dir(_,<<"collection[posterLogo]">>)       -> <<"/opt/mwd_admin/images/collection_logos/">>;
get_tmp_dir(_,<<"collection[posterSeason]">>)     -> <<"/opt/mwd_admin/images/collection_logos/">>;
get_tmp_dir(_,<<"collection[posterCollection]">>) -> <<"/opt/mwd_admin/images/collection_logos/">>;
get_tmp_dir(_,<<"dress[back]">>) 			        -> <<"/opt/mwd_admin/images/collection_items/">>;
get_tmp_dir(_,<<"dress[front]">>) 		            -> <<"/opt/mwd_admin/images/collection_items/">>;
get_tmp_dir(_,<<"file">>)		 		            -> <<"/opt/mwd_admin/images/collection_items/">>;
get_tmp_dir(_,<<"coverimage">>)		 	            -> <<"/opt/mwd_admin/images/usercovers/">>;
get_tmp_dir(_,<<"blog_image">>)                   -> <<"/opt/mwd_admin/images/blog_category/">>;
get_tmp_dir(_,<<"category[blog_image]">>)         -> <<"/opt/mwd_admin/images/blog_category/">>;
get_tmp_dir(_,<<"post[image]">>)                  -> <<"/opt/mwd_admin/images/blog_post/">>;
get_tmp_dir(_,<<"post[logo]">>)                   -> <<"/opt/mwd_admin/images/blog_post/">>;
get_tmp_dir(_,<<"image">>)                        -> <<"/opt/mwd_admin/images/blog_post/">>;
get_tmp_dir(_,<<"logo">>)                         -> <<"/opt/mwd_admin/images/blog_post/">>;
get_tmp_dir(_,_) -> <<"/tmp/">>.

-spec is_supported_image_format(binary()) -> true|false.
is_supported_image_format(ExtName) ->
    is_supported_image_format_i(list_to_binary(string:to_lower(binary_to_list(ExtName)))).

is_supported_image_format_i(<<".jpg">>) -> true;
is_supported_image_format_i(<<".png">>) -> true;
is_supported_image_format_i(<<".gif">>) -> true;
is_supported_image_format_i(<<".jpeg">>)-> true;
is_supported_image_format_i(_) -> false.

-spec get_preview_options() -> binary().
get_preview_options() ->
    %<<" -resize 685x877 ">>.
    %<<" -thumbnail 685x877 ">>.
    <<" -thumbnail x877 ">>.

-spec create_preview(binary(), binary()) -> true|false.
create_preview(NameTo, NameFrom) -> 
    %RS = get_preview_options(),
    %Cmd = <<"convert ", NameFrom/binary, RS/binary, NameTo/binary>>,
    Cmd = <<"convert ../../685x877tr.png \\( ", NameFrom/binary, " -thumbnail 685x877 \\) -gravity NorthWest -composite ",NameTo/binary>>,
    lager:debug("Convert cmd: ~p", [Cmd]),
    Ret = os:cmd(binary_to_list(Cmd)),
    lager:debug("Convert ret: ~p", [Ret]),
    true.

