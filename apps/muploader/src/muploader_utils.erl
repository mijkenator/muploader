-module(muploader_utils).

-export([
    get_tmp_dir/0
    ,is_supported_image_format/1
    ,create_preview/2
]).

get_tmp_dir() -> <<"/tmp/">>.

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
    <<" -thumbnail 685x877 ">>.

-spec create_preview(binary(), binary()) -> true|false.
create_preview(NameTo, NameFrom) -> 
    RS = get_preview_options(),
    Cmd = <<"convert ", NameFrom/binary, RS/binary, NameTo/binary>>,
    lager:debug("Convert cmd: ~p", [Cmd]),
    Ret = os:cmd(binary_to_list(Cmd)),
    lager:debug("Convert ret: ~p", [Ret]),
    true.

