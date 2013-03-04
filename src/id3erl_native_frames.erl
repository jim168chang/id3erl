%% Copyright
-module(id3erl_native_frames).
-author("Nikolay Mavrenkov (koluch@koluch.ru)").
-include("../include/id3v2.hrl").

%% API
-export([is_text/1, parse/2,get_data_type/1]).

is_text("TALB") -> true;
is_text("TBPM") -> true;
is_text("TCOM") -> true;
is_text("TCON") -> true;
is_text("TCOP") -> true;
is_text("TDEN") -> true;
is_text("TDLY") -> true;
is_text("TDOR") -> true;
is_text("TDRC") -> true;
is_text("TDRL") -> true;
is_text("TDTG") -> true;
is_text("TENC") -> true;
is_text("TEXT") -> true;
is_text("TFLT") -> true;
is_text("TIPL") -> true;
is_text("TIT1") -> true;
is_text("TIT2") -> true;
is_text("TIT3") -> true;
is_text("TKEY") -> true;
is_text("TLAN") -> true;
is_text("TLEN") -> true;
is_text("TMCL") -> true;
is_text("TMED") -> true;
is_text("TMOO") -> true;
is_text("TOAL") -> true;
is_text("TOFN") -> true;
is_text("TOLY") -> true;
is_text("TOPE") -> true;
is_text("TOWN") -> true;
is_text("TPE1") -> true;
is_text("TPE2") -> true;
is_text("TPE3") -> true;
is_text("TPE4") -> true;
is_text("TPOS") -> true;
is_text("TPRO") -> true;
is_text("TPUB") -> true;
is_text("TRCK") -> true;
is_text("TRSN") -> true;
is_text("TRSO") -> true;
is_text("TSOA") -> true;
is_text("TSOP") -> true;
is_text("TSOT") -> true;
is_text("TSRC") -> true;
is_text("TSSE") -> true;
is_text("TSST") -> true;
is_text("TXXX") -> true;
is_text(_) -> false.

is_url("WCOM") -> true;
is_url("WCOP") -> true;
is_url("WOAF") -> true;
is_url("WOAR") -> true;
is_url("WOAS") -> true;
is_url("WORS") -> true;
is_url("WPAY") -> true;
is_url("WPUB") -> true;
is_url("WXXX") -> true;
is_url(_) -> false.

get_data_type(Id) ->
    case is_text(Id) of
        true -> text;
        _ ->
            case is_url(Id) of
                true -> url;
                _ -> unknown
            end
    end.

parse(Id, Data) ->
    Type = get_data_type(Id),
    parse_by_type(Id, Type, Data).


parse_by_type(Id, text, <<EncNum:8,EncTextBin/binary>>) ->
    Enc = get_encoding(EncNum),
    EncText = binary_to_list(EncTextBin),
    case Id of
        "TXXX" ->
            Zero = get_zero_code(Enc),
            [Desc|Value] = string:tokens(EncText, Zero),
            {
                #string{enc=Enc,value=Desc},
                #string{enc=Enc,value=Value}
            };
        _ -> #string{enc=Enc,value= EncText}
    end;

parse_by_type(Id, url, UrlBin) ->
    case Id of
        "WXXX" ->
            <<EncNum:8,Url/binary>> = UrlBin,
            Enc = get_encoding(EncNum),
            Zero = get_zero_code(Enc),
            [Desc|Value] = string:tokens(binary_to_list(Url), Zero),
            {
                #string{enc=Enc,value=Desc},
                #string{value=Value}
            };
        _ -> #string{value=binary_to_list(UrlBin)}
    end;

parse_by_type(_Id, _UnknownType, Data) -> Data.


get_encoding(0) -> iso88591;
get_encoding(1) -> utf16;
get_encoding(2) -> utf16be;
get_encoding(3) -> utf8.

get_zero_code(iso88591) -> [0];
get_zero_code(utf16) -> [0,0];
get_zero_code(utf16be) -> [0,0];
get_zero_code(utf8) -> [0].
