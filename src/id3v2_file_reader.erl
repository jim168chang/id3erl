%% Copyright
-module(id3v2_file_reader).
-author("Nikolay Mavrenkov (koluch@koluch.ru)").

%% API
-export([read_file/1]).
-include("../include/id3v2.hrl").

-define(MAJOR_VERSION,32#04).
-define(REVISION,32#00).

-define(ID3_FRAME_HEADER_SIZE, 10).
-define(ID3_EXT_HEADER_SIZE, 6).
-define(ID3_HEADER_SIZE, 10).


read_file(FileName) ->
    {ok, File} = file:open(FileName, [read,binary]),
    Header = read_header(File),

    ExtHeader = case Header#id3_header.flags#id3_header_flags.ext_header of
        true -> read_ext_header(File);
        _ -> not_present
    end,

    FramesSize = Header#id3_header.size - get_ext_header_size(ExtHeader),
    Frames = read_frames(File, FramesSize),

    Footer = case Header#id3_header.flags#id3_header_flags.footer of
        true -> read_footer(File);
        _ -> not_present
    end,

    #id3_tag{
            header = Header,
            ext_header = ExtHeader,
            frames = Frames,
            footer = Footer
    }.


%%%%%%%%
%% Header routine
read_header(File) ->
    case file:read(File,10) of
        {ok, <<"ID3",?MAJOR_VERSION:8 ,?REVISION:8,Flags:1/binary,SyncsafeSize:32>>} ->
            case Flags of
                <<Unsync:1,Ext:1,Exp:1,Footer:1,2#0000:4>> ->
                    #id3_header{
                            version=#id3_header_version{major=?MAJOR_VERSION,revision=?REVISION},
                            flags=#id3_header_flags{
                                    unsinc= id3v2_misc:int_to_bool(Unsync),
                                    ext_header = id3v2_misc:int_to_bool(Ext),
                                    experiment = id3v2_misc:int_to_bool(Exp),
                                    footer= id3v2_misc:int_to_bool(Footer)},
                            size= id3v2_misc:read_syncsafe_int(SyncsafeSize)
                    }
            end
    end.

%%%%%%%%
%% Ext header routine
read_ext_header(File) ->
    case file:read(File,6) of
        {ok, <<SyncsafeSize:32,32#01:8,BinFlags/binary>>} ->
            {Update,CRC,Rests} = case BinFlags of
                <<2#0:1,_Update:1,_CRC:1,_Rests:1,2#0000:4>> ->
                    {id3v2_misc:int_to_bool(_Update), id3v2_misc:int_to_bool(_CRC), id3v2_misc:int_to_bool(_Rests)}
            end,
            UpdateFlag = read_ext_header_flag(File, update, Update),
            CRCFlag = read_ext_header_flag(File, crc, CRC),
            RestsFlag = read_ext_header_flag(File, rests, Rests),
            #id3_ext_header{
                size = id3v2_misc:read_syncsafe_int(SyncsafeSize),
                flags = [UpdateFlag,CRCFlag,RestsFlag]
            }
    end.

read_ext_header_flag(File, Name, true) ->
    {ok, <<FlagLength>>} = file:read(File, 1),
    {ok, FlagData} = file:read(File, FlagLength),
    #id3_ext_header_flag{name = Name, value = true, data = FlagData};
read_ext_header_flag(_File, Name, _False) -> #id3_ext_header_flag{name = Name, value = false}.

get_ext_header_size(not_present) -> 0;
get_ext_header_size(ExtHeader) -> ExtHeader#id3_ext_header.size + 6.


%%%%%%%%
%% Frames routine

read_frames(File, FramesSize) -> read_frames1(File,FramesSize,[]).

read_frames1(_File, SubZero, FrameList) when SubZero < 0 -> read_frames1(_File, 0, FrameList);
read_frames1(_File, 0, FrameList) -> #id3_frames{state = full, size = calculate_frame_size(FrameList,0), list = lists:reverse(FrameList)};
read_frames1(File, RestFramesSize, FrameList) ->
    Frame = read_frame(File),
    case Frame of
        padding -> read_frames1(File, 0, [{padding,RestFramesSize}|FrameList]);
        _ ->
            Size = Frame#id3_frame.size + 10,
            read_frames1(File, RestFramesSize - Size, [Frame|FrameList])
    end.

read_frame(File) ->
    {ok, <<BinId:4/binary,SyncsafeSize:32,Flags:2/binary>>} = file:read(File, 10),
    Size = id3v2_misc:read_syncsafe_int(SyncsafeSize),
    case Size of
        0 -> padding;
        _ ->
            Id = binary_to_list(BinId),
            RawData = read_frame_data(File, Size),
            ParsedData = id3v2_native_frames:parse(Id, RawData),
            #id3_frame{
                id = Id,
                size = Size,
                flags = Flags,
                data = ParsedData,
                raw_data = RawData
            }
    end.

read_frame_data(File, Size) ->
    {ok, Data} = file:read(File, Size), Data.

calculate_frame_size([], Size) -> Size;
calculate_frame_size([{padding, PaddingSize}|Rest], Size) -> calculate_frame_size(Rest, Size + PaddingSize);
calculate_frame_size([Frame|Rest], Size) ->
    NewSize = Size + Frame#id3_frame.size + ?ID3_FRAME_HEADER_SIZE,
    calculate_frame_size(Rest, NewSize).



%%%%%%%%
%% Footer routine
read_footer(File) ->
    case file:read(File,10) of
        {ok, <<"3DI",?MAJOR_VERSION:8 ,?REVISION:8,Flags:1/binary,Size:32>>} ->
            case Flags of
                <<Unsync:1,Ext:1,Exp:1,Footer:1,2#0000:4>> ->
                    #id3_footer{
                            version=#id3_footer_version{major=?MAJOR_VERSION,revision=?REVISION},
                            flags=#id3_footer_flags{
                                    unsinc= id3v2_misc:int_to_bool(Unsync),
                                    ext_header = id3v2_misc:int_to_bool(Ext),
                                    experiment = id3v2_misc:int_to_bool(Exp),
                                    footer= id3v2_misc:int_to_bool(Footer)},
                            size=Size
                    }
            end
    end.


%%
%% Common routine

