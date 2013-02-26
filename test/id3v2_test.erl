%% Copyright
-module(id3v2_test).
-author("Nikolay Mavrenkov (koluch@koluch.ru)").

%% API
-export([start/1]).
-include("../include/id3v2.hrl").
-define(MAJOR_VERSION,32#04).
-define(REVISION,32#00).

start(FileName) ->
    write_test(FileName).

%%%%%%%%
%% Write test data
write_test(FileName) ->

    %% Write header
    {ok, File} = file:open(FileName, [write,binary]),
    MajorVersion = ?MAJOR_VERSION,
    Revision = ?REVISION,
    Flags = 2#01010000,
    Size = 51,
    file:write(File, <<"ID3",MajorVersion:8,Revision:8,Flags:8,Size:32>>),

    %% Write extender header
    ExtHeaderSize = 9,
    ExtFlags = 2#01110000,
    Crc = 16#0FFFFFFF,
    Restrictions = 2#10101010,
    file:write(File, <<ExtHeaderSize:32,01:8,ExtFlags:8,00:8,05:8,Crc:40,01:8,Restrictions:8>>),

    %% Write frame (song name)
    SongNameFrameId = list_to_binary("TIT2"),
    SongNameFrameSize = 7,
    SongNameFrameFlags = 2#0111000001001111,
    SongNameFrameEncoding = 32#03,
    SongName = list_to_binary("Lulaby"),
    file:write(File, <<SongNameFrameId:4/binary,SongNameFrameSize:32,SongNameFrameFlags:16,SongNameFrameEncoding:8,SongName:6/binary>>),

    %% Write frame (artist name)
    ArtistFrameId = list_to_binary("TPE1"),
    ArtistFrameSize = 9,
    ArtistFrameFlags = 2#0111000001001111,
    ArtistFrameEncoding = 32#03,
    Artist = list_to_binary("The Cure"),
    file:write(File, <<ArtistFrameId:4/binary,ArtistFrameSize:32,ArtistFrameFlags:16,ArtistFrameEncoding:8,Artist:8/binary>>)

    %% Write footer
    ,file:write(File, <<"3DI",MajorVersion:8,Revision:8,Flags:8,Size:32>>)
.

