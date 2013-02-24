%% Copyright
-module(music_info).
-author("Nikolay Mavrenkov (koluch@koluch.ru)").

%% API
-export([get_info/1]).
-include("../../headers/id3v2.hrl").
-include("../../headers/music_info.hrl").

get_info(FileName) ->
    case string:right(FileName,4) of
        ".mp3" -> get_mp3_info(FileName)
    end.

get_mp3_info(FileName) ->
    Tag = id3v2:read_file(FileName),
    Artist = find_info("TPE1", Tag#tag.frames),
    Album = find_info("TALB", Tag#tag.frames),
    Title = find_info("TIT2", Tag#tag.frames),
    TrackNumber = find_info("TRCK", Tag#tag.frames),
    #media_info{
        artist = Artist,
        album = Album,
        title = Title,
        track_number = TrackNumber
    }.

find_info(Name, [#frame{id=Name} = Frame|_]) -> Frame#frame.data;
find_info(_Name, []) -> not_found;
find_info(Name, [_|Rest]) -> find_info(Name, Rest).
