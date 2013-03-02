%% Copyright
-module(id3v2).
-author("Nikolay Mavrenkov (koluch@koluch.ru)").

%% API
-export([file_full/1,file_stream/1]).

%% Read full tag from file
file_full(FileName) -> id3v2_file_reader:read_file(FileName).

%% Start process for read file on demand
file_stream(FileName) -> id3v2_stream_reader:start(id3v2_stream:create_by_file(FileName)).
