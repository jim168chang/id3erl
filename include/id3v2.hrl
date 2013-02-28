%% Copyright
-author("Nikolay Mavrenkov (koluch@koluch.ru)").

-record(string, {enc=iso88591,value}).

-record(id3_header_version, {major,revision}).
-record(id3_header_flags, {unsinc, ext_header, experiment,footer}).
-record(id3_header, {ident = "ID3", version=#id3_header_version{},flags=#id3_header_flags{},size}).

-record(id3_frame, {id,size,flags,data,raw_data}).
-record(id3_frames, {state = partly, size = 0, list = []}).

-record(id3_ext_header_flag, {name,value,data}).
-record(id3_ext_header, {size,flags}).

-record(id3_footer_version, {major,revision}).
-record(id3_footer_flags, {unsinc, ext_header, experiment,footer}).
-record(id3_footer, {ident = "3DI", version=#id3_footer_version{},flags=#id3_footer_flags{},size}).


-record(id3_tag, {header, ext_header, frames = #id3_frames{}, footer}).


