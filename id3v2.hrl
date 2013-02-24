%% Copyright
-author("Nikolay Mavrenkov (koluch@koluch.ru)").

-record(string, {enc=iso88591,value}).

-record(header_version, {major,revision}).
-record(header_flags, {unsinc, ext_header, experiment,footer}).
-record(header, {ident = "ID3", version=#header_version{},flags=#header_flags{},size}).

-record(frame, {id,size,flags,data,raw_data}).

-record(ext_header_flag, {name,value,data}).
-record(ext_header, {size,flags}).

-record(footer_version, {major,revision}).
-record(footer_flags, {unsinc, ext_header, experiment,footer}).
-record(footer, {ident = "3DI", version=#footer_version{},flags=#footer_flags{},size}).


-record(tag, {header, ext_header, frames, footer}).


