all: compile

compile:
	erlc id3v2.erl
	erlc id3v2_native_frames.erl

clean:
	rm -f *.beam
	rm -f *.dump

install: compile
	rm -rf ~/.erlang_libs/id3v2
	mkdir ~/.erlang_libs/id3v2
	cp *.beam ../../headers/*.hrl  ~/.erlang_libs/id3v2

run: compile
	erl -eval 'Info = music_info:get_info("../test/2.mp3"), io:format("Info: ~p~n", [Info])' -s init stop -noshell


