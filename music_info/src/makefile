all: compile

compile:
	erlc music_info.erl

clean:
	rm -f *.beam
	rm -f *.dump

run: compile
	erl -eval 'Info = music_info:get_info("../test/2.mp3"), io:format("Info: ~p~n", [Info])' -s init stop -noshell


