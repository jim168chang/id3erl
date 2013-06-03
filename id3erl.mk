#test: build
#	erl -eval 'io:format("Tag for file '1.mp3': ~p~n", [id3v2_file_reader:read_file("1.mp3")])' -s init stop -noshell
#	erl -eval 'id3v2:test("1.mp3")' -s init stop -noshell