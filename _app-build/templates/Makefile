ROOT=.
BUILD_DIR=$(ROOT)
APP_NAME={{app_name}}
VERSION={{vsn}}
FULL_APP_NAME=$(APP_NAME)-$(VERSION)

all: build

clean:
	rm -rf $(ROOT)/ebin/*.beam
	rm -rf *.dump

build: clean
	erlc -o $(ROOT)/ebin $(ROOT)/src/*.erl

pkg: build
	rm -rf $(APP_NAME)-*
	mkdir $(FULL_APP_NAME)
	cp -r $(ROOT)/ebin $(FULL_APP_NAME)
	cp -r $(ROOT)/include $(FULL_APP_NAME)

install: pkg
	rm -rf ~/.erlang_libs/$(APP_NAME)-*
	mv $(FULL_APP_NAME) ~/.erlang_libs/

test:
#	erl -eval 'io:format("Tag for file '1.mp3': ~p~n", [id3v2_file_reader:read_file("1.mp3")])' -s init stop -noshell
	erl -eval 'id3v2:test("1.mp3")' -s init stop -noshell


