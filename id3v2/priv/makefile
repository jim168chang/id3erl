ROOT=..
APP_NAME=id3v2
VERSION=0.1.0
BUILD_DIR=${ROOT}/$(APP_NAME)-$(VERSION)

all: compile

test:
	pwd

clean:
	rm -rf $(BUILD_DIR)
	rm -rf $(ROOT)/*.dump

build: clean
	rm -rf $(BUILD_DIR)
	mkdir $(BUILD_DIR)
	mkdir $(BUILD_DIR)/ebin
	erlc -o $(BUILD_DIR)/ebin $(ROOT)/src/*.erl
	erlc generate_app_file.erl
	erl -eval 'generate_app_file:generate([{id, $(APP_NAME)},{vsn,"$(VERSION)"}])' -s init stop -noshell
	cp -r $(ROOT)/include $(BUILD_DIR)
	mv $(ROOT)/$(APP_NAME).app $(BUILD_DIR)/ebin


install: build
	rm -rf ~/.erlang_libs/$(APP_NAME)
	mkdir ~/.erlang_libs/$(APP_NAME)
	cp ../src/*.beam ../include/*.hrl  ~/.erlang_libs/id3v2

run: compile
	erl -eval 'Info = music_info:get_info("../test/2.mp3"), io:format("Info: ~p~n", [Info])' -s init stop -noshell


