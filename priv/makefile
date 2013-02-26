ROOT=..
APP_NAME=id3v2
VERSION=0.1.0
FULL_APP_NAME=$(APP_NAME)-$(VERSION)
BUILD_DIR=$(ROOT)/$(FULL_APP_NAME)

all: build

test:
	pwd

clean:
	rm -rf $(BUILD_DIR)
	rm -rf *.dump

build: clean
	rm -rf $(BUILD_DIR)
	mkdir $(BUILD_DIR)
	mkdir $(BUILD_DIR)/ebin
	erlc -o $(BUILD_DIR)/ebin $(ROOT)/src/*.erl
	erlc generate_app_file.erl
	erl -eval 'generate_app_file:generate([{id, $(APP_NAME)},{vsn,"$(VERSION)"}])' -s init stop -noshell
	cp -r $(ROOT)/include $(BUILD_DIR)
	mv $(APP_NAME).app $(BUILD_DIR)/ebin


install: build
	rm -rf ~/.erlang_libs/$(APP_NAME)-*
	cp -r $(BUILD_DIR) ~/.erlang_libs/

run: compile
	erl -eval 'Info = music_info:get_info("../test/2.mp3"), io:format("Info: ~p~n", [Info])' -s init stop -noshell


