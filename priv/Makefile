ROOT=..
APP_NAME=id3v2
VERSION=0.1.0
FULL_APP_NAME=$(APP_NAME)-$(VERSION)
BUILD_DIR=$(ROOT)/$(FULL_APP_NAME)

all: build

clean:
	rm -rf $(BUILD_DIR)
	rm -rf *.dump

build: clean
	rm -rf $(BUILD_DIR)
	mkdir $(BUILD_DIR)
	mkdir $(BUILD_DIR)/ebin
	erlc -o $(BUILD_DIR)/ebin $(ROOT)/src/*.erl
	./generate_app_file.esh $(APP_NAME) $(VERSION)
	cp -r $(ROOT)/include $(BUILD_DIR)
	mv $(APP_NAME).app $(BUILD_DIR)/ebin


install: build
	rm -rf ~/.erlang_libs/$(APP_NAME)-*
	cp -r $(BUILD_DIR) ~/.erlang_libs/

test:
#	erl -eval 'io:format("Tag for file '1.mp3': ~p~n", [id3v2_file_reader:read_file("1.mp3")])' -s init stop -noshell
	erl -eval 'id3v2:test("1.mp3")' -s init stop -noshell


