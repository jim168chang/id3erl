ROOT=.
BUILD_DIR=$(ROOT)
APP_NAME={{description}}
VERSION={{vsn}}
FULL_APP_NAME=$(APP_NAME)-$(VERSION)

all: build

clean:
	rm -rf $(ROOT)/ebin
	rm -rf *.dump

build: clean
	rm -rf $(BUILD_DIR)/ebin/*.beam
	erlc -o $(ROOT)/ebin $(ROOT)/src/*.erl
	$(ROOT)/priv/generate_app_file.esh $(ROOT)/priv/app.src $(ROOT)/ebin/$(APP_NAME).app

install: build
	rm -rf $(FULL_APP_NAME)
	mkdir $(FULL_APP_NAME)
	cp -r $(ROOT)/ebin $(FULL_APP_NAME)
	cp -r $(ROOT)/include $(FULL_APP_NAME)
	rm -rf ~/.erlang_libs/$(APP_NAME)-*
	mv $(FULL_APP_NAME) ~/.erlang_libs/

test:
#	erl -eval 'io:format("Tag for file '1.mp3': ~p~n", [id3v2_file_reader:read_file("1.mp3")])' -s init stop -noshell
	erl -eval 'id3v2:test("1.mp3")' -s init stop -noshell


