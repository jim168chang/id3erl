ROOT=.
APP_NAME=id3erl
VERSION=0.1.0
FULL_APP_NAME=$(APP_NAME)-$(VERSION)

BUILD_DIR=$(ROOT)
PKG_DIR=$(ROOT)/priv/pkg
CURRENT_PKG_DIR=$(ROOT)/priv/pkg/$(FULL_APP_NAME)

all: pkg

clean_tmp:
	rm -rf *.dump

clean_ebin:
	rm -rf $(ROOT)/ebin/

clean_pkg:
	rm -rf $(CURRENT_PKG_DIR)

clean: clean_ebin clean_tmp clean_pkg

ebin: clean_ebin
	rm -rf $(ROOT)/ebin
	mkdir $(ROOT)/ebin
	erlc -o $(ROOT)/ebin $(ROOT)/src/*.erl
	chmod a+x $(ROOT)/priv/generate_app_file.esh
	$(ROOT)/priv/generate_app_file.esh $(ROOT)/priv/app.src $(ROOT)/ebin/$(APP_NAME).app

pkg: ebin
	rm -rf $(APP_NAME)-*
	mkdir $(CURRENT_PKG_DIR)
	cp -r $(ROOT)/ebin $(CURRENT_PKG_DIR)
	cp -r $(ROOT)/include $(CURRENT_PKG_DIR)

install: pkg
	rm -rf ~/.erlang_libs/$(APP_NAME)-*
	mv $(CURRENT_PKG_DIR) ~/.erlang_libs/
