.SUFFIXES: 

# env
ERL_PROG := $(shell which erl)
ERL_TOP := $(shell ls -la $(ERL_PROG) | awk '{ gsub(/bin\/erl/, "", $$NF); print $$NF }')
ERTS_VERSION := $(shell ls -la "$(ERL_TOP)" | grep erts- | awk '{ gsub(/erts-/, "", $$NF); print $$NF }')
CWD := pwd

ERL := $(ERL_PROG)
EBIN := ebin
CODE_PATH := -pa ebin -pa test
ERL_BOOT := -sname ct -s ct_run script_start -s erlang halt
ERL_FLAGS=$(CODE_PATH) $(ERL_BOOT) -I include 
SOURCE_DIRS := `find src -type d`
BEAMDIRS := `find src -type d | sed s/src/ebin/`
COVER_SOURES := `find src -type f | sed s/src/ebin/`
DIRNAME := $(shell pwd | awk -F "/" '{ gsub(/ $$/,"",$$NF); print $$NF }') 
APPFILE := $(shell find . -type f | grep app$ | sort -u)
APPVER := $(shell cat $(APPFILE) | awk '/vsn/ { gsub(/vsn|,|{|}|\"/, ""); print $$NF }') 
APPNAME ?= $(shell cat $(APPFILE) | awk '/{application,.*/ { gsub(/application|,|{|}/, ""); print $$NF }')
RELEASE := $(APPNAME:%=%.rel)
COVER_DATA := $(shell pwd | awk -F "/" '{ print "coverage/"$$NF".coverdata" }') 
TESTLOGS=test/logs

# PORT_TEST_CSRC := `find test/port_server_SUITE_data -type f | grep .c`
# PORT_TEST_OBJ=$(patsubst %.c, %.o, $(PORT_TEST_CSRC))

# rules and tasks

.PHONY: help clean build test release 

all: clean test 

#TODO: implement release, boot script generation and install

info:
	$(info erl program located at $(ERL_PROG))
	$(info ERL_TOP locating in $(ERL_TOP))
	$(info installed erts version is $(ERTS_VERSION))

# generate release and boot scripts, create release tarball
release: $(RELEASE)
	$(info generating release for $(APPNAME))

$(RELEASE): build
	$(info generating .rel script $@)
	./make-release.sh $(APPNAME) $(APPVER) $(ERTS_VERSION)

# run test suite(s) using common_test
test: $(TESTLOGS) $(COVER_SOURCES) $(COVER_DATA) build
	erl $(ERL_BOOT) -dir test -logdir $(TESTLOGS) -ct_config test/test_config.ctc -cover coverage.spec 

# build all sources using make:all
build: $(EBIN) Emakefile
	$(ERL) -noshell -make

# generate an Emakefile for all the directories under src
Emakefile:
	find src -type f | awk -F "/" '{ print substr($$0, 0, index($$0, $$NF) - 1) }' | sort -u \
		| awk -v cwd=$(CWD) '{ print "{""\""$$0"*\",[\ndebug_info,\n{outdir, ""\"""ebin""\"""},\n{i,""\"""./include""\"""}\n]""}." }' >> Emakefile

# make the ebin dir and any subdirs required by packages in use in src/**
$(EBIN):
	mkdir -p $(BEAMDIRS)

# make the test log directories
$(TESTLOGS) coverage: 
	mkdir -p $@

# touch the cover data file
$(COVER_DATA): coverage
	touch $(COVER_DATA)

# remove generated files and folders
clean:
	rm -rf $(EBIN) test/*.beam coverage/ $(TESTLOGS) Emakefile $(RELEASE)

