PROJECT = pubsub
PROJECT_DESCRIPTION = Celtra test assignment
PROJECT_VERSION = 0.1.0

DEPS = cowboy jsx
LOCAL_DEPS = inets ssl
DEP_PLUGINS = cowboy
TEST_DEPS = meck

dep_cowboy_commit = 2.0.0-rc.4
dep_meck = git https://github.com/eproxus/meck.git master

APP_VERSION = $(shell cat rel/version)

pubsub: all
	echo APP_VERSION = $(APP_VERSION)

include erlang.mk
