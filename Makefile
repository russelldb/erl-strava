PROJ = strava
ERL ?= erl
ERL_FLAGS ?= -pa ebin/ -pa deps/*/ebin/ -smp
REBAR ?= ./rebar
RM ?= rm -rf

.PHONY: all app clean distclean shell test

all: $(REBAR)
	$(REBAR) compile

app: $(REBAR)
	$(REBAR) compile skip_deps=true

clean: $(REBAR)
	$(REBAR) clean

distclean: clean
	$(REBAR) delete-deps
	$(RM) .eunit

doc: $(REBAR)
	$(REBAR) doc

rebar:
	wget "https://github.com/rebar/rebar/releases/download/2.6.0/rebar" -O $@-part
	chmod +x $@-part
	mv $@-part $@

shell: app
	$(ERL) $(ERL_FLAGS)

test: $(APP) $(REBAR)
	$(REBAR) ct
