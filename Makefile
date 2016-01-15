PROJ = strava
REBAR ?= ./rebar3
RM ?= rm -rf

.PHONY: all app clean distclean shell test

all: $(REBAR)
	$(REBAR) compile

app: $(REBAR)
	$(REBAR) compile skip_deps=true

clean: $(REBAR)
	$(REBAR) clean

distclean: clean
	$(RM) .eunit
	$(RM) _build

doc: $(REBAR)
	$(REBAR) edoc

rebar3:
	wget "https://s3.amazonaws.com/rebar3/rebar3" -O $@-part
	chmod +x $@-part
	mv $@-part $@

shell: $(REBAR)
	$(REBAR) shell

test: $(REBAR)
	$(REBAR) ct
