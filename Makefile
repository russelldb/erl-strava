PROJ = strava
REBAR ?= ./rebar3

.PHONY: all app clean distclean shell test

all: $(REBAR)
	$(REBAR) compile

app: $(REBAR)
	$(REBAR) compile skip_deps=true

clean: $(REBAR)
	$(REBAR) clean

distclean: clean
	rm -rf .eunit/ _build/ doc/ rebar3

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
