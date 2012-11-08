REBAR=rebar

compile:
	$(REBAR) compile

itest: compile
	$(REBAR) ct apps=ebi_core

clean:
	$(REBAR) clean apps=ebi_core

clean-all:
	$(REBAR) clean

test:
	$(REBAR) eunit apps=ebi_core

.PHONY: compile itest clean test
