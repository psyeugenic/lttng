REBAR=./rebar

all: $(REBAR)
	$(REBAR) compile
	 
$(REBAR):		
	wget --output-document=$(REBAR) http://cloud.github.com/downloads/basho/rebar/rebar && chmod u+x $(REBAR)
