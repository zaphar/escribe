include ../include/globals.mk

all: $(ERL_OBJECTS)

doc: 
	@echo "generating docs for escribe"
	$(ERL_RUN) edoc files $(ERL_SOURCES) $(ERL_STOP)
	mv *.html $(DOC_DIR)
	mv *.png $(DOC_DIR)
	mv *.css $(DOC_DIR)

clean:
	rm -f $(CLEAN_TARGETS)
	rm -f $(DOC_DIR)/*

test:
	@echo "No tests for src directory"
