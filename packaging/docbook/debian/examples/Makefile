SRCS	:= $(wildcard *.sgml)
NSGMLS	:= $(shell if command -v onsgmls 2>/dev/null; then	 \
			:					;\
		   elif command -v nsgmls 2>/dev/null; then	 \
			:					;\
		   else						 \
			echo "false"				;\
			echo "warning: no nsgmls found" >&2	;\
		   fi)

NSGMLSARGS=

.PHONY: validate
validate:	$(addsuffix .nsgmls,$(SRCS))

%.nsgmls   : %
	$(NSGMLS) -E1 -wall $(NSGMLSARGS) -gues $<

.PHONY: clean
clean:
	rm -f *.html *~

