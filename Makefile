.PHONY: all

all: data/eur_cad.json data/eur_aud.json data/eur_zar.json data/eur_nok.json

%.json:
	mkdir -p $(dir $@)
	download_candles --instrument $(shell echo $(basename $(notdir $@)) | tr '[:lower:]' '[:upper:]') --granularity H1 --begin 2005 > $@
