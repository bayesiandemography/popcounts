
.PHONY: all
all: ../data/booth_standard_1.rda \
     ../data/booth_standard_5.rda \
     ../data/lx_west_7_abridged.rda \
     ../data/lx_west_7_complete.rda \
     documentation


## Make datasets

../data/booth_standard_1.rda: booth_standard_1.R
	Rscript $^ $@

../data/booth_standard_5.rda: booth_standard_5.R
	Rscript $^ $@

../data/lx_west_7_abridged.rda: lx_west_7_abridged.R \
  unpd_2011_mlt_130_2.5y_abridged.xlsx
	Rscript $^ $@

../data/lx_west_7_complete.rda: lx_west_7_complete.R \
  unpd_2011_mlt_130_2.5y_complete.xlsx
	Rscript $^ $@

## Update help files

.PHONY: documentation
documentation:
	Rscript -e "devtools::document()"


## Clean up

.PHONY: clean
clean:
	rm -rf ../data
	mkdir ../data


