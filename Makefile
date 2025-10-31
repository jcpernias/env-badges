SHELL := /bin/sh

RSCRIPT := Rscript --no-save --no-restore

src_files := data_dgt

dgt_data_files := $(addprefix data/orig/DatosMunicipalesGeneral_, \
	$(addsuffix .xlsx, 2021 2022 2023))

out_dir := _output

qmd_files := $(addsuffix .qmd, $(src_files))
html_files := $(addprefix $(out_dir)/, $(addsuffix .html, $(src_files)))

all: $(html_files)


.PHONY: clean
clean:
	-rm -rf _output

.PHONY: veryclean
veryclean: clean
	-rm data/*.rds

$(out_dir)/%.html: %.qmd
	quarto render $<

$(out_dir)/data_dgt.html: $(dgt_data_files) data/dgt_mun_dict.csv

