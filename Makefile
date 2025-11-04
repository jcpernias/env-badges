SHELL := /bin/sh

RSCRIPT := Rscript --no-save --no-restore

src_files := data_dgt data_ine

dgt_data_files := $(addprefix data/orig/DatosMunicipalesGeneral_, \
	$(addsuffix .xlsx, 2021 2022 2023))

ine_atlas_files := $(addprefix data/orig/, \
	$(addsuffix .csv.xz, 30824 30832 37677))

ine_census_files := $(addprefix data/orig/, \
	$(addsuffix .csv.xz, 66620 68065))

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

$(out_dir)/data_ine.html: data/ine_mun_codes.csv \
	data/ine_atlas_vars.csv $(ine_atlas_files) \
	data/ine_educ_labels.csv $(ine_census_files)
