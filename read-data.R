library(dplyr)
library(purrr)
library(stringr)
library(readr)
library(readxl)

# Lee los datos municipales de la DGT
raw_data_dir <- "data/orig"
dgt_files <- read_csv("data/dgt_mun_files.csv", col_types = "ic") |>
  mutate(file = file.path(raw_data_dir, file))

dgt_dict <- read_csv("data/dgt_mun_dict.csv", col_types = "icc") |>
  mutate(dgt_col_types = case_when(
    is.na(var) ~ "skip",
    type == "c" ~ "text",
    .default = "numeric"
  ))

# Funciones para leer los datos de un año
drop_na <- function(x) {
  x[!is.na(x)]
}

read_dgt_file <- function(year, input_file, dict) {
  # El año 2021 hay una variable menos
  if (year == 2021) {
    dict <- dict |> filter(column != 57)
  }

  read_xlsx(input_file,
    skip = 1,
    col_types = dict$dgt_col_types,
    col_names = drop_na(dict$var)
  ) |>
    filter(!str_detect(mun, fixed("sin especificar"))) |>
    mutate(year = year)
}

# Lee los datos de todos los años
dgt_data <- pmap(
  dgt_files,
  \(year, file) read_dgt_file(year, file, dgt_dict)
) |>
  bind_rows()

saveRDS(dgt_data, file = "data/dgt_mun_data.rds", compress = "xz")

