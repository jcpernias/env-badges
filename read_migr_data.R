library(dplyr)
library(readr)
library(stringr)
library(tidyr)

# Lee los datos de población municipal
# Obtenidos del Censo anual de población publicado en el INE.
# Datos para todos los municipios de España desde 2021 a 2024.
# Población total por sexo y nacimiento (grandes grupos).
raw_data_dir <- "data/orig"
csv_file <- file.path(raw_data_dir, "68538.csv.xz")
migr_data <- read_delim(csv_file, delim = ";",
                       locale = locale(decimal_mark = ",",
                                       grouping_mark = "."),
                       col_types = "--cccin",
                       col_names = c("mun", "sex", "birth_place",
                                     "year", "value"),
                       skip = 1) |>
  filter(!is.na(mun)) |>
  filter(!birth_place == "Total") |>
  mutate(mun_code = str_sub(mun, end = 5)) |>
  select(-mun)
saveRDS(migr_data, file = "data/ine_migr_data.rds", compress = "xz")


