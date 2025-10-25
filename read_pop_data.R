library(dplyr)
library(readr)
library(stringr)
library(tidyr)

# Lee los datos de población municipal
# Obtenidos del Censo anual de población publicado en el INE.
# Datos para todos los municipios de España desde 2021 a 2024.
# Población total y por sexo.
raw_data_dir <- "data/orig"
csv_file <- file.path(raw_data_dir, "68065.csv.xz")
pop_data <- read_delim(csv_file, delim = ";",
                       locale = locale(decimal_mark = ",",
                                       grouping_mark = "."),
                       col_types = "_ccin",
                       col_names = c("mun", "sex", "year", "value"),
                       skip = 1) |>
  filter(!is.na(mun)) |>
  mutate(mun_code = str_sub(mun, end = 5)) |>
  select(-mun) |>
  pivot_wider(names_from = sex, values_from = value) |>
  rename(pop = Total, pop_men = Hombres, pop_women = Mujeres)
saveRDS(pop_data, file = "data/ine_pop_data.rds", compress = "xz")


