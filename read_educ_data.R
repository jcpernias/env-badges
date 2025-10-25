library(dplyr)
library(readr)
library(stringr)
library(tidyr)
library(rlang)

ine_educ_labels <- read_csv("data/ine_educ_labels.csv", col_types = "ccc")

make_factor <- function(x) {
  labs <- ine_educ_labels |>
    filter(var == as_label(enquo(x)))
  new_levels  <- labs$level
  tbl <- new_levels
  names(tbl) <- labs$ine_label
  factor(tbl[x], levels = new_levels) |>
    unname()
}

# Lee los datos de población municipal por estudios completados,
# tramo de edad y sexo.
# Obtenidos del Censo anual de población publicado en el INE.
# Datos para todos los municipios de España de 500 habitantes o más
# desde 2021 a 2023.
raw_data_dir <- "data/orig"
csv_file <- file.path(raw_data_dir, "66620.csv.xz")
educ_data <- read_delim(csv_file, delim = ";",
                        locale = locale(decimal_mark = ",",
                                        grouping_mark = "."),
                       col_types = "ccccin") |>
  mutate(educ = make_factor(`Nivel de estudios`),
         age = make_factor(Edad),
         sex = make_factor(Sexo),
         mun_code = str_sub(`Municipios de 500 habitantes o más`, end = 5),) |>
  rename(year = Periodo, value = Total) |>
  select(-(1:4))
saveRDS(educ_data, file = "data/ine_educ_data.rds", compress = "xz")

