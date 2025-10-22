library(dplyr)
library(readr)
library(stringr)
library(purrr)
library(tidyr)

# Códigos INE de los municipios
mun_codes <- read_csv("data/ine_mun_codes.csv",
                      col_types = "cccc") |>
  mutate(mun_code = paste0(prov_code, mun_order))

# En el Atlas siguen apareciendo municipios que ya no
# existen, o que han cambiado de provincia.
#
# 04039    Darrical     Desaparece en 1997 incorporándolse a Alcolea
# 12066    Gatova       Pasa a la provincia de Valencia en 1995
# 15026    Cesuras      Fusión en 2013 con Oza dos Ríos creándose Oza-Cesuras
# 15063    Oza dos Ríos Fusión en 2013 con Cesuras creándose Oza-Cesuras
# 17122    Palmerola    Desaparece en 1991 incorporándose a Les Lloses
# 36011    Cerdedo      Fusión en 2016 con Cotobade creándose Cerdedo-Cotobade
# 36012    Cotobade     Fusión en 2016 con Cerdedo creándose Cerdedo-Cotobade

drop_mun_codes <- c("04039", "12066", "15026", "15063",
                    "17122", "36011", "36012")

# Nombres de las variables del Atlas
var_names <- read_csv("data/ine_atlas_vars.csv",
                      col_types = "cc")

raw_data_dir <- "data/orig"

convert_numbers <- function(x) {
  x |>
    str_remove(fixed(".")) |>
    str_replace(fixed(","), ".") |>
    as.numeric()
}

# Se leen los valores de las variables como texto para
# evitar problemas con los valores ausentes que se codifican
# como la cadena "".
read_atlas_file <- function(csv_file) {
  read_delim(csv_file, delim = ";", col_types = "ccccic",
             na = c("", ".", "..")) |>
    filter(is.na(Distritos), is.na(Secciones)) |>
    filter(!is.na(Total)) |>
    mutate(mun_code = str_sub(Municipios, end = 5),
           value = convert_numbers(Total)) |>
    select(-c(Municipios, Distritos, Secciones, Total)) |>
    filter(!mun_code %in% drop_mun_codes) |>
    rename(var_label = 1, year = Periodo)
}

# Datos del Atlas de distribución de renta de los hogares
# Serie 2015-2023
# Se usan 3 archivos CSV proporcionados por el INE:
# - 30824: Renta
# - 30832: Demografía
# - 37677: Distribución de la renta

csv_files <- file.path(raw_data_dir,
                       paste0(c("30824", "30832", "37677"), ".csv.xz"))

db <- map(csv_files, read_atlas_file) |>
  bind_rows() |>
  left_join(var_names, by = "var_label") |>
  select(-var_label) |>
  pivot_wider(names_from = var, values_from = value)

saveRDS(db, file = "data/ine_atlas_data.rds", compress = "xz")


