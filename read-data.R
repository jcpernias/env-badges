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
    .default = "numeric"))

# Funciones para leer los datos de un año
drop_na <- function(x) {
  x[!is.na(x)]
}

read_dgt_file <- function(year, input_file, dict)
{
  # El año 2021 hay una variable menos
  if(year == 2021)
    dict <- dict |> filter(column != 57)

  read_xlsx(input_file,
            skip = 1,
            col_types = dict$dgt_col_types,
            col_names = drop_na(dict$var)) |>
    filter(!str_detect(mun, fixed("sin especificar"))) |>
    mutate(year = year)
}

# Lee los datos de todos los años
dgt_data <- pmap(dgt_files,
                 \(year, file) read_dgt_file(year, file, dgt_dict)) |>
  bind_rows()

# Determina los municipios poco poblados
low_pop_thresh <- 1000

low_pop <- dgt_data |>
  summarise(min_pop = min(pop_total), .by = ine_code) |>
  filter(min_pop < low_pop_thresh)

dgt_smpl <- dgt_data |>
  anti_join(low_pop, by = "ine_code")

# Incrementos del parque y de los distintivos

incr <- dgt_smpl |>
  filter(year %in% c(2021, 2024)) |>
  group_by(ine_code) |>
  arrange(year) |>
  mutate(incr_num_all = num_all - lag(num_all),
         incr_badge_0 = badge_0 - lag(badge_0),
         incr_badge_eco = badge_eco - lag(badge_eco),
         incr_badge_c = badge_c - lag(badge_c),
         incr_badge_b = badge_b - lag(badge_b),
         incr_badge_none = badge_none - lag(badge_none)) |>
  ungroup() |>
  filter(year == 2024) |>
  select(ine_code, incr_num_all, incr_badge_0, incr_badge_eco, incr_badge_c,
         incr_badge_b, incr_badge_none)


incr |>
  summarise(na_all = is.na(sum(c(incr_num_all))), .by = ine_code) |>
  filter(na)

incr |>
  filter(!is.na(incr_num_all)) |>
  select(-ine_code) |>
  cor()
