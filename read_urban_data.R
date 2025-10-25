library(dplyr)
library(readxl)

raw_data_dir <- "data/orig"
xlsx_file <- file.path(raw_data_dir,
                       "listado_municipios_categoria_urbana_2025.xlsx")

urb_db <- read_xlsx(xlsx_file, sheet = "LISTADO") |>
  select(mun_code = `CÓDIGO MUNICIPIO`,
         urb_cat = `CÓDIGO URBANO`,
         marea_code = COD_GAU) |>
  mutate(urb = factor(
    case_when(urb_cat == 0 ~ "Not urban",
              urb_cat == 3 ~ "Small Urban",
              urb_cat == 2 ~ "Urban",
              urb_cat == 1 ~ "Metropolitan area"),
    levels = c("Not urban", "Small Urban", "Urban", "Metropolitan area"))) |>
  select(mun_code, urb, marea_code)

saveRDS(urb_db, file = "data/mun_urb_data.rds", compress = "xz")
