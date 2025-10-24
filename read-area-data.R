library(foreign)
library(dplyr)

raw_data_dir <- "data/orig"

pop23_db <- read.dbf(file.path(raw_data_dir, "CifraPob2023.dbf")) |>
  as_tibble() |>
  transmute(mun_code = codmun_ine, pop_23 = pob_23)

dens23_db <- read.dbf(file.path(raw_data_dir, "DensPob2023.dbf")) |>
  as_tibble() |>
  transmute(mun_code = codmun_ine, dens_23 = dens_pob)

area_db <- pop23_db |>
  left_join(dens23_db, by = join_by(mun_code)) |>
  mutate(area = pop_23 / dens_23)  |>
  select(mun_code, area) |>
  arrange(mun_code)

saveRDS(area_db, file = "data/mun_area_data.rds", compress = "xz")
