library(dplyr)

# Lee los datos municipales de la DGT
dgt_data <- readRDS("data/dgt_mun_data.rds")

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
  mutate(
    incr_num_all = num_all - lag(num_all),
    incr_badge_0 = badge_0 - lag(badge_0),
    incr_badge_eco = badge_eco - lag(badge_eco),
    incr_badge_c = badge_c - lag(badge_c),
    incr_badge_b = badge_b - lag(badge_b),
    incr_badge_none = badge_none - lag(badge_none)
  ) |>
  ungroup() |>
  filter(year == 2024) |>
  select(
    ine_code, incr_num_all, incr_badge_0, incr_badge_eco, incr_badge_c,
    incr_badge_b, incr_badge_none
  )


incr |>
  summarise(na_all = is.na(sum(c(incr_num_all))), .by = ine_code) |>
  filter(na_all) |>
  left_join(dgt_smpl |> select(ine_code, mun, pop_total), by = "ine_code") |>
  print(n = 50)

incr |>
  filter(!is.na(incr_num_all)) |>
  select(-ine_code) |>
  cor()
