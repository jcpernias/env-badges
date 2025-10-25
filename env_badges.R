library(tidyverse)

# Lee los datos municipales de la DGT
# Excluye municipios fuera de la península:
# - Balears (Illes)
# - Ceuta
# - Melilla
# - Canarias
# Solo se consideran los datos de 2021 a 2023
dgt_data <- readRDS("data/dgt_mun_data.rds") |>
  filter(!region %in% c("Balears (Illes)", "Ceuta", "Melilla", "Canarias")) |>
  filter(year != 2024)


# Determina los municipios poco poblados
low_pop_thresh <- 1000

low_pop <- dgt_data |>
  summarise(min_pop = min(pop_total), .by = ine_code) |>
  filter(min_pop < low_pop_thresh)

dgt_smpl <- dgt_data |>
  anti_join(low_pop, by = "ine_code")

# Municipios con datos ausentes en 2021
miss_mun <- dgt_smpl |>
  filter(year == 2021, is.na(num_all)) |>
  pull(ine_code)

dgt_smpl <- dgt_smpl |>
  filter(!ine_code %in% miss_mun)

# Incrementos del parque y de los distintivos
incr <- dgt_smpl |>
  filter(year %in% c(2021, 2023)) |>
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
  filter(year == 2023) |>
  select(
    ine_code, incr_num_all, incr_badge_0, incr_badge_eco, incr_badge_c,
    incr_badge_b, incr_badge_none
  )

# Revisa municipios con incrementos NA
incr |>
  summarise(na_all = is.na(sum(c(incr_num_all))), .by = ine_code) |>
  filter(na_all) |>
  left_join(dgt_smpl |> select(ine_code, mun, pop_total), by = "ine_code") |>
  print(n = 50)

# Correlaciones entre incrementos
incr |>
  select(-ine_code) |>
  cor() |>
  round(2)

dgt_smpl <- dgt_smpl |>
  left_join(readRDS("data/ine_atlas_data.rds"),
            by = join_by(ine_code == mun_code, year))

educ_db <- readRDS("data/ine_educ_data.rds") |>
  filter(educ != "ed_all", sex == "sex_all", age == "age_all") |>
  select(-c(sex, age)) |>
  pivot_wider(names_from = educ, values_from = value)

loc_db <- readRDS("data/location_data.rds")

reg_db <- dgt_smpl |>
  left_join(educ_db, by = join_by(ine_code == mun_code, year)) |>
  left_join(loc_db, by = join_by(ine_code, year)) |>
  mutate(pct_0 = badge_0 / num_all * 100,
         pct_eco = badge_eco / num_all * 100,
         pct_c = badge_c / num_all * 100,
         pct_b = badge_b / num_all * 100,
         pct_none = badge_none / num_all * 100,
         pc_0 = badge_0 / pop_total * 100,
         pc_eco = badge_eco / pop_total * 100,
         pc_c = badge_c / pop_total * 100,
         pc_b = badge_b / pop_total * 100,
         pc_none = badge_none / pop_total * 100,
         pct_women = pop_women / pop_total * 100,
         pct_women_drv = driv_women / driv_all * 100,
         pct_sec_low = ed_sec_low / pop_total * 100,
         pct_sec_high = ed_sec_high / pop_total * 100,
         pct_sup = ed_sup / pop_total * 100,
         y2022 = if_else(year == 2022, 1, 0),
         y2023 = if_else(year == 2023, 1, 0),
         linc = log(netinch))


library(plm)

plm(pct_b ~ y2022 + y2023 + pct_women +
      pct_sec_low + pct_sec_high + pct_sup +
      linc + gini + log(avgage) + lt18pct + gt65pct +
      hhsize + natpct + location,
    data = reg_db, index = c("ine_code", "year")) |>
  summary(vcov = vcovHC)

plm(log(num_all) ~ y2022 + y2023 + pct_women +
      pct_sec_low + pct_sec_high + pct_sup +
      linc + gini + log(avgage) + lt18pct + gt65pct +
      hhsize + natpct + location,
    data = reg_db, index = c("ine_code", "year")) |>
  summary(vcov = vcovHC)


lm(pct_eco ~ y2022 + y2023 + linc + log(avgage), data = reg_db) |>
  summary()

lm(pct_c ~ y2022 + y2023 + linc + gini + log(avgage), data = reg_db) |>
  summary()

lm(pct_b ~ y2022 + y2023 + linc + gini + log(avgage), data = reg_db) |>
  summary()

lm(pct_none ~ y2022 + y2023 + linc + gini + log(avgage), data = reg_db) |>
  summary()


lm(pc_0 ~ y2022 + y2023 + linc, data = reg_db) |>
  summary()

lm(pc_eco ~ y2022 + y2023 + linc, data = reg_db) |>
  summary()

lm(pc_c ~ y2022 + y2023 + linc, data = reg_db) |>
  summary()

lm(pc_b ~ y2022 + y2023 + linc, data = reg_db) |>
  summary()

lm(pc_none ~ y2022 + y2023 + linc, data = reg_db) |>
  summary()

lm(log(num_all) ~ y2022 + y2023 + linc + p8020 + log(avgage), data = reg_db) |>
  summary()
