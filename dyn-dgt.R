library(tidyverse)
library(modelsummary)

pop_threshold <- 10000

dgt_db <- readRDS("data/dgt_mun_data.rds")

smpl_mun <- dgt_db |>
  filter(pop_total > pop_threshold) |>
  count(ine_code) |>
  filter_out(n != 5) |>
  pull(ine_code)

smpl_db <- dgt_db |>
  filter(ine_code %in% smpl_mun) |>
  mutate(pc_none = badge_none / pop_total,
         pc_b    = badge_b    / pop_total,
         pc_c    = badge_c    / pop_total,
         pc_eco  = badge_eco  / pop_total,
         pc_0    = badge_0    / pop_total)

pc_db <- smpl_db |>
  select(ine_code, year, starts_with("pc_"))


pc_db |>
  datasummary((pc_none + pc_b + pc_c + pc_eco + pc_0) * year ~ Mean + SD,
              data = _)

pc_db |>
  filter(year == 2021) |>
  datasummary_correlation()

pc_db |>
  filter(year == 2022) |>
  datasummary_correlation()

pc_db |>
  filter(year == 2023) |>
  datasummary_correlation()

pc_db |>
  filter(year == 2024) |>
  datasummary_correlation()

pc_db |>
  filter(year == 2025) |>
  datasummary_correlation()


l_year <- pc_db |>
  count(year) |>
  mutate(l_year = factor(as.integer(levels(year)) - 1, levels = levels(year))) |>
  filter_out(is.na(l_year)) |>
  select(-n)

lag_names <- c("ine_code", paste0("l_", names(pc_db)[-1]))


pc_dyn_db <- pc_db |>
  left_join(l_year, by = join_by(year)) |>
  left_join(setNames(pc_db, lag_names),
            by = join_by(ine_code, l_year)) |>
  mutate(d_pc_none = pc_none - l_pc_none,
         d_pc_b    = pc_b    - l_pc_b,
         d_pc_c    = pc_c    - l_pc_c,
         d_pc_eco  = pc_eco  - l_pc_eco,
         d_pc_0    = pc_0    - l_pc_0,
         g_pc_none = log(pc_none / l_pc_none),
         g_pc_b    = log(pc_b    / l_pc_b),
         g_pc_c    = log(pc_c    / l_pc_c),
         g_pc_eco  = log(pc_eco  / l_pc_eco),
         g_pc_0    = log(pc_0    / l_pc_0),
         c_pc_none = d_pc_none / l_pc_none,
         c_pc_b    = d_pc_b    / l_pc_b,
         c_pc_c    = d_pc_c    / l_pc_c,
         c_pc_eco  = d_pc_eco  / l_pc_eco,
         c_pc_0    = d_pc_0    / l_pc_0,
  ) |>
  filter_out(is.na(l_year))


X <- ~ l_pc_none + l_pc_b + l_pc_c + l_pc_eco + l_pc_0 + year

eq_none <- lm(update(X, g_pc_none ~ .),
              data = pc_dyn_db)

eq_b <- lm(update(X, g_pc_b ~ .),
           data = pc_dyn_db)

eq_c <- lm(update(X, g_pc_c ~ .),
           data = pc_dyn_db)

eq_eco <- lm(update(X, g_pc_eco ~ .),
             data = pc_dyn_db)

eq_0 <- lm(update(X, g_pc_0 ~ .),
           data = pc_dyn_db)

models <- list("None" = eq_none,
               "B" = eq_b,
               "C" = eq_c,
               "ECO" = eq_eco,
               "0" = eq_0)

modelsummary(models,
             stars = c('*' = .1, '**' = 0.05, '***' = 0.01),
             estimate = "{estimate}{stars}",
             vcov = ~ ine_code)


pars <- map(models, \(x) as_tibble_row(coef(x))) |>
  bind_rows()


B <- pars |>
  select(starts_with("l_pc_")) |>
  as.matrix()

C <- pars |>
  select(-starts_with("l_pc_")) |>
  as.matrix()

solve(-B, C)

eigen(diag(5) + B)

# library(plm)
# pc_db_plm <- pdata.frame(pc_db, index = c("ine_code", "year"), row.names = FALSE)

# d_pc <- diff(pc_db_plm)
