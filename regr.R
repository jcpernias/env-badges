library(tidyverse)
library(plm)
library(lmtest)
library(car)

library(rlang)

library(tinytable)

library(huxtable)
options(huxtable.long_minus = TRUE)

library(modelsummary)
library(broom)

params <- list(
  low_pop_thresh = 2500
)

ine_data <- readRDS("data/ine_data.rds")  |>
  filter(!reg_name %in% c("Balears (Illes)", "Canarias",
                          "Ceuta", "Melilla"))

mun_low_pop <- ine_data |>
  summarise(min_pop = min(pop), .by = mun_code) |>
  filter(min_pop < params$low_pop_thresh) |>
  pull(mun_code)

dgt_data <- readRDS("data/dgt_mun_data.rds") |>
  select(-starts_with("pop_"))

mun_missing_dgt <- dgt_data |>
  filter(is.na(fleet) | is.na(lic_all)) |>
  pull(ine_code) |> unique()

ev_data <- readRDS("data/ecovidrio_data.rds") |>
  select(year, kg, mun_code)

no_ev_data <- ev_data |>
  filter(kg == 0) |>
  pull(mun_code) |> unique()

db <- ine_data |>
  left_join(dgt_data,
            by = join_by(mun_code == ine_code, year)) |>
  left_join(select(ev_data, year, kg, mun_code),
            by = join_by(mun_code, year)) |>
  filter(!mun_code %in% unique(c(mun_missing_dgt, mun_low_pop, no_ev_data))) |>
  mutate(fleet2 = badge_0 + badge_eco + badge_c + badge_b + badge_none,
         pct_0 = badge_0 / fleet2 * 100,
         pct_eco = badge_eco / fleet2 * 100,
         pct_c = badge_c / fleet2 * 100,
         pct_b = badge_b / fleet2 * 100,
         pct_none = badge_none / fleet2 * 100,
         pc_0 = badge_0 / pop,
         pc_eco = badge_eco / pop,
         pc_c = badge_c / pop,
         pc_b = badge_b / pop,
         pc_none = badge_none / pop,
         lpc_0    = log(pc_0),
         lpc_eco  = log(pc_eco),
         lpc_c    = log(pc_c),
         lpc_b    = log(pc_b),
         lpc_none = log(pc_none),
         pct_pop_lt_18 = pop_lt_18 / pop * 100,
         pct_pop_gt_65 = pop_gt_65 / pop * 100,
         pct_pop_native = pop_native / pop * 100,
         pct_women = pop_women / pop * 100,
         pct_lic = lic_all / pop * 100,
         pct_lic_women = lic_women / pop_women * 100,
         pct_prim = ed_prim / pop * 100,
         pct_sec_low = ed_sec_low / ed_all * 100,
         pct_sec_high = ed_sec_high / ed_all * 100,
         pct_comp = pct_sec_low + pct_prim,
         pct_sup = ed_sup / ed_all * 100,
         y2022 = if_else(year == 2022, 1, 0),
         y2023 = if_else(year == 2023, 1, 0),
         linc = log(ninc_hh),
         pct_pop_imm = 100 - pct_pop_native,
         lkg_pc = log(kg/pop))


update_lhs <- function(frml, ...) {
  args <- ensyms(...)
  labels <- map_chr(args, as_label)
  map(args, \(x) rlang::`f_lhs<-`(frml, x)) |>
    setNames(labels)
}

fe_vcov <- function(mod) {
  plm::vcovHC(mod, method = "arellano", type = "HC2")
}

fe_est <- function(frml, data, vcov = NULL) {
  x <- plm(frml, data = data)
  vx <- NULL
  if (!is.null(vcov)) {
    vx <- vcov(x)
  }
  list(fit = x, vcov = vx)
}

regr_table <- function(rlist) {
  rlist |>
    map(\(x) coeftest(x$fit, vcov. = x$vcov, save = TRUE)) |>
    huxreg(stars = c(`***` = 0.01, `**` = 0.05, `*` = 0.10),
           statistics = c(R2 = "r.squared")) |>
    set_width(0.80)
}

frml <-  ~ y2022 + y2023 +
  pct_women +
  pct_comp + pct_sec_high +
  linc +
  pct_pop_lt_18 + pct_pop_gt_65 +
  log(hh_size) + hh1_pct +
  pct_pop_imm + lkg_pc

reg_db <-  db |>
  pdata.frame(index = c("mun_code", "year"), row.names = FALSE)


regr_list <-
  update_lhs(frml, pct_0, pct_eco, pct_c, pct_b, pct_none) |>
  map(\(frml) fe_est(frml, data = reg_db, vcov = fe_vcov))


regr_list |> regr_table()
