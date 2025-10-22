library(dplyr)
library(tidyr)
library(readr)
library(readxl)

voting_vars <- read_csv("data/voting_vars.csv",
                        col_types = "ccc")
raw_data_dir <- "data/orig"
voting_file <- file.path(raw_data_dir, "02_201911_1.xlsx")

voting_data <- read_xlsx(voting_file, skip = 6,
                         col_names = voting_vars$var,
                         col_types = voting_vars$type) |>
  mutate(ine_code = formatC(prov_code * 1000 + mun_order,
                            flag = "0", format = "d", width = 5))


voting_data |>
  summarise(across(-c(1:12, ine_code),  ~ sum(., na.rm = TRUE))) |>
  pivot_longer(everything(), names_to = "party", values_to = "votes") |>
  arrange(-votes) |>
  mutate(pct = votes / sum(votes) * 100, cum_pct = cumsum(pct)) |>
  print(n = 100)
