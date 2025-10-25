library(dplyr)
library(tidyr)
library(readr)
library(readxl)
library(purrr)

# Lee los archivos con los resultados electorales
raw_data_dir <- "data/orig"

xlsx_files <- c("02_201911_1.xlsx",
                "02_202307_1.xlsx")
voting_files <- tibble(xlsx_file = file.path(raw_data_dir, xlsx_files),
                       year = c(2019, 2023))


read_voting_file <- function(xlsx_file, year) {
  read_xlsx(xlsx_file, skip = 5) |>
    mutate(ine_code =
             formatC(`Código de Provincia` * 1000 + `Código de Municipio`,
                     flag = "0", format = "d", width = 5),
           year = year) |>
    select(-c(1:10, 12, 13))
}

voting_data <- pmap(voting_files, read_voting_file)

get_total_votes <- function(db) {
  db |>
    rename(total = `Votos a candidaturas`) |>
    select(ine_code, year, total)
}

total_party_votes <- map(voting_data, get_total_votes) |>
  bind_rows()


get_vote_distr <- function(db) {
  db |>
    select(-1) |>
    pivot_longer(cols = -c(ine_code, year),
                 names_to = "party",
                 values_to = "votes") |>
    filter(votes != 0) |>
    left_join(total_party_votes, by = join_by(ine_code, year)) |>
    mutate(pct = votes / total * 100) |>
    arrange(year, ine_code, -votes) |>
    select(ine_code, year, party, votes, pct)
}


vote_distr <- map(voting_data, get_vote_distr) |>
  bind_rows()


# Lee los datos de ideología del CIS
cis_db <- readRDS("data/cis_ideol_data.rds") |>
  left_join(read_csv("data/party_labels.csv", col_types = "iicc"),
            by = join_by(year, last_vote == cis_label)) |>
  select(-last_vote) |>
  rename(cis_year = year)

locations <- vote_distr |>
  right_join(cis_db, by = join_by(year == voting_year, party == label),
             relationship = "many-to-many") |>
  group_by(cis_year, ine_code) |>
  summarise(location = sum(pos * pct) / 100 - 5.5, .groups = "drop") |>
  rename(year = cis_year)

saveRDS(locations, file = "data/location_data.rds", compress = "xz")
