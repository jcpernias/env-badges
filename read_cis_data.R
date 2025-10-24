library(dplyr)
library(haven)
library(purrr)

raw_data_dir <- "data/orig"
spss_files <- tibble(year = c(2021, 2023),
                     file = file.path(raw_data_dir,
                                      c("3344.sav", "3431.sav")))

read_cis_file <- function(input_file, year) {
  skip_levels <- c(
    "Voto nulo",
    "Otros partidos",
    "En blanco",
    "No recuerda",
    "N.C.",
    "N.R.")

  read_spss(input_file)  |>
    transmute(ideol = if_else(ESCIDEOL >= 98,
                              NA_integer_,
                              as.integer(ESCIDEOL)),
              last_vote = as.character(as_factor(RECUVOTOG))) |>
    mutate(year = year) |>
    filter(!is.na(last_vote),
           !is.na(ideol),
           !(last_vote %in% skip_levels))
}


db_cis <- pmap(spss_files,
                 \(year, file) read_cis_file(file, year)) |>
  bind_rows() |>
  group_by(year, last_vote) |>
  summarise(pos = mean(ideol, na.rm = TRUE), .groups = "drop")

saveRDS(db_cis, file = "data/cis_ideol_data.rds", compress = "xz")
