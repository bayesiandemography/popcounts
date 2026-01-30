
suppressPackageStartupMessages({
  library(readxl)
  library(dplyr)
  library(poputils)
  library(command)
})

cmd_assign(.raw = "data-raw/unpd_2011_mlt_130_2.5y_abridged.xlsx",
           .out = "data/lx_west_7_abridged.rda")

raw <- read_xlsx(path = .raw,
                 sheet = "Sheet1",
                 col_types = c(rep("skip", 2),
                               rep("text", 2),
                               rep("numeric", 2),
                               rep("skip", 2),
                               "numeric",
                               rep("skip", times = 6)))

lx_west_7_abridged <- raw |>
  filter(Type == "CD West") |>
  mutate(is_level_7 = ((Sex == "Female") & (E0 == 35)) |
           ((Sex == "Male") & (E0 == 32.5))) |>
  filter(is_level_7) |>
  filter(age <= 100) |>
  mutate(age = as.integer(age)) |>
  select(sex = Sex, age, lx = lxn)

save(lx_west_7_abridged, file = .out, compress = "bzip2")
