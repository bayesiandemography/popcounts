
suppressPackageStartupMessages({
  library(dplyr)
  library(poputils)
  library(command)
})

cmd_assign(.out = "data/west_level_12.rda")

west_level_12 <- poputils::west_lifetab |>
  filter(level == 12) |>
  select(sex, age, lx)

save(west_level_12, file = .out, compress = "bzip2")
