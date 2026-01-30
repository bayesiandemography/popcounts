
suppressPackageStartupMessages({
  library(dplyr)
  library(poputils)
  library(agetime)
  library(command)
})

cmd_assign(.out = "data/booth_standard1.rda")

booth_standard_5 <- poputils::booth_standard


s <- splinefun(x = age_mid(booth_standard_5$age),
               y = log(booth_standard_5$value),
               method = "natural")

booth_standard_1 <- tibble(age = 10:49,
                           value = exp(s(age))) |>
  mutate(value = value / sum(value))
  
save(booth_standard_1, file = .out, compress = "bzip2")
