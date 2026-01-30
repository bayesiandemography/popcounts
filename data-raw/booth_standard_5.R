
suppressPackageStartupMessages({
  library(poputils)
  library(command)
})

cmd_assign(.out = "data/booth_standard5.rda")

booth_standard_5 <- poputils::booth_standard

save(booth_standard_5, file = .out, compress = "bzip2")
