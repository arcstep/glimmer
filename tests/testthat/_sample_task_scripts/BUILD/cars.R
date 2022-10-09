library(tibble)
library(tidyr)
library(dplyr)

mtcars |>
  as_tibble() |>
  tibble::rowid_to_column("id") |>
  ds_write("cars")
