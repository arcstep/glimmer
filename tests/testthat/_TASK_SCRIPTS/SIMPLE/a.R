library(tibble)
library(tidyr)
library(dplyr)
x <- tibble(age = c(5,6,5,3,8), name = c("liyihan", "xueyile", "wangzixin", "chenzile", "adi"))
x |> arrange(desc(age))