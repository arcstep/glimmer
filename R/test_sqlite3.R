## 与SQLite3的性能对比
## SQLite是典型的行存储数据集

library(dplyr, warn.conflicts = F)
library(tibble, warn.conflicts = F)
library(tidyr, warn.conflicts = F)
library(dbplyr, warn.conflicts = F)
library(RSQLite, warn.conflicts = F)
library(DBI)

set_topic("STATE", "/tmp/glimmer/STATE")
set_topic("CACHE", "/tmp/glimmer/CACHE")

prepare_db <- function() {
  d0 <- mtcars |> as_tibble() |> head(20)
  d <- d0
  2:1000 |> purrr::walk(function(i) {
    d <<- rbind(d, d0)
  })
  rbind(d, d, d, d, d)
}


clear_dir <- function() {
  get_path("CACHE") |> remove_dir()
  get_path("STATE") |> remove_dir()
}

bench_sqlite3 <- function() {
  d <- prepare_db()
  
  # SQLite3
  system.time({
    f <- tempfile()
    if(fs::file_exists(f)) {
      fs::file_delete(f)
    }
    con <- dbConnect(RSQLite::SQLite(), dbname = f)
    dbWriteTable(con, "mtcars", d)
  })

  system.time({
    res <- dbSendQuery(con, "SELECT sum(mpg) FROM mtcars")
    dbFetch(res)
  })
  
  ## arrow
  system.time({
    d |> ds_write("mtcars")
  })
  system.time({
    ds_read("mtcars") |> summarise(value = sum(mpg)) |> collect()
  })
  
}
