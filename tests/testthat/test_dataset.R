library(dplyr)
library(tibble)

set_topic("STATE", "/tmp/glimmer/STATE")
set_topic("CACHE", "/tmp/glimmer/CACHE")

if(fs::dir_exists("/tmp/glimmer/STATE")) {
  fs::dir_delete(get_path("STATE"))
}

if(fs::dir_exists("/tmp/glimmer/CACHE")) {
  fs::dir_delete(get_path("CACHE"))
}

test_that("写入一个简单的文件", {
  ds_remove_path("车数据")
  mtcars |> as_tibble() |>
    mutate(cyl = as.integer(cyl)) |>
    glimmer::ds_write("车数据")
  ds_read("车数据") |> nrow() |>
    testthat::expect_equal(nrow(mtcars))
  
  fs::dir_delete(get_path("CACHE"))
})

test_that("按分区写入文件：无关键列", {
  ds_remove_path("dd")
  d1 <- tibble(n = 1:3, p = c("a", "a", "b"))
  d2 <- tibble(n = 2:4, p = c("a", "b", "b"))
  d1 |> glimmer::ds_write("dd", partColumns = c("p"))
  ds_read("dd") |> nrow() |> testthat::expect_equal(nrow(d1))
  d2 |> glimmer::ds_write("dd", partColumns = c("p"))
  ds_read("dd") |> nrow() |> testthat::expect_equal(6)
  
  fs::dir_delete(get_path("CACHE"))
})

test_that("按分区写入文件：默认更新旧数据", {
  ds_remove_path("dd")
  d1 <- tibble(n = 1:3, p = c("a", "a", "b"), x = 11:13)
  d2 <- tibble(n = 2:4, p = c("a", "b", "b"), x = 21:23)
  d1 |> glimmer::ds_write("dd", partColumns = c("p"), keyColumns = c("n"))
  ds_read("dd") |> nrow() |> testthat::expect_equal(nrow(d1))
  d2 |> glimmer::ds_write("dd", partColumns = c("p"), keyColumns = c("n"))
  ds_read("dd") |> nrow() |> testthat::expect_equal(4)
  (ds_read("dd") |> collect() |> arrange(x))$x |> testthat::expect_equal(c(11, 21:23))
  
  fs::dir_delete(get_path("CACHE"))
})

test_that("按分区写入文件：设置为追加模式（不更新旧数据）", {
  ds_remove_path("dd")
  d1 <- tibble(n = 1:3, p = c("a", "a", "b"), x = 11:13)
  d2 <- tibble(n = 2:4, p = c("a", "b", "b"), x = 21:23)
  d1 |> glimmer::ds_write("dd", partColumns = c("p"), keyColumns = c("n"))
  ds_read("dd") |> nrow() |> testthat::expect_equal(nrow(d1))
  d2 |> glimmer::ds_write("dd", partColumns = c("p"), keyColumns = c("n"), mode = "append")
  ds_read("dd") |> nrow() |> testthat::expect_equal(4)
  (ds_read("dd") |> collect() |> arrange(x))$x |> testthat::expect_equal(c(11:13, 23))
  
  fs::dir_delete(get_path("CACHE"))
})

test_that("数据集准备异常时尝试写入", {
  ds_remove_path("车数据")
  tibble() |> glimmer::ds_write("车数据") |>
    testthat::expect_warning("Empty Dataset")
  
  c("abc") |> glimmer::ds_write("车数据") |>
    testthat::expect_error("Not Tibble")
  
  mtcars |> filter(cyl == 1000) |> glimmer::ds_write("车数据") |>
    testthat::expect_warning("No Content")
})

test_that("更新部分文件分区", {
  ds_remove_path("车数据")
  all <- mtcars |> as_tibble() |>
    mutate(cyl = as.integer(cyl), am = as.integer(am))
  
  ## 提前写入一个分区数据
  all |> filter(cyl == 6 & am == 1) |>
    glimmer::ds_write("车数据", partColumns = c("cyl", "am"))
  part1 <- (fs::file_info(get_path("CACHE", "车数据", "cyl=6/am=1/part-0.parquet")))$modification_time
  # Sys.sleep(1)

  ## 将数据写入不重叠的另一个分区，此时不应更新旧文件
  all |> filter(cyl == 4 & am == 1) |>
    glimmer::ds_write("车数据", partColumns = c("cyl", "am"))
  part2 <- (fs::file_info(get_path("CACHE", "车数据", "cyl=6/am=1/part-0.parquet")))$modification_time
  testthat::expect_equal(part1, part2)

  ## 将数据写入重叠的分区，此时应更新旧文件
  all |> filter(cyl == 6) |>
    glimmer::ds_write("车数据", partColumns = c("cyl", "am"))
  part3 <- (fs::file_info(get_path("CACHE", "车数据", "cyl=6/am=1/part-0.parquet")))$modification_time
  testthat::expect_gt(part3, part1)
  
  fs::dir_delete(get_path("CACHE"))
})

test_that("当关键列所在分区被修改", {
  ds_remove_path("车数据")
  all <- mtcars |> as_tibble() |>
    mutate(cyl = as.integer(cyl), am = as.integer(am)) |>
    mutate(id = row_number())
  
  ## 提前写入一个分区数据
  all |> filter(cyl == 6 & am == 1) |>
    glimmer::ds_write("车数据", partColumns = c("cyl", "am"), keyColumns = "id")
  part1 <- (fs::file_info(get_path("CACHE", "车数据", "cyl=6/am=1/part-0.parquet")))$modification_time
  # Sys.sleep(1)
  
  ## 将数据写入不重叠的另一个分区，此时不应更新旧文件
  all |> filter(id %in% c(3:4)) |>
    glimmer::ds_write("车数据", partColumns = c("cyl", "am"), keyColumns = "id")
  part2 <- (fs::file_info(get_path("CACHE", "车数据", "cyl=6/am=1/part-0.parquet")))$modification_time
  testthat::expect_equal(part1, part2)
  
  ## 根据分区写入重叠的分区，此时应更新旧文件
  all |> filter(cyl == 6) |>
    glimmer::ds_write("车数据", partColumns = c("cyl", "am"), keyColumns = "id")
  part3 <- (fs::file_info(get_path("CACHE", "车数据", "cyl=6/am=1/part-0.parquet")))$modification_time
  testthat::expect_gt(part3, part1)
  
  ## 根据主键写入重复的分区，此时应更新旧文件
  all |> filter(id == 2) |>
    glimmer::ds_write("车数据", partColumns = c("cyl", "am"), keyColumns = "id")
  part4 <- (fs::file_info(get_path("CACHE", "车数据", "cyl=6/am=1/part-0.parquet")))$modification_time
  testthat::expect_gt(part4, part1)
  
  fs::dir_delete(get_path("CACHE"))
})

test_that("内存中对数据去重", {
  all <- mtcars |> as_tibble() |>
    mutate(cyl = as.integer(cyl), am = as.integer(am)) |>
    mutate(id = row_number())
  
  rbind(all |> slice(1:3), all |> slice(4:6)) |>
    ds_as_unique("id") |>
    nrow() |>
    testthat::expect_equal(6)
    
  rbind(all |> slice(1:3), all |> slice(3:5)) |>
    ds_as_unique("id") |>
    nrow() |>
    testthat::expect_equal(5)
})

test_that("写入磁盘时对数据去重", {
  ds_remove_path("车数据")
  all <- mtcars |> as_tibble() |>
    mutate(cyl = as.integer(cyl), am = as.integer(am)) |>
    mutate(id = row_number())
  
  ## 写入3条
  all |> filter(id %in% 1:3) |>
    glimmer::ds_write("车数据", partColumns = c("cyl", "am"))
  glimmer::ds_read("车数据") |> nrow() |> testthat::expect_equal(3)

  ## 不指定关键列重复写入，应有6条
  all |> filter(id %in% 1:3) |>
    glimmer::ds_write("车数据", partColumns = c("cyl", "am"))
  glimmer::ds_read("车数据") |> nrow() |> testthat::expect_equal(6)
  
  ## 指定关键列重复，应有3条
  all |> filter(id %in% 1:3) |>
    glimmer::ds_write("车数据", partColumns = c("cyl", "am"), keyColumns = "id")
  glimmer::ds_read("车数据") |> nrow() |> testthat::expect_equal(3)
  
  ## 指定关键列重复，应有4条
  all |> filter(id %in% 2:4) |>
    glimmer::ds_write("车数据", partColumns = c("cyl", "am"), keyColumns = "id")
  glimmer::ds_read("车数据") |> nrow() |> testthat::expect_equal(4)
  
  fs::dir_delete(get_path("CACHE"))
})

test_that("提取读取重写过分区的数据集文件", {
  ds_remove_path("车数据")
  all <- mtcars |> as_tibble() |>
    mutate(cyl = as.integer(cyl), am = as.integer(am)) |>
    mutate(id = row_number())
  
  ## 提前写入一个分区数据
  all |> filter(cyl == 6 & am == 1) |>
    glimmer::ds_write("车数据", partColumns = c("cyl", "am"), keyColumns = "id")

  ## 将数据写入不重叠的另一个分区，此时不应更新旧文件
  all |> filter(id %in% c(3:4)) |>
    glimmer::ds_write("车数据", partColumns = c("cyl", "am"), keyColumns = "id")
  (state_read("__WRITE_DATASET__") |> filter(dataset == "车数据") |> collect())$affected[[1]] |>
  ds_read_affected() |>
  nrow() |>
  testthat::expect_equal(2)
  
  all |> filter(id %in% c(7:9)) |>
    glimmer::ds_write("车数据", partColumns = c("cyl", "am"), keyColumns = "id")
  (state_read("__WRITE_DATASET__") |> filter(dataset == "车数据") |> collect())$affected[[1]] |>
    ds_read_affected() |>
    nrow() |>
    testthat::expect_equal(3)
  
  fs::dir_delete(get_path("CACHE"))
})

test_that("提取最近一次重写过分区的数据集文件", {
  ds_remove_path("车数据")
  all <- mtcars |> as_tibble() |>
    mutate(cyl = as.integer(cyl), am = as.integer(am)) |>
    mutate(id = row_number())
  
  ## 提前写入一个分区数据
  all |> filter(cyl == 6 & am == 1) |>
    glimmer::ds_write("车数据", partColumns = c("cyl", "am"), keyColumns = "id")
  
  ## 将数据写入不重叠的另一个分区，此时不应更新旧文件
  all |> filter(id %in% c(3:4)) |>
    glimmer::ds_write("车数据", partColumns = c("cyl", "am"), keyColumns = "id")
  
  ## 交互环境下，快速定位受影响数据行
  ds_last_affected() |>
    nrow() |>
    testthat::expect_equal(2)

  ## 在有可能并发执行的情况下，指定数据集快速获取受影响行
  ds_last_affected("车数据") |>
    nrow() |>
    testthat::expect_equal(2)
  
  fs::dir_delete(get_path("CACHE"))
})