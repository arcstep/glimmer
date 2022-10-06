library(dplyr, warn.conflicts = F)
library(tibble, warn.conflicts = F)

config_init(tempdir())

clear_dir <- function() {
  get_path("CACHE") |> remove_dir()
}

test_that("空更新：写入空数据时, 数据目录不受影响", {
  ds_remove_path("车数据")
  mtcars |> as_tibble() |>
    mutate(cyl = as.integer(cyl)) |>
    arrow::write_dataset(
      path = get_path("CACHE", "车数据"),
      format = "parquet",
      partitioning = "cyl",
      version = "2.0",
      basename_template = "abc-{i}.p",
      existing_data_behavior = "delete_matching")
  
  mtcars |> as_tibble() |>
    filter(F) |>
    arrow::write_dataset(
      path = get_path("CACHE", "车数据"),
      format = "parquet",
      partitioning = "cyl",
      version = "2.0",
      existing_data_behavior = "delete_matching")
 arrow::open_dataset(get_path("CACHE", "车数据"))$files |> length() |>
    testthat::expect_equal(3)
  
  tibble() |>
    arrow::write_dataset(
      path = get_path("CACHE", "车数据"),
      format = "parquet",
      # partitioning = "cyl",
      version = "2.0",
      existing_data_behavior = "delete_matching")
  arrow::open_dataset(get_path("CACHE", "车数据"))$files |> length() |>
    testthat::expect_equal(3)

  clear_dir()
})


## delete_matching 模式会覆盖涉及到的分区
## 但不会删除数据中没有涉及到的分区目录
test_that("更新分区：重写数据中包含的分区目录", {
  ds_remove_path("车数据")
  mtcars |> as_tibble() |>
    mutate(cyl = as.integer(cyl)) |>
    arrow::write_dataset(
      path = get_path("CACHE", "车数据"),
      format = "parquet",
      partitioning = "cyl",
      version = "2.0",
      existing_data_behavior = "delete_matching")
 arrow::open_dataset(get_path("CACHE", "车数据"))$files |> length() |>
    testthat::expect_equal(3)

  mtcars |> as_tibble() |>
    mutate(cyl = as.integer(cyl)) |>
    filter(cyl != 8) |>
    arrow::write_dataset(
      path = get_path("CACHE", "车数据"),
      format = "parquet",
      partitioning = "cyl",
      version = "2.0",
      basename_template = "abc-{i}.p",
      existing_data_behavior = "delete_matching")
 arrow::open_dataset(get_path("CACHE", "车数据"))$files |> length() |>
    testthat::expect_equal(3)
  
  clear_dir()
})

## overwrite 模式会覆盖同名文件
## 但不会删除名字不同的数据文件，即使分区相同
test_that("更新文件：重写数据中包含的所有分区内文件", {
  ds_remove_path("车数据")
  mtcars |> as_tibble() |>
    mutate(cyl = as.integer(cyl)) |>
    arrow::write_dataset(
      path = get_path("CACHE", "车数据"),
      format = "parquet",
      partitioning = "cyl",
      version = "2.0",
      existing_data_behavior = "delete_matching")
 arrow::open_dataset(get_path("CACHE", "车数据"))$files |> length() |>
    testthat::expect_equal(3)
  
  mtcars |> as_tibble() |>
    mutate(cyl = as.integer(cyl)) |>
    filter(cyl != 8) |>
    arrow::write_dataset(
      path = get_path("CACHE", "车数据"),
      format = "parquet",
      partitioning = "cyl",
      version = "2.0",
      basename_template = "abcd-{i}.p",
      existing_data_behavior = "overwrite")
 arrow::open_dataset(get_path("CACHE", "车数据"))$files |> length() |>
    testthat::expect_equal(5)
  
  clear_dir()
})

## 局部写入数据时，如果列格式不同会发生什么？
## 列结构不同时，允许新数据文件写入
## 但读取时，会根据第一个（按字母排序）数据文件内的列结构读取所有数据
test_that("更新结构：当新分区数据与旧数据不一致", {
  ds_remove_path("车数据")
  mtcars |> as_tibble() |>
    slice(1:10) |>
    select(cyl, 1:3) |>
    mutate(cyl = as.integer(cyl)) |>
    arrow::write_dataset(
      path = get_path("CACHE", "车数据"),
      format = "parquet",
      partitioning = "cyl",
      version = "2.0",
      existing_data_behavior = "delete_matching")
 arrow::open_dataset(get_path("CACHE", "车数据")) |> head() |> length() |>
    testthat::expect_equal(3)

  mtcars |> as_tibble() |>
    slice(11:20) |>
    select(cyl, 3:6) |>
    mutate(cyl = as.integer(cyl)) |>
    arrow::write_dataset(
      path = get_path("CACHE", "车数据"),
      format = "parquet",
      basename_template = "z-patch-{i}.parquet",
      partitioning = "cyl",
      version = "2.0",
      existing_data_behavior = "overwrite")
 arrow::open_dataset(get_path("CACHE", "车数据")) |> head() |> length() |>
    testthat::expect_equal(3)

  mtcars |> as_tibble() |>
    slice(11:20) |>
    select(cyl, 3:6) |>
    mutate(cyl = as.integer(cyl)) |>
    arrow::write_dataset(
      path = get_path("CACHE", "车数据"),
      format = "parquet",
      basename_template = "a-patch-{i}.parquet",
      partitioning = "cyl",
      version = "2.0",
      existing_data_behavior = "overwrite")
 arrow::open_dataset(get_path("CACHE", "车数据")) |> head() |> length() |>
    testthat::expect_equal(5)
  
  clear_dir()
})

## 支持按列族保存数据
## 1、写数据时分别写入
## 2、读数据时使用left_join、right_join等函数灵活合并结果
test_that("合并读取：将列分为两组存储，合并读取时仍然支持惰性操作", {
  ds_remove_path("车数据")
  ds_remove_path("车数据补充")
  mtcars |>
    rownames_to_column() |>
    select(1:3) |>
    arrow::write_dataset(
      path = get_path("CACHE", "车数据"),
      format = "parquet",
      version = "2.0",
      existing_data_behavior = "delete_matching")
  mtcars |>
    rownames_to_column() |>
    select(1, 4:5) |>
    arrow::write_dataset(
      path = get_path("CACHE", "车数据补充"),
      format = "parquet",
      version = "2.0",
      existing_data_behavior = "delete_matching")
  ## 惰性操作读取
  left_join(
   arrow::open_dataset(get_path("CACHE", "车数据")),
    arrow::open_dataset(get_path("CACHE", "车数据补充")) |> head(5),
    by = "rowname") |>
    filter(cyl != 4) |>
    collect() |>
    length() |>
    testthat::expect_equal(5)

  clear_dir()
})

## 如果在同一时段大量写入，可使用追加模式
## 1、支持快速追加新记录：写入时使用overwrite模式分散文件写入，不需要读取旧有数据
## 2、整理时使用delete_matching模式合并分散的文件，合并时按键值去重
## 3、读取时，根据元数据按键值去重
test_that("追加新增：补充新增记录，读时合并", {
  ds_remove_path("车数据")
  mtcars |> as_tibble() |>
    slice(1) |>
    mutate(cyl = as.integer(cyl)) |>
    arrow::write_dataset(
      path = get_path("CACHE", "车数据"),
      format = "parquet",
      basename_template = paste0(gen_batchNum(), "-{i}.parquet"),
      partitioning = "cyl",
      version = "2.0",
      existing_data_behavior = "overwrite")
  mtcars |> as_tibble() |>
    slice(2:3) |>
    mutate(cyl = as.integer(cyl)) |>
    arrow::write_dataset(
      path = get_path("CACHE", "车数据"),
      format = "parquet",
      basename_template = paste0(gen_batchNum(), "-{i}.parquet"),
      partitioning = "cyl",
      version = "2.0",
      existing_data_behavior = "overwrite")
  mtcars |> as_tibble() |>
    slice(4:10) |>
    mutate(cyl = as.integer(cyl)) |>
    arrow::write_dataset(
      path = get_path("CACHE", "车数据"),
      format = "parquet",
      basename_template = paste0(gen_batchNum(), "-{i}.parquet"),
      partitioning = "cyl",
      version = "2.0",
      existing_data_behavior = "overwrite")
  
 arrow::open_dataset(get_path("CACHE", "车数据"))$files |> length() |>
    testthat::expect_equal(6)
 arrow::open_dataset(get_path("CACHE", "车数据")) |> nrow() |>
    testthat::expect_equal(10)
  
  ## 重新保存后，合并文件
 arrow::open_dataset(get_path("CACHE", "车数据")) |> collect() |>
    arrow::write_dataset(
      path = get_path("CACHE", "车数据"),
      format = "parquet",
      partitioning = "cyl",
      version = "2.0",
      existing_data_behavior = "delete_matching")    
 arrow::open_dataset(get_path("CACHE", "车数据"))$files |> length() |>
    testthat::expect_equal(3)
 arrow::open_dataset(get_path("CACHE", "车数据")) |> nrow() |>
    testthat::expect_equal(10)
  
  clear_dir()
})

## 使用@action标记写入方法：C（Create），U（Update），D（Delete）
## 1、支持快速写如删除记录，写入数据时将其设`@delete`为FALSE，删除设为TRUE
## 2、整理时使用delete_matching模式，剔除标记为删除的记录
## 3、读数据时将其过滤，合并时按删除记录的键值剔除
test_that("追加删除：补充删除记录，读时过滤", {
  ds_remove_path("车数据")
  d <- mtcars |> as_tibble() |>
    rownames_to_column()
  ## 写入时增加`@delete`
  d |> slice(1:10) |>
    mutate(cyl = as.integer(cyl)) |>
    mutate(`@action` = "C") |>
    arrow::write_dataset(
      path = get_path("CACHE", "车数据"),
      format = "parquet",
      basename_template = paste0(gen_batchNum(), "-{i}.parquet"),
      partitioning = "cyl",
      version = "2.0",
      existing_data_behavior = "delete_matching")
 arrow::open_dataset(get_path("CACHE", "车数据")) |> filter(`@action` == "C") |> nrow() |>
    testthat::expect_equal(10)
  
  
  d |> slice(1,3,5) |>
    mutate(cyl = as.integer(cyl)) |>
    mutate(`@action` = "D") |>
    arrow::write_dataset(
      path = get_path("CACHE", "车数据"),
      format = "parquet",
      basename_template = paste0(gen_batchNum(), "-{i}.parquet"),
      partitioning = "cyl",
      version = "2.0",
      existing_data_behavior = "overwrite")
 arrow::open_dataset(get_path("CACHE", "车数据")) |> filter(`@action` == "C") |>
    anti_join(arrow::open_dataset(get_path("CACHE", "车数据")) |> filter(`@action` == "D"), by = "rowname") |>
    collect() |>
    nrow() |>
    testthat::expect_equal(7)
  
  clear_dir()
})

## 很容易实现多次追加更新，但删除只允许一次
test_that("追加更新：补充更新记录，读时过滤", {
  ds_remove_path("车数据")
  d <- mtcars |> as_tibble() |>
    rownames_to_column()
  d |> slice(1:10) |>
    mutate(cyl = as.integer(cyl)) |>
    mutate(`@action` = "C") |>
    arrow::write_dataset(
      path = get_path("CACHE", "车数据"),
      format = "parquet",
      basename_template = paste0(gen_batchNum(), "-{i}.parquet"),
      partitioning = "cyl",
      version = "2.0",
      existing_data_behavior = "delete_matching")
 arrow::open_dataset(get_path("CACHE", "车数据")) |> filter(`@action`=="C") |> nrow() |>
    testthat::expect_equal(10)
  
  
  d |> slice(1,3,5) |>
    mutate(cyl = as.integer(cyl)) |>
    mutate(`@action` = "U") |>
    arrow::write_dataset(
      path = get_path("CACHE", "车数据"),
      format = "parquet",
      basename_template = paste0(gen_batchNum(), "-{i}.parquet"),
      partitioning = "cyl",
      version = "2.0",
      existing_data_behavior = "overwrite")
 arrow::open_dataset(get_path("CACHE", "车数据")) |> filter(`@action` %in% c("C", "U")) |>
    anti_join(arrow::open_dataset(get_path("CACHE", "车数据")) |> filter(`@action`=="U"), by = c("rowname", "@action")) |>
    collect() |>
    nrow() |>
    testthat::expect_equal(10)
  
  clear_dir()
})



## 1、在顶层按__CREATE__、__UPDATE__、__DELETE__等操作分区
## 2、增加元数据字段@lastmodifiedAt
##
## 在已经创建数百万条数据的情况下，只有第3种方法可以实际使用
##
test_that("最佳实践：按CUD操作做顶层分区，及时整理", {
  ds_remove_path("车数据")
  
  x <-mtcars |> as_tibble()
  d <- x
  # A tibble: 256 × 11
  1:3 |> purrr::walk(function(i) {
    d <<- rbind(d, d)
  })

  ## 下面这个示例用于测试较大规模的数据
  # A tibble: 1,048,576 × 11
  # 1:17 |> purrr::walk(function(i) {
  #   d <<- rbind(d, d)
  # })
  d <- d |> rownames_to_column()
  
  ## 写入10条
  d |>
    mutate(cyl = as.integer(cyl)) |>
    mutate(`@action` = "__CREATE__") |>
    mutate(`@lastmodifiedAt` = lubridate::now(tzone = "Asia/Shanghai")) |>
    arrow::write_dataset(
      path = get_path("CACHE", "车数据"),
      format = "parquet",
      chunk_size = 1e4,
      basename_template = paste0("create-", gen_batchNum(), "-{i}.parquet"),
      partitioning = c("@action", "cyl"),
      version = "2.0",
      existing_data_behavior = "overwrite")
 arrow::open_dataset(get_path("CACHE", "车数据")) |> filter(`@action`=="__CREATE__") |> nrow() |>
    testthat::expect_equal(256)
  
  ## 修改3条
  d |> slice(2,4,6) |>
    mutate(cyl = as.integer(cyl)) |>
    mutate(`@action` = "__UPDATE__") |>
    mutate(am = 100) |>
    mutate(`@lastmodifiedAt` = lubridate::now(tzone = "Asia/Shanghai")) |>
    arrow::write_dataset(
      path = get_path("CACHE", "车数据"),
      format = "parquet",
      basename_template = paste0("update-", gen_batchNum(), "-{i}.parquet"),
      partitioning = c("@action", "cyl"),
      version = "2.0",
      existing_data_behavior = "overwrite")
 arrow::open_dataset(get_path("CACHE", "车数据")) |> nrow() |>
    testthat::expect_equal(256+3)
  
  ## 删除3条  
  d |> slice(1,3,5) |>
    mutate(cyl = as.integer(cyl)) |>
    mutate(`@action` = "__DELETE__") |>
    mutate(`@lastmodifiedAt` = lubridate::now(tzone = "Asia/Shanghai")) |>
    arrow::write_dataset(
      path = get_path("CACHE", "车数据"),
      format = "parquet",
      basename_template = paste0("delete-", gen_batchNum(), "-{i}.parquet"),
      partitioning = c("@action", "cyl"),
      version = "2.0",
      existing_data_behavior = "overwrite")
 arrow::open_dataset(get_path("CACHE", "车数据")) |> nrow() |>
    testthat::expect_equal(256+3+3)
  
  ## 再修改2条
  d |> slice(1,3) |>
    mutate(cyl = as.integer(cyl)) |>
    mutate(`@action` = "__UPDATE__") |>
    mutate(am = 200) |>
    mutate(`@lastmodifiedAt` = lubridate::now(tzone = "Asia/Shanghai")) |>
    arrow::write_dataset(
      path = get_path("CACHE", "车数据"),
      format = "parquet",
      basename_template = paste0("update-", gen_batchNum(), "-{i}.parquet"),
      partitioning = c("@action", "cyl"),
      version = "2.0",
      existing_data_behavior = "overwrite")
 arrow::open_dataset(get_path("CACHE", "车数据")) |> nrow() |>
    testthat::expect_equal(256+3+3+2)

  ## 第1种读取方法
 arrow::open_dataset(get_path("CACHE", "车数据")) |>
    collect() |>
    group_by(rowname) |>
    arrange(desc(`@lastmodifiedAt`)) |>
    slice(1) |>
    filter(`@action` != "__DELETE__") |>
    nrow() |>
    testthat::expect_equal(256-1)

  ## 第2种读取方法
  keys <-arrow::open_dataset(get_path("CACHE", "车数据")) |>
    collect() |>
    group_by(rowname) |>
    summarise(`@lastmodifiedAt` = max(`@lastmodifiedAt`))
 arrow::open_dataset(get_path("CACHE", "车数据")) |>
    semi_join(keys, by = c("rowname", "@lastmodifiedAt")) |>
    collect() |>
    filter(`@action` != "__DELETE__") |>
    nrow() |>
    testthat::expect_equal(256-1)
  
  ## 第3种读取方法
  keys <- rbind(
    get_path("CACHE", "车数据", "@action=__UPDATE__") |> 
      arrow::open_dataset(format = "parquet") |>
      collect(),
    get_path("CACHE", "车数据", "@action=__DELETE__") |> 
      arrow::open_dataset(format = "parquet") |>
      collect()) |>
    group_by(rowname) |>
    summarise(`@lastmodifiedAt` = max(`@lastmodifiedAt`))
  rbind(
   arrow::open_dataset(get_path("CACHE", "车数据")) |>
      filter(`@action` == "__CREATE__") |>
      anti_join(keys, by = c("rowname")) |>
      select(`@action`, everything()) |>
      collect(),
   arrow::open_dataset(get_path("CACHE", "车数据")) |>
      filter(`@action` != "__CREATE__") |>
      semi_join(keys, by = c("rowname", "@lastmodifiedAt")) |>
      select(`@action`, everything()) |>
      collect() |>
      filter(`@action` != "__DELETE__")) |>
    nrow() |>
    testthat::expect_equal(256-1)
  
  clear_dir()
})


