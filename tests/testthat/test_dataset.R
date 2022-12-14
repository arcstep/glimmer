test_that("当数据集配置文件不存在时", {
  sample_init()
  ds_drop("车数据")
  ds_append(mtcars, "车数据") |>
    testthat::expect_error("Empty Dataset Metadata")
  
  temp_remove()
})

test_that("当新数据集结构不一致", {
  sample_init()
  ds_drop("车数据")
  ds_init("车数据", data = mtcars |> head())
  
  ## 接受缺少字段
  mtcars |> select(-2) |> ds_append("车数据")
  (names(mtcars)[[2]] %in% names(ds_read("车数据") |> collect())) |>
    testthat::expect_true()
  
  ## 接受多出字段
  mtcars |> mutate("xx" = "XX") |> ds_append("车数据")
  ("xx" %in% names(ds_read("车数据") |> collect())) |>
    testthat::expect_false()
  
  temp_remove()
})

test_that("当新数据集结构不一致，且缺少关键字段", {
  sample_init()
  ds_drop("车数据")
  m <- mtcars |> as_tibble() |> rownames_to_column("id")
  ds_init("车数据", data = m |> head(), keyColumns = "id", partColumns = "cyl")
  
  ## 缺少主键
  mtcars |> as_tibble() |>
    ds_append("车数据") |>
    testthat::expect_error("No keyColumns")
  
  ## 缺少分区字段
  m |>
    select(-cyl) |>
    ds_append("车数据") |>
    testthat::expect_error("No partColumns")
  
  ## 删除时仅提供主键字段
  m |> ds_append("车数据")
  ds_read("车数据") |> collect() |> nrow() |> testthat::expect_equal(32)
  tibble("id" = "1", "cyl" = 4) |> ds_delete("车数据")
  ds_read("车数据") |> collect() |> nrow() |> testthat::expect_equal(31)

  temp_remove()
})

test_that("当新数据集架构不一致，按要求转换", {
  sample_init()
  ds_drop("AAA")
  ds_init("AAA", schema = list(
    list("fieldName" = "a", "fieldType" = "int32"),
    list("fieldName" = "b", "fieldType" = "timestamp[us, tz=Asia/Shanghai]")))
  
  tibble(a = 10.0, b = lubridate::as_datetime("2022-10-01 10:00:00")) |>
    ds_append("AAA")
  (ds_read("AAA") |> collect())$a |> class() |>
    testthat::expect_equal("integer")
  
  temp_remove()
})

test_that("追加数据：缺少架构描述", {
  sample_init()
  m <- mtcars |> as_tibble() |> rownames_to_column()
  ds_drop("车数据")
  ds_init("车数据", keyColumns = "rowname")
  
  m |> slice(1:10) |> as_tibble() |> ds_append("车数据") |>
    testthat::expect_error("No Schema")
  
  temp_remove()
})

test_that("追加数据：创建新数据", {
  sample_init()
  m <- mtcars |> as_tibble() |> rownames_to_column()
  ds_drop("车数据")
  ds_init("车数据", keyColumns = "rowname", data = m |> head())
  
  m |> slice(1:10) |> as_tibble() |> ds_append("车数据")
  ## ds_read
  ds_read("车数据") |> collect() |> nrow() |>
    testthat::expect_equal(10)
  ## ds_read0
  ds_read0("车数据") |> collect() |> nrow() |>
    testthat::expect_equal(0)
  ## ds_submit
  ds_submit("车数据")
  ds_read0("车数据") |> collect() |> nrow() |>
    testthat::expect_equal(10)
  
    
  m |> slice(1:10) |> as_tibble() |> ds_append("车数据")
  get_path("CACHE", "车数据") |> arrow::open_dataset() |> nrow() |>
    testthat::expect_equal(20)
  ds_read("车数据") |> collect() |> nrow() |>
    testthat::expect_equal(10)
  
  temp_remove()
})

test_that("读取数据：ds_read0 vs ds_read vs arrow::open_dataset", {
  sample_init()
  d <- ds_demo("mpg") |> select(-rowid)
  1:5 |> purrr::walk(function(i) d <<- rbind(d, d))
  d <- tibble::rowid_to_column(d, "rowid")
  ds_drop("cars")
  ds_init("cars", keyColumns = "rowid", data = d)
  d |> ds_append("cars")
  ## ds_read
  ds_read("cars") |>
    collect() |>
    nrow() |>
    testthat::expect_equal(nrow(d))
  ds_read0("cars") |>
    collect() |>
    nrow() |>
    testthat::expect_equal(0)
  arrow::open_dataset(get_path("CACHE", "cars")) |>
    collect() |>
    nrow() |>
    testthat::expect_equal(nrow(d))
  ##
  ds_submit("cars")
  ds_read("cars") |>
    collect() |>
    nrow() |>
    testthat::expect_equal(nrow(d))
  ds_read0("cars") |>
    collect() |>
    nrow() |>
    testthat::expect_equal(nrow(d))
  arrow::open_dataset(get_path("CACHE", "cars")) |>
    collect() |>
    nrow() |>
    testthat::expect_equal(nrow(d))
  
  temp_remove()  
})

test_that("追加数据：修改旧数据", {
  sample_init()
  m <- mtcars |> as_tibble() |> rownames_to_column()
  ds_drop("车数据")
  ds_init("车数据", keyColumns = "rowname", data = m |> head())
  
  m |> slice(1:10) |> as_tibble() |> ds_append("车数据")
  r1 <- ds_read("车数据") |> collect() |> select(mpg) |> head(1)
  ds_read("车数据") |> collect() |> 
    mutate(mpg = mpg + 10) |>
    ds_append("车数据")
  r2 <- ds_read("车数据") |> collect() |> select(mpg) |> head(1)
  testthat::expect_equal(r1$mpg + 10, r2$mpg)
  
  ds_submit("车数据")
  r3 <- ds_read("车数据") |> collect() |> select(mpg) |> head(1)
  testthat::expect_equal(r1$mpg + 10, r3$mpg)
  
  temp_remove()
})

test_that("存取数据：因子类型", {
  sample_init()
  
  ## 按照因子类型存取
  m <- mtcars |>
    as_tibble() |>
    rownames_to_column() |>
    mutate(cyl = forcats::fct_reorder(as.character(cyl), disp))
  ds_drop("车数据")
  ds_init("车数据", keyColumns = "rowname", data = m |> head())
  
  m |> slice(1:10) |> as_tibble() |> ds_append("车数据")
  (ds_yaml_schema("车数据") |> filter(fieldName == "cyl"))$fieldType |>
    testthat::expect_equal("dictionary<values=string, indices=int32>")
  (ds_read("车数据") |> collect())$cyl |> class() |>
    testthat::expect_equal("factor")
  
  ## 无法读取：存储为字符串，读取时转换为因子类型
  ds_drop("车数据")
  ds_init("车数据", keyColumns = "rowname", data = m |> head())
  
  m |> slice(1:10) |> as_tibble() |>
    mutate(cyl = as.character(cyl)) |>
    ds_append("车数据")
  (ds_yaml_schema("车数据") |> filter(fieldName == "cyl"))$fieldType |>
    testthat::expect_equal("dictionary<values=string, indices=int32>")
  (ds_read("车数据") |> collect())$cyl |> class() |>
    testthat::expect_error("Unsupported cast from string to dictionary")
  
  ## 可以兼容：存储为因子类型，读取时转为字符串
  ds_drop("车数据")
  ds_init("车数据", keyColumns = "rowname", data = m |> mutate(cyl = as.character(cyl)))
  
  m |> slice(1:10) |> as_tibble() |>
    ds_append("车数据")
  (ds_yaml_schema("车数据") |> filter(fieldName == "cyl"))$fieldType |>
    testthat::expect_equal("string")
  (ds_read("车数据") |> collect())$cyl |> class() |>
    testthat::expect_equal("character")
  
  temp_remove()
})



test_that("删除数据：没有设置主键时不允许删除", {
  sample_init()
  ## 没有设置主键，删除失败
  ds_drop("车数据")
  ds_init("车数据", data = mtcars |> head())
    mtcars |> slice(1:10) |> as_tibble() |> ds_append("车数据")
  mtcars |> slice(1:3) |> as_tibble() |> ds_delete("车数据") |>
    testthat::expect_error("Can't Delete without keyColumns")

  ## 设置了主键，删除成功
  ds_drop("车数据")
  m <- mtcars |> as_tibble() |> rownames_to_column()
  ds_init("车数据", keyColumns = "rowname", data = m |> head())
  m |> slice(1:10) |> as_tibble() |> ds_append("车数据")
  m |> slice(1:3) |> as_tibble() |> ds_delete("车数据")
  ds_read("车数据") |> collect() |> nrow() |>
    testthat::expect_equal(7)
  
  temp_remove()
})

test_that("归档数据操作：无分区", {
  sample_init()
  ## 设置了主键，删除成功
  ds_drop("车数据")
  m <- mtcars |> as_tibble() |> rownames_to_column()
  ds_init("车数据", keyColumns = "rowname", data = m |> head())
  m |> slice(1:10) |> as_tibble() |> ds_append("车数据")
  m |> slice(1:3) |> as_tibble() |> ds_delete("车数据")
  
  ds_submit("车数据")
  ds_read("车数据") |> collect() |> nrow() |>
    testthat::expect_equal(7)
  
  temp_remove()
})

test_that("归档数据操作：有分区", {
  sample_init()
  ## 设置了主键，删除成功
  ds_drop("车数据")
  m <- mtcars |> as_tibble() |> rownames_to_column() |> mutate(cyl = as.integer(cyl))
  ds_init("车数据", keyColumns = "rowname", partColumns = c("cyl"), data = m |> head())
  m |> slice(1:10) |> as_tibble() |> ds_append("车数据")
  m |> slice(6:15) |> as_tibble() |> ds_append("车数据")
  m |> slice(1:3) |> as_tibble() |> ds_delete("车数据")
  ds_read("车数据") |> collect() |>
    filter(cyl == 4) |>
    mutate(mpg = mpg + 2) |>
    ds_append("车数据")
    
  ds_submit("车数据")
  ds_read("车数据") |> collect() |> nrow() |>
    testthat::expect_equal(10+5-3)
  
  m |> filter(cyl == 4) |> as_tibble() |> ds_append("车数据")
  ds_submit("车数据")
  ds_read("车数据") |> collect() |> nrow() |>
    testthat::expect_equal(21)
  
  temp_remove()
})



test_that("内存中对数据去重", {
  sample_init()
  
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
  
  temp_remove()
})

test_that("读写数据时使用推荐的显示列", {
  sample_init()
  
  all <- mtcars |> as_tibble() |>
    mutate(cyl = as.integer(cyl), am = as.integer(am)) |>
    mutate(id = row_number())
  
  ds_drop("车数据")
  glimmer::ds_init("车数据", data = head(all), keyColumns = "id", suggestedColumns = c("id"))
  all |> glimmer::ds_append("车数据")
  ("id" %in% (glimmer::ds_read("车数据") |> names())) |>
    testthat::expect_true()
  glimmer::ds_read("车数据") |> names() |> length() |>
    testthat::expect_equal(length(all |> names()) + 5)
  
  ds_drop("车数据")
  glimmer::ds_init("车数据2", data = head(all), keyColumns = "id", suggestedColumns = c("id", "cyl"))
  all |> glimmer::ds_append("车数据2")
  (glimmer::ds_read("车数据2") |> names())[[2]] |>
    testthat::expect_equal("cyl")
  
  temp_remove()
})

