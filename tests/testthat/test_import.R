library(dplyr, warn.conflicts = F)
library(tibble, warn.conflicts = F)
library(lubridate, warn.conflicts = F)

config_init(tempdir())

## clear
clear_dir <- function() {
  get_path("IMPORT") |> remove_dir()
  get_path("CACHE") |> remove_dir()
  get_path("TASK_DEFINE") |> remove_dir()
  get_path("TASK_SCRIPTS") |> remove_dir()
}
clear_dir()

## IMPORT
path_import_1A <- get_path("IMPORT", "schedual_1", "A", "联系人.csv")
path_import_2A <- get_path("IMPORT", "schedual_2", "A", "联系人.csv")
path_import_2B <- get_path("IMPORT", "schedual_2", "B", "生日礼物.csv")
path_import_2C <- get_path("IMPORT", "schedual_2", "C", "狗粮.csv")
c(
  path_import_1A,
  path_import_2A,
  path_import_2B,
  path_import_2C
) |> fs::path_dir() |> fs::dir_create()

tibble(
  name = c("xueyile", "liyihan"),
  age = c(6L, 5L)
) |> readr::write_excel_csv(path_import_1A)

tibble(
  name = c("chenzile", "wangzixin"),
  age = c(6L, 4L)
) |> readr::write_excel_csv(path_import_2A)

tibble(
  name = c("xueyile", "liyihan"),
  birthday = c(as_date("2015-03-15"), as_date("2016-08-10"))
) |> readr::write_excel_csv(path_import_2B)

tibble(
  name = c("A", "BB"),
  price = c(60, 40)
) |> readr::write_excel_csv(path_import_2C)

## TASK_SCRIPTS
path_a <- get_path("TASK_SCRIPTS", "A", "a.R")
path_b <- get_path("TASK_SCRIPTS", "A", "b.R")
path_c <- get_path("TASK_SCRIPTS", "A", "c.R")
c(path_a, path_b) |> fs::path_dir() |> fs::dir_create()

## a.R
'
library(tibble)
library(tidyr)
library(dplyr)
x <- tibble(age = c(5,6,5,3,8), name = c("liyihan", "xueyile", "wangzixin", "chenzile", "adi"))
x |> arrange(desc(age))
' |> write(path_a)
## b.R
'x |> mutate(id = paste0(name, "-", age))' |> write(path_b)

test_that("列举导入素材", {
  import_folders_all() |> nrow() |>
    testthat::expect_equal(4)
})

test_that("扫描未导入素材", {
  import_dataset_init()
  queue_dataset_init()
  
  ## 定义任务
  ## 每个导入任务，还应当负责更新素材的状态数据
  task_create(taskId = "A", taskType = "__TYPE_IMPORT__")
  task_item_add(taskId = "A", taskScript = path_a, scriptType = "file")
  
  ## 扫描文件夹
  import_folders_scan()
  ## 生成任务队列
  import_task_gen()
  
  ## 批量执行所有队列任务
  ##
  queue_run()
})