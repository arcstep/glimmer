## 按国务院法定假日调休，修改为节日
holidays_gov <- c(
  ## 2019年度
  ##
  ## 元旦
  sprintf("2018-12-%02d", 30:31), "2019-01-01",
  ## 春节
  sprintf("2019-02-%02d", 4:10),
  ## 清明
  sprintf("2019-04-%02d", 4:6),
  ## 五一
  sprintf("2019-05-%02d", 1),
  ## 端午
  sprintf("2019-06-%02d", 7:9),
  ## 中秋
  sprintf("2019-09-%02d", 13:15),
  ## 国庆
  sprintf("2019-10-%02d", 1:7),
  
  ## 2020年度
  ##
  ## 元旦
  sprintf("2020-01-%02d", 1),
  ## 春节
  sprintf("2020-02-%02d", 24:30),
  ## 清明
  sprintf("2020-04-%02d", 3:5),
  ## 五一
  sprintf("2020-05-%02d", 1:5),
  ## 端午
  sprintf("2020-06-%02d", 25:27),
  ## 中秋、国庆
  sprintf("2019-10-%02d", 1:8),
  
  ## 2021年度
  ##
  ## 元旦
  sprintf("2021-01-%02d", 1:3),
  ## 春节
  sprintf("2021-02-%02d", 11:17),
  ## 清明节
  sprintf("2021-04-%02d", 3:5),
  ## 劳动节
  sprintf("2021-05-%02d", 1:5),
  ## 端午节
  sprintf("2021-06-%02d", 12:14),
  ## 中秋节
  sprintf("2021-09-%02d", 19:21),
  ## 国庆节
  sprintf("2021-10-%02d", 1:7),
  
  ## 2022年度
  ##
  ## 元旦
  sprintf("2021-01-%02d", 1:3),
  ## 春节
  sprintf("2021-01-31"),
  sprintf("2021-02-%02d", 2:6),
  ## 清明节
  sprintf("2021-04-%02d", 3:5),
  ## 劳动节
  sprintf("2021-04-30"),
  sprintf("2021-05-%02d", 1:4),
  ## 端午节
  sprintf("2021-06-%02d", 3:5),
  ## 中秋节
  sprintf("2021-09-%02d", 10:12),
  ## 国庆节
  sprintf("2021-10-%02d", 1:7)
)

## 按国务院法定假日调休，修改为上班日
workdays_gov <- c(
  ## 2019年度
  ##
  ## 国庆节
  c("2019-09-29", "2019-10-12"),
  
  ## 2020年度
  ##
  ## 春节
  c("2020-01-19", "2020-02-30"),
  ## 五一
  c("2020-04-26", "2020-05-09"),
  ## 端午
  c("2020-06-28"),
  ## 中秋、国庆
  c("2020-09-27", "2020-10-10"),
  
  ## 2021年度
  ##
  ## 春节
  c("2021-02-07", "2021-02-20"),
  ## 劳动节
  c("2021-04-25", "2021-05-08"),
  ## 中秋节
  c("2021-09-18"),
  ## 国庆节
  c("2021-09-26", "2021-10-09"),
  
  ## 2022年度
  ##
  ## 春节
  c("2021-01-29", "2021-01-30"),
  ## 清明节
  c("2021-04-02"),
  ## 劳动节
  c("2021-04-24", "2021-05-07"),
  ## 国庆节
  c("2021-10-08", "2021-10-09")
)

#' @title 工作日清单
#' @description
#' 默认为周一至周五为工作日，再根据中国国务院发布的工作日调整政策修正
#'
#' @param fromDay 开始日期
#' @param toDay 截止日期
#' @export
workdays <- function(fromDay = "2019-01-01", toDay = "2022-12-31") {
  ## 生成全年的时间序列
  days <- seq(from = as.Date(fromDay),to = as.Date(toDay),by = 1)
  ## 生成全年的工作日标记序列
  workmarkers <- vector(mode = "logical",length = length(days))
  ## 标记管理的工作日和休息日
  workmarkers[!weekdays.Date(days) %in% c("星期六","星期日", "Saturday","Sunday")] <- T
  workmarkers[weekdays.Date(days) %in% c("星期六","星期日", "Saturday","Sunday")] <- F
  ## 按国务院发布的节假日，修改为节日放假
  holidays_confirm <- as.Date(holidays_gov)
  workmarkers[days %in% holidays_confirm] <- F
  ## 按国务院发布的节假日，调整为工作日
  workdays_confirm <- as.Date(workdays_gov)
  workmarkers[days %in% workdays_confirm] <- T
  ## 返回数据框
  tibble(day = days, workday = workmarkers) |>
    mutate(seq_day = ifelse(workday, 1, 0)) |>
    mutate(cum_day = cumsum(seq_day))
}

#' @title 计算工作日
#' @description
#' 计算数据集中指定两列占用的工作日数量
#' @param d 要补充字段的数据集
#' @param fromDayName 开始日期字段名
#' @param toDayName 截止日期字段名
#' @param fromDay 开始日期
#' @param toDay 截止日期
#' @export
workdays_patch <- function(d, fromDayName, toDayName, fromDay = "2019-01-01", toDay = "2022-12-31") {
  ds <- tibble(
    "from" = d[[fromDayName]] |> lubridate::as_date(),
    "to" =  d[[toDayName]] |> lubridate::as_date()
  )
  wd <- workdays(fromDay, toDay) |> select(day, cum_day)
  ds |>
    left_join(wd, by = c("from"="day")) |>
    rename(`@workday_from` = cum_day) |>
    left_join(wd, by = c("to"="day")) |>
    rename(`@workday_to` = cum_day) |>
    mutate(`@workdays` = as.integer(`@workday_to` - `@workday_from`)) |>
    select(`@workdays`) |>
    cbind(d) |>
    as_tibble()
}
