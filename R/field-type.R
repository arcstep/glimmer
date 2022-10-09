allFields <- list(
  "int8" = function()int8(),
  "int16" = function()int16(),
  "int" = function()int32(),
  "int32" = function()int32(),
  "int64" = function()int64(),
  "uint8" = function()uint8(),
  "uint16" = function()uint16,
  "uint32" = function()uint32(),
  "uint64" = function()uint64(),
  "float16" = function()float16(),
  "halffloat" = function()halffloat(),
  "float32" = function()float32(),
  "float" = function()float(),
  "float64" = function()float64(),
  "double" = function()float64(),
  "logical" = function()boolean(),
  "boolean" = function()boolean(),
  "bool" = function()bool(),
  "utf8" = function()utf8(),
  "large_utf8" = function()large_utf8(),
  "binary" = function()binary(),
  "character" = function()string(),
  "string" = function()string(),
  "dictionary<values=string, indices=int32>" = function()dictionary(index_type = int32(), value_type = utf8(), ordered = FALSE),
  "day" = function()date32(),
  "date32" = function()date32(),
  "date32[day]" = function()date32(),
  "date64" = function()date64(),
  "time32" = function()time32(),
  "time64" = function()time64(),
  "null" = function()null(),
  "timestamp" = function()timestamp(unit = c("s", "ms", "us", "ns")),
  "timestamp[s]" = function()timestamp(unit = c("s", "ms", "us", "ns")),
  "timestamp[us]" = function()timestamp(unit = c("us")),
  "timestamp[us, tz=UTC]" = function()timestamp(unit = c("us")),
  "timestamp[us, tz=Asia/Shanghai]" = function()timestamp(unit = c("us"), timezone = "Asia/Shanghai")
)

#' @title 构造字段对象
#' @export
dt_field <- function(x) allFields[[x]]()

dt_bool <- function() TRUE
dt_int <- function() 1000L
dt_double <- function() 3.14
dt_string <- function() "I_AM_STRING"
dt_date <- function() as_date("2022-10-01", tz = "Asia/Shanghai")
dt_datetime <- function() as_datetime("2022-10-01 08:28:15", tz = "Asia/Shanghai")

