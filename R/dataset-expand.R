#' @title 初始化扩展数据集
#' @description 
#' 对数据集实现扩展列或数据标注。
#' @param ... 参考函数 \link{ds_init}
#' @param bindDataset 绑定的数据集
#' @family dataset-expand function
#' @export
ds_expand_init <- function(..., bindDataset) {
  ## 要补充的元数据
  ex <- list("bind" = bindDataset)

  ## 写入配置文件
  ds_yaml_write(..., ex = ex, type = "import")
}
