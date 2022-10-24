language = list(
    "sProcessing" = "处理中...",
    "sLengthMenu" = "显示 _MENU_ 项结果",
    "sZeroRecords" =  "没有匹配结果",
    "sInfo" = "显示第 _START_ 至 _END_ 项结果，共 _TOTAL_ 项",
    "sInfoEmpty" ="显示第 0 至 0 项结果，共 0 项",
    "sInfoFiltered" = "(由 _MAX_ 项结果过滤)",
    "sInfoPostFix" ="",
    "sSearch" = "搜索:",
    "sUrl" = "",
    "sEmptyTable" = "表中数据为空",
    "sLoadingRecords" = "载入中...",
    "sInfoThousands" = ",",
    "oPaginate" = list(
      "sFirst" = "首页",
      "sPrevious" = "上页",
      "sNext" = "下页",
      "sLast" = "末页"
    ),
    "oAria" = list(
      "sSortAscending" = ": 以升序排列此列",
      "sSortDescending" = ": 以降序排列此列"
    ))

#' @title 数据表
#' @examples 
#' mtcars |> gali_DT()
#' @family lib-DT functions
#' @export
gali_DT <- function(`@ds`,
                    b_rownames = FALSE,
                    e_selection = 'none',
                    e_extensions = "Responsive",
                    e_class = 'white-space: nowrap',
                    e_options = list("responsive" = TRUE),
                    ...) {
  xoptions <- e_options
  xoptions$language = language
  `@ds` |> collect() |>
    DT::datatable(rownames = b_rownames, selection = e_selection,
      extensions = e_extensions, class = e_class, options = xoptions, ...)
}

