# @param input$filePath 导入素材文件名
# @param input$importTopic 导入素材主题
# @param input$cacheTopic 保存主题
# @param input$taskId 任务ID
# @param input$taskTopic 任务主题
# task_run()

input$path |> purrr::walk(function(f) {
  readr::read_csv(f) |>
    ds_append("score")
})
ds_submit("score")
