# 饼图

    Code
      task_plotly_pie(mtcars, value = "cyl")

---

    Code
      task_plotly_pie2(count(mtcars, cyl), value = "n", pull = c(0.1, 0, 0))

# 柱图

    Code
      task_plotly_bar(count(mtcars, cyl), x = "cyl", y = "n")

# 线图

    Code
      task_plotly_line(count(mtcars, cyl), x = "cyl", y = "n")

# 面积图

    Code
      task_plotly_area(count(mtcars, cyl), x = "cyl", y = "n")

---

    Code
      task_plotly_area(summarise(group_by(mtcars, cyl), displ = mean(disp)), x = "cyl",
      y = "displ")

# 散点图

    Code
      task_plotly_marker(mtcars, x = "mpg", y = "disp", alpha = 0.6)

# 直方图

    Code
      task_plotly_hist(mtcars, x = "mpg")

# 极坐标柱图

    Code
      task_plotly_barpolar(mutate(count(mtcars, cyl), cyl = sprintf("CYL: %d", cyl)),
      theta = "cyl", r = "n")

