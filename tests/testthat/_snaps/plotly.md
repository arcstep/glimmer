# 直方图

    Code
      plot_hist(mtcars, x = "mpg")

# 散点图

    Code
      plot_marker(mtcars, x = "mpg", y = "disp", alpha = 0.6)

# 饼图

    Code
      plot_pie(mtcars, value = "cyl")

---

    Code
      plot_pie2(count(mtcars, cyl), value = "n", pull = c(0.1, 0, 0))

# 柱图

    Code
      plot_bar(count(mtcars, cyl), x = "cyl", y = "n")

# 线图

    Code
      plot_line(count(mtcars, cyl), x = "cyl", y = "n")

# 面积图

    Code
      plot_area(count(mtcars, cyl), x = "cyl", y = "n")

---

    Code
      plot_area(summarise(group_by(mtcars, cyl), displ = mean(disp)), x = "cyl", y = "displ")

# 极坐标柱图

    Code
      plot_barpolar(mutate(count(mtcars, cyl), cyl = sprintf("CYL: %d", cyl)), theta = "cyl",
      r = "n")

