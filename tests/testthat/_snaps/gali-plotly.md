# 直方图

    Code
      gali_plot_hist(mtcars, e_x = "mpg")

# 散点图

    Code
      gali_plot_marker(mtcars, e_x = "mpg", e_y = "disp", f1_alpha = 0.6)

# 饼图

    Code
      gali_plot_pie(mtcars, e_value = "cyl")

---

    Code
      gali_plot_pie2(count(mtcars, cyl), e_value = "n", f1s_pull = c(0.1, 0, 0))

# 柱图

    Code
      gali_plot_bar(count(mtcars, cyl), e_x = "cyl", e_y = "n")

# 线图

    Code
      gali_plot_line(count(mtcars, cyl), e_x = "cyl", e_y = "n")

# 面积图

    Code
      gali_plot_area(count(mtcars, cyl), e_x = "cyl", e_y = "n")

---

    Code
      gali_plot_area(summarise(group_by(mtcars, cyl), displ = mean(disp)), e_x = "cyl",
      e_y = "displ")

# 极坐标柱图

    Code
      gali_plot_barpolar(mutate(count(mtcars, cyl), cyl = sprintf("CYL: %d", cyl)),
      e_theta = "cyl", e_r = "n")

