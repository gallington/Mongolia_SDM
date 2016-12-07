# Fig 4
############################################

# using COWPLOT

# draw_plot(plot, x = 0, y = 0, width = 1, height = 1)
# plot: the plot to place (ggplot2 or a gtable)
# x: The x location of the lower left corner of the plot.
# y: The y location of the lower left corner of the plot.
# width, height: the width and the height of the plot
# 
# The function ggdraw() is used to initialize an empty drawing canvas.
# install.packages("cowplot")
library(cowplot)

legend_scenarios <- get_legend(xg + theme(legend.position="bottom"))

# add the legend underneath the row we made earlier. Give it 10% of the height
# of one plot (via rel_heights).

fig4 <- plot_grid(xg,
           xc ,
           xl ,
           xr,
           align = 'vh',
           labels = c("a)", "b)", "c)", "d)"),
           hjust = -1,
           nrow = 2
           )

fig4 <- plot_grid(fig4, legend_scenarios, 
                  ncol = 1, rel_heights = c(1, 0.2))


# figure4 <- ggdraw() +
#   draw_plot(xg, x = 0, y = .5, width = 0.5, height = 0.5) +
#   draw_plot(xc, x = .5, y = .5, width = 0.5, height = 0.5) +
#   draw_plot(xl, x = 0, y = 0, width = 0.5, height = 0.5) +
#   draw_plot(xr, x = .5, y = 0, width = 0.5, height = 0.5) +
#   draw_plot_label(c("a)", "b)", "c)", "d)"), 
#                   c(0, 0.5, 0, 0.5), c(1,1, 0.525, 0.525), size = 15)
save_plot("../plots/figure4.png", fig4, base_aspect_ratio = 1.3)
