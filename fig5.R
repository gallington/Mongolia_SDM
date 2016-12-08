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
legend_fig5 <- get_legend(sr + theme(legend.position="bottom") +
                            guides(shape = guide_legend(nrow = 2, byrow = TRUE)) +
                            theme(legend.key = element_blank()) +
                            theme(legend.title=element_blank()))

# add the legend underneath the row we made earlier. Give it 10% of the height
# of one plot (via rel_heights).
theme_set(theme_cowplot(font_size=4)) # reduce default font size

fig5 <- plot_grid(sg,
                  sl ,
                  sr ,
                  align = 'vh',
                  labels = c("a)", "b)", "c)"),
                  hjust = -0.5,
                  vjust = 1,
                  nrow = 3
)
fig5 <- plot_grid(fig5, legend_fig5, 
                  ncol = 1, rel_heights = c(1, 0.1), scale = 0.95)


# figure4 <- ggdraw() +
#   draw_plot(xg, x = 0, y = .5, width = 0.5, height = 0.5) +
#   draw_plot(xc, x = .5, y = .5, width = 0.5, height = 0.5) +
#   draw_plot(xl, x = 0, y = 0, width = 0.5, height = 0.5) +
#   draw_plot(xr, x = .5, y = 0, width = 0.5, height = 0.5) +
#   draw_plot_label(c("a)", "b)", "c)", "d)"), 
#                   c(0, 0.5, 0, 0.5), c(1,1, 0.525, 0.525), size = 15)
save_plot("../plots/figure5.png", fig5, base_height = 10, base_width = 5, base_aspect_ratio = 1.3)
