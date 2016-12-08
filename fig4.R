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

# legend_scenarios <- get_legend(xg + theme(legend.position="bottom"))
legend_scenarios <- get_legend(xg + theme(legend.position="bottom") + guides(shape = guide_legend(nrow=3,byrow=TRUE)) + theme(legend.title=element_blank()) + 
                   theme(legend.key = element_blank()))

# add the legend underneath the row we made earlier. Give it 10% of the height
# of one plot (via rel_heights).
theme_set(theme_cowplot(font_size=4)) # reduce default font size

# fig4 <- plot_grid(xg,
#            xc ,
#            xl ,
#            xr,
#            align = 'vh',
#            labels = c("a)", "b)", "c)", "d)"),
#            hjust = -1,
#            nrow = 2
#            )

fig4 <- plot_grid(xg,
                  xc ,
                  xl ,
                  xr,
                  align = 'vh',
                  labels = c("a)", "b)", "c)", "d)"),
                  hjust = -0.1,
                  nrow = 4
)
fig4 <- plot_grid(fig4, legend_scenarios, 
                  ncol = 1, rel_heights = c(1, 0.1), scale = 0.99)


save_plot("../plots/figure4.png", fig4, base_height = 12, base_width = 5, base_aspect_ratio = 1.3)
