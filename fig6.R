############################################
# Fig 6
############################################

library(cowplot)

# get the legend from the figs
legend1_fig6 <- get_legend(ufg+ theme(legend.position="left") +
                             theme(legend.key = element_blank())
)
legend2_fig6 <- get_legend(sufg+ theme(legend.position="left")+ 
                             theme(legend.key = element_blank())
)
# arrange top panels
figure6_top <- plot_grid(ufg, ufl, ufb, 
                  align = "vh",
                  hjust = -1,
                  nrow = 1)
# arrange bottom panels
figure6_bottom <- plot_grid(sufg, sufl, sufb,
                         align = "vh",
                         hjust = -1,
                         nrow = 1)

# arrange legends with panels 
figure6 <- plot_grid(legend1_fig6, figure6_top,
                     legend2_fig6, figure6_bottom,
                  ncol = 2, rel_widths = c(0.2,1), scale = 0.98)


save_plot("../plots/figure6.png", figure6, 
          base_height = 5, base_width = 10,
          base_aspect_ratio = 1.3)
