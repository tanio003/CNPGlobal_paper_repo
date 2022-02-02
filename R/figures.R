######################
# AUXILLIARY FUNCTIONS
######################

gg_color_hue <- function(n) {
  hues = seq(15, 375, length = n + 1)
  hcl(h = hues, l = 65, c = 100)[1:n]
}

# Function to make cp_box plot
cp_boxplot <- function(POM_all) {
  cp_box <- {ggplot(data = POM_all, mapping = aes(y = exp(logCP), x = region)) + 
  geom_boxplot(fill = gg_color_hue(3)[1]) + 
  geom_hline(yintercept = 106, linetype = "dashed", size = 0.5, color = gg_color_hue(3)[1]) + 
  scale_x_discrete(limits=rev) + 
    labs(y = 'C:P', x = '') + 
    stat_summary(fun = "mean", geom="point",color = "black", width = 0.2) +
    ylim(75,250) +
    theme_bw() + 
    theme(panel.border = element_rect(color = "black"), panel.grid.major = element_blank(), panel.grid.minor = element_blank())
} + coord_flip() + theme(axis.text.y = element_text(angle = 45))
  cp_box
}

# Function to make np_box plot
np_boxplot <- function(POM_all) {
  np_box <- {ggplot(data = POM_all, mapping = aes(y = exp(logNP), x = region)) + 
  geom_boxplot(fill = gg_color_hue(3)[2]) + 
  geom_hline(yintercept = 16, linetype = "dashed", size = 0.5, color = gg_color_hue(3)[2]) + 
  scale_x_discrete(limits=rev) + 
    labs(y = 'N:P', x = '') + 
    stat_summary(fun = "mean", geom="point",color = "black", width = 0.2) +
    ylim(10,35) +
    theme_bw() + 
    theme(panel.border = element_rect(color = "black"), panel.grid.major = element_blank(), panel.grid.minor = element_blank())
} + coord_flip()
  np_box
}

# Function to make cn_box plot
cn_boxplot <- function(POM_all) {
  cn_box <- {ggplot(data = POM_all, mapping = aes(y = exp(logCN), x = region)) +
  geom_boxplot(fill = gg_color_hue(3)[3]) +
  geom_hline(yintercept = 106/16, linetype = "dashed", size = 0.5, color = gg_color_hue(3)[3]) +
  scale_x_discrete(limits=rev) +
    labs(y = 'C:N', x = '') +
    stat_summary(fun = "mean", geom="point",color = "black", width = 0.2) +
    ylim(5,9) +
    theme_bw() +
    theme(panel.border = element_rect(color = "black"), panel.grid.major = element_blank(), panel.grid.minor = element_blank())
} + coord_flip()
  cn_box
}


###############
# PAPER FIGURES
###############

make_fig_1 <- function(dest, fig_out_folder,POM_all) {
	figa<- 
	{ggplot(tibble(POM_all$Latitude, exp(POM_all$logCP)), aes(POM_all$Latitude, exp(POM_all$logCP))) + 
	geom_hline(yintercept = 106, linetype = "dashed", size = 0.5, color = gg_color_hue(3)[1]) + 
	geom_vline(xintercept = 0, linetype = "dashed", size = 0.2) + 
	geom_jitter(color = gg_color_hue(3)[1], alpha = 0.1) + 
	geom_smooth(method = 'gam',fullrange = FALSE, color = gg_color_hue(3)[1],fill = gg_color_hue(3)[1]) + 
	labs(y = 'C:P', x = 'Latitude') + 
	scale_x_continuous(breaks = scales::pretty_breaks(n = 5)) + 
	coord_cartesian(xlim = c(-70, 55), ylim = c(75, 225)) + 
	scale_y_continuous(breaks = c(75, 100, 125, 150, 175, 200, 225),
	 		   labels = c(75, 100, 125, 150, 175, 200, 225), limits = c(62.5, 237.5)) + 
        theme_bw(base_size = 10, base_family = "Helvetica") + 
	theme(panel.border = element_rect(color = "black"), panel.grid.major = element_blank())} + coord_flip() 
        figa <- ggplotly(figa)
	
	figb<- 
	{ggplot(tibble(POM_all$Latitude, exp(POM_all$logNP)), aes(POM_all$Latitude, exp(POM_all$logNP))) + 
	geom_hline(yintercept = 16, linetype = "dashed", size = 0.5, color = gg_color_hue(3)[2]) + 
	geom_vline(xintercept = 0, linetype = "dashed", size = 0.2) + 
	geom_jitter(color = gg_color_hue(3)[2], alpha = 0.1) + 
	geom_smooth(method = 'gam',fullrange = TRUE, color = gg_color_hue(3)[2], fill = gg_color_hue(3)[2]) + 
	labs(y = 'N:P', x = '') + 
	xlim(-70,55) + 
	scale_y_continuous(breaks = c(12, 16, 20, 24, 28, 32),
                          labels = c(12, 16, 20, 24, 28, 32), limits = c(10, 34)) + 
        scale_x_continuous(breaks = scales::pretty_breaks(n = 5)) + 
	theme_bw(base_size = 10, base_family = "Helvetica") + 
	theme(panel.border = element_rect(color = "black"), panel.grid.major = element_blank())} + coord_flip() 
        figb <- ggplotly(figb)
	
	figc<- 
	{ggplot(tibble(POM_all$Latitude, exp(POM_all$logCN)), aes(POM_all$Latitude, exp(POM_all$logCN))) + 
	geom_hline(yintercept = 106/16, linetype = "dashed", size = 0.5, color = gg_color_hue(3)[3]) + 
	geom_vline(xintercept = 0, linetype = "dashed", size = 0.2) + 
	geom_jitter(color = gg_color_hue(3)[3], alpha = 0.1) + 
	geom_smooth(method = 'gam',fullrange = TRUE, color = gg_color_hue(3)[3], fill = gg_color_hue(3)[3]) + 
	labs(y = 'C:N', x = '') + 
	xlim(-70,55) + 
	ylim(5,9) + 
	scale_x_continuous(breaks = scales::pretty_breaks(n = 5)) + 
	theme_bw(base_size = 10, base_family = "Helvetica") + 
	theme(panel.border = element_rect(color = "black"), panel.grid.major = element_blank())} + coord_flip() 
        figc <- ggplotly(figc)

        figure1 <-plotly::subplot(figa, figb, figc, heights = 0.5, widths = c(0.33,0.34,0.33), shareX = FALSE, shareY = TRUE, titleX = TRUE, titleY = TRUE, margin = 0.01) 
	figure1
	orca(figure1, dest)
	server <- orca_serve()
	server$close()
}



make_sp_fig_1 <- function(dest, fig_out_folder,POM_all) {
	
	figa2 <- cp_boxplot(POM_all)
	figa2_ply <- ggplotly(figa2)
	figb2 <- np_boxplot(POM_all)
	figb2_ply <- ggplotly(figb2)
	figc2 <- cn_boxplot(POM_all)
	figc2_ply <- ggplotly(figc2)

        sp_fig_1 <-plotly::subplot(figa2_ply,figb2_ply, figc2_ply, nrows = 1, widths = c(0.33,0.34,0.33), shareX = FALSE, shareY = TRUE, titleX = TRUE, titleY = TRUE, margin = 0.01) 
        sp_fig_1
	orca(sp_fig_1, dest)
	server <- orca_serve()
	server$close()

}
