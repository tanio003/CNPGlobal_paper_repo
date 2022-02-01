######################
# AUXILLIARY FUNCTIONS
######################

gg_color_hue <- function(n) {
  hues = seq(15, 375, length = n + 1)
  hcl(h = hues, l = 65, c = 100)[1:n]
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

	# orca(figure1, 'Figures/Fig1_cnp_lat_.pdf')
	# server <- orca_serve()
	# server$close()
	# unlink('Fig1_cnp_lat.pdf')
}

