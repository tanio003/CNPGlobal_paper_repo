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
    ylim(62.5,237.5) +
    theme_bw() + 
    theme(panel.border = element_rect(color = "black"), panel.grid.major = element_blank(), panel.grid.minor = element_blank())
} + coord_flip() + theme(axis.text.y = element_text(angle = 45))
}

# Function to make np_box plot
np_boxplot <- function(POM_all) {
  np_box <- {ggplot(data = POM_all, mapping = aes(y = exp(logNP), x = region)) + 
  geom_boxplot(fill = gg_color_hue(3)[2]) + 
  geom_hline(yintercept = 16, linetype = "dashed", size = 0.5, color = gg_color_hue(3)[2]) + 
  scale_x_discrete(limits=rev) + 
    labs(y = 'N:P', x = '') + 
    stat_summary(fun = "mean", geom="point",color = "black", width = 0.2) +
    ylim(10,34) +
    theme_bw() + 
    theme(panel.border = element_rect(color = "black"), panel.grid.major = element_blank(), panel.grid.minor = element_blank())
} + coord_flip()
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
}

# Custom color function
scale_fill_fermenter_custom <- function(pal, na.value = "grey50", guide = "coloursteps", aesthetics = "fill", ...) {
  binned_scale("fill", "fermenter", ggplot2:::binned_pal(scales::manual_pal(unname(pal))), na.value = na.value, guide = guide, ...)
}

###############
# MAIN PAPER FIGURES
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


# Function to make Figure 2 in pdf (cosmetic treatments are done with Adobe Illustrator)
make_fig_2 <- function(dest,
                       fig_out_folder,
                       M.POM_highlat_corr_selected,
                       M.POM_lowlat_corr_selected,
                       testRes_selected_highlat,
                       testRes_selected_lowlat,
                       CNP_highlat_gam_devexpl,
                       CNP_lowlat_gam_devexpl) {
  pdf(dest, width = 5.5, height = 4)
  par(mfrow=c(2,2))
  p1 <- fig_2a(M.POM_highlat_corr_selected,testRes_selected_highlat, title_name = "Polar & Subpolar")
  p2 <- fig_2b(CNP_highlat_gam_devexpl)
  p3 <- fig_2a(M.POM_lowlat_corr_selected,testRes_selected_lowlat, title_name = "Tropical & Subtropical")
  p4 <- fig_2b(CNP_lowlat_gam_devexpl)
  dev.off()
}

fig_2a <- function(M.POM_highlat_corr_selected,testRes_selected_highlat, title_name) {
  fig <- corrplot(M.POM_highlat_corr_selected, p.mat = testRes_selected_highlat,
          col = rev(brewer.rdbu(40)),
          method = "circle", 
          cl.cex = 0.5,
          sig.level = c(0.001, 0.01, 0.05), insig = 'label_sig',pch.cex = 0.7,tl.col="black", tl.srt=45,
          title = title_name, mar=c(0,0,1,0),na.label = "NA")
}

fig_2b <- function(CNP_highlat_gam_devexpl) {
  fig <- barplot(as.matrix(CNP_highlat_gam_devexpl[1:4,]), width = c(0.5,0.5,0.5,0.5),
        col = brewer.pal(nrow(CNP_highlat_gam_devexpl[1:4,]), "Set1"),
        font.axis=2,
        cex.names=0.75,
        las = 1,
        legend = c('SST', 'Nitrate', 'Nutricline', 'Nutricline x Nutlim'),
        xlim = c(0, ncol(CNP_highlat_gam_devexpl) + 0.2),
        ylim = c(0, 0.7),
        ylab = 'Explained Deviance',
        args.legend = list(
          x = ncol(CNP_highlat_gam_devexpl)*0.9,
          y = max(colSums(CNP_highlat_gam_devexpl[1:4,])) + 0.8,
          bty = "n", cex=0.75
        )
        )
}

make_fig_3a <- function(data_all, mod_CP) {
  mod_CP_SST_pred <- make_mod_CNP_SST_pred(data_all, mod_CP)
  fig3a <- {ggplot(data = data_all, aes(SST, exp(logCP))) + 
      annotate("text", x=33*0.95, y=255*0.95, label= "(A)") +
      geom_jitter(aes(color = Nutlim), size = 0.45) +   
      geom_ribbon(aes(ymin=exp(fit -2*se.fit), ymax=exp(fit +2*se.fit), x=SST),
                  data=mod_CP_SST_pred, 
                  alpha=0.4, 
                  inherit.aes=FALSE) +
      geom_line(aes(y=exp(fit)), data=mod_CP_SST_pred, cex = 0.75)+ 
      coord_cartesian(xlim = c(0,33), ylim = c(60, 255)) + 
      xlab("") +
      ylab("C:P") +
      xlim(0,33) + 
      ylim(60,255) + 
      scale_x_continuous(breaks = scales::pretty_breaks(n = 5)) + 
      scale_y_continuous(breaks = c(75, 100, 125, 150, 175, 200, 225),
                         labels = c(75, 100, 125, 150, 175, 200, 225)) + 
      theme_bw(base_size = 8, base_family = "Helvetica") + 
      theme(panel.border = element_rect(color = "black"), panel.grid.major = element_blank(),
            panel.grid.minor = element_blank())
    }  + theme(axis.title.x=element_blank(), axis.text.x=element_blank()) + theme(legend.position ="none") + scale_color_manual(values=rev(gg_color_hue(4)))
  fig3a <- ggplotly(fig3a)
}

make_fig_3d <- function(data_all,mod_NP) {
    mod_NP_SST_pred <- make_mod_CNP_SST_pred(data_all, mod_NP)
    fig3d <- {ggplot(data = data_all, aes(SST, exp(logNP))) + 
        annotate("text", x=33*0.95, y=36*0.95, label= "(D)") +
        geom_jitter(aes(color = Nutlim), size = 0.45) +   
        geom_ribbon(aes(ymin=exp(fit -2*se.fit), ymax=exp(fit +2*se.fit), x=SST),
              data=mod_NP_SST_pred, 
              alpha=0.4, 
              inherit.aes=FALSE) +
        geom_line(aes(y=exp(fit)), data=mod_NP_SST_pred, cex = 0.75)+ 
        coord_cartesian(xlim = c(0,33), ylim = c(10, 36)) + 
        xlab("") +
        ylab("N:P") +
        xlim(0,33) + 
        ylim(10,36) + 
        scale_x_continuous(breaks = scales::pretty_breaks(n = 5)) + 
        scale_y_continuous(breaks = c(12, 16, 20, 24, 28,32),
                   labels = c(12, 16, 20, 24, 28,32)) +       
        theme_bw(base_size = 8, base_family = "Helvetica") + 
        theme(panel.border = element_rect(color = "black"), panel.grid.major = element_blank(), panel.grid.minor = element_blank())
      }  + theme(axis.title.x=element_blank(), axis.text.x=element_blank()) + theme(legend.position = "none") + scale_color_manual(values=rev(gg_color_hue(4)))
    fig3d <- ggplotly(fig3d)  
}  

make_fig_3g <- function(data_all,mod_CN) {
    mod_CN_SST_pred <- make_mod_CNP_SST_pred(data_all, mod_CN)  
    fig3g <- {ggplot(data = data_all, aes(SST, exp(logCN))) + 
        annotate("text", x=33*0.95, y=9*0.95, label= "(G)") +
        geom_jitter(aes(color = Nutlim), size = 0.45) +   
        geom_ribbon(aes(ymin=exp(fit -2*se.fit), ymax=exp(fit +2*se.fit), x=SST),
              data=mod_CN_SST_pred, 
              alpha=0.4, 
              inherit.aes=FALSE) +
        geom_line(aes(y=exp(fit)), data=mod_CN_SST_pred, cex = 0.75)+ 
        coord_cartesian(xlim = c(0,33),ylim = c(5, 9.0)) + 
        xlab("SST (°C)") +
        ylab("C:N") +
        xlim(0,33) + 
        ylim(5,9.0) + 
        scale_x_continuous(breaks = scales::pretty_breaks(n = 5)) +        
        theme_bw(base_size = 8) + 
        theme(panel.border = element_rect(color = "black"), panel.grid.major = element_blank(), panel.grid.minor = element_blank())
      }  + theme(legend.position = "none") + scale_color_manual(values=rev(gg_color_hue(4)))
    fig3g <- ggplotly(fig3g)
}  

make_fig_3b <- function(data_all,mod_CP) {
  mod_CP_Nitrate_pred <- make_mod_CNP_Nitrate_pred(data_all, mod_CP)
  fig3b <- {ggplot(data = data_all, aes(log10(exp(logNO3_fill)), exp(logCP))) + 
      annotate("text", x=1.5*0.95, y=255*0.95, label= "(B)") +
      geom_jitter(aes(color = Nutlim), size = 0.45) +   
      geom_ribbon(aes(ymin=exp(fit -2*se.fit), ymax=exp(fit +2*se.fit), x=log10(exp(logNO3_fill))),
              data=mod_CP_Nitrate_pred, 
              alpha=0.4, 
              inherit.aes=FALSE) +
      geom_line(aes(y=exp(fit)), data=mod_CP_Nitrate_pred, cex = 0.75)+ 
      coord_cartesian(xlim = c(-1.0,1.5), ylim = c(60, 255)) + 
      xlab("") +
      ylab("") +
      xlim(-1.0,1.5) + 
      ylim(60,255) + 
      scale_x_continuous(breaks = scales::pretty_breaks(n = 5)) + 
      scale_y_continuous(breaks = c(75, 100, 125, 150, 175, 200, 225),
                   labels = c(75, 100, 125, 150, 175, 200, 225)) +     
      theme_bw(base_size = 8, base_family = "Helvetica") + 
      theme(panel.border = element_rect(color = "black"), panel.grid.major = element_blank(), panel.grid.minor = element_blank())
    }  + theme(axis.title.x=element_blank(), axis.text.x=element_blank(),axis.text.y=element_blank()) + theme(legend.position = "none") + scale_color_manual(values=rev(gg_color_hue(4)))
  fig3b <- ggplotly(fig3b)
} 

make_fig_3e <- function(data_all,mod_NP) {
    mod_NP_Nitrate_pred <- make_mod_CNP_Nitrate_pred(data_all, mod_NP)
    fig3e <- {ggplot(data = data_all, aes(log10(exp(logNO3_fill)), exp(logNP))) + 
        annotate("text", x=1.5*0.95, y=36*0.95, label= "(E)") +
        geom_jitter(aes(color = Nutlim), size = 0.45) +   
        geom_ribbon(aes(ymin=exp(fit -2*se.fit), ymax=exp(fit +2*se.fit), x=log10(exp(logNO3_fill))),
              data=mod_NP_Nitrate_pred, 
              alpha=0.4, 
              inherit.aes=FALSE) +
        geom_line(aes(y=exp(fit)), data=mod_NP_Nitrate_pred, cex = 0.75)+ 
        coord_cartesian(xlim = c(-1.0,1.5), ylim = c(10, 36)) + 
        xlab("") +
        ylab("") +
        xlim(-1.0,1.5) + 
        ylim(10,36) + 
        scale_x_continuous(breaks = scales::pretty_breaks(n = 5)) + 
        scale_y_continuous(breaks = c(12, 16, 20, 24, 28, 32),
                   labels = c(12, 16, 20, 24, 28, 32)) +       
        theme_bw(base_size = 8, base_family = "Helvetica") + 
        theme(panel.border = element_rect(color = "black"), panel.grid.major = element_blank(), panel.grid.minor = element_blank())
      }  + theme(axis.title.x=element_blank(), axis.text.x=element_blank(),axis.text.y=element_blank()) + theme(legend.position = "none") + scale_color_manual(values=rev(gg_color_hue(4)))
    fig3e <- ggplotly(fig3e)
}  

make_fig_3h <- function(data_all,mod_CN) {
  mod_CN_Nitrate_pred <- make_mod_CNP_Nitrate_pred(data_all, mod_CN)
  # Tease it so that the shading does not go below C:N of 5 for display
  cn_shade_min <- exp(mod_CN_Nitrate_pred$fit - 2*mod_CN_Nitrate_pred$se.fit)
  cn_shade_min <- pmax(cn_shade_min,rep(5,length(cn_shade_min)))
  fig3h <- {ggplot(data = data_all, aes(log10(exp(logNO3_fill)), exp(logCN))) + 
      annotate("text", x=1.5*0.95, y=9*0.95, label= "(H)") +
      geom_jitter(aes(color = Nutlim), size = 0.45) +   
      geom_ribbon(aes(ymin=cn_shade_min, ymax=exp(fit +2*se.fit), x=log10(exp(logNO3_fill))),
              data=mod_CN_Nitrate_pred, 
              alpha=0.4, 
              inherit.aes=FALSE) +
      geom_line(aes(y=exp(fit)), data=mod_CN_Nitrate_pred, cex = 0.75)+ 
      coord_cartesian(xlim = c(-1.0, 1.5),ylim = c(5, 9.0)) + 
      xlab("log10[NO3]") +
      ylab("") +
      xlim(-1.0,1.5) + 
      ylim(5,9.0) + 
      scale_x_continuous(breaks = scales::pretty_breaks(n = 5)) + 
      theme_bw(base_size = 8, base_family = "Helvetica") + 
      theme(panel.border = element_rect(color = "black"), panel.grid.major = element_blank(), panel.grid.minor = element_blank())
    }  + theme(axis.text.y=element_blank()) + theme(legend.position = "none") + scale_color_manual(values=rev(gg_color_hue(4)))
  fig3h <- ggplotly(fig3h)
}

make_fig_3c <- function(data_all,mod_CP_Nutcline_Nutlim_modGS) {
  mod_CP_Nutcline_Nutlim_pred <- make_mod_CNP_Nutcline_Nutlim_pred(data_all,
                                                                 mod_CP_Nutcline_Nutlim_modGS)
  fig3c <- {ggplot(data = data_all, aes(Nutcline_GLODAP_1um, exp(logCP))) + 
      annotate("text", x=210*0.95, y=255*0.95, label= "(C)") +
      geom_jitter(aes(color = Nutlim), size = 0.45, alpha = 1.0) +   
      geom_line(aes(x = Nutcline_GLODAP_1um, y=exp(fit), group = Nutlim, color = Nutlim), data=mod_CP_Nutcline_Nutlim_pred, cex = 0.75) + 
      coord_cartesian(xlim = c(0,210), ylim = c(60, 255)) + 
      xlab("") +
      ylab("") +
      xlim(0,210) + 
      ylim(60,255) +
      scale_x_continuous(breaks = scales::pretty_breaks(n = 5)) + 
      scale_y_continuous(breaks = c(75, 100, 125, 150, 175, 200, 225),
                   labels = c(75, 100, 125, 150, 175, 200, 225)) + 
      theme_bw(base_size = 8, base_family = "Helvetica") + 
      theme(panel.border = element_rect(color = "black"), panel.grid.major = element_blank(), panel.grid.minor = element_blank())
    }  + theme(axis.title.x=element_blank(), axis.text.x=element_blank(),axis.title.y=element_blank(), axis.text.y=element_blank()) + theme(legend.position = "none") + scale_color_manual(values=rev(gg_color_hue(4)))
  fig3c <- ggplotly(fig3c)
}  

make_fig_3f <- function(data_all, mod_NP_Nutcline_Nutlim_modGS) {
  mod_NP_Nutcline_Nutlim_pred <- make_mod_CNP_Nutcline_Nutlim_pred(data_all,
                                                                 mod_NP_Nutcline_Nutlim_modGS)
  fig3f <- {ggplot(data = data_all, aes(Nutcline_GLODAP_1um, exp(logNP))) +
      annotate("text", x=210*0.95, y=36*0.95, label= "(F)") +
      geom_jitter(aes(color = Nutlim), size = 0.45, alpha = 1.0) +   
      geom_line(aes(x = Nutcline_GLODAP_1um, y=exp(fit), group = Nutlim, color = Nutlim), data=mod_NP_Nutcline_Nutlim_pred, cex = 0.75) + 
      coord_cartesian(xlim = c(0,210), ylim = c(10, 36)) + 
      xlab("") +
      ylab("") +
      xlim(0,210) + 
      ylim(10,36) + 
      scale_x_continuous(breaks = scales::pretty_breaks(n = 5)) + 
      scale_y_continuous(breaks = c(12, 16, 20, 24, 28,32),
                   labels = c(12, 16, 20, 24, 28,32)) +      
      theme_bw(base_size = 8, base_family = "Helvetica") + 
      theme(panel.border = element_rect(color = "black"), panel.grid.major = element_blank(), panel.grid.minor = element_blank())
    }  + theme(axis.title.x=element_blank(), axis.text.x=element_blank(),axis.title.y=element_blank(), axis.text.y=element_blank()) + theme(legend.position = "none") + scale_color_manual(values=rev(gg_color_hue(4)))
  fig3f <- ggplotly(fig3f)
}

make_fig_3i <- function(data_all, mod_CN_Nutcline_Nutlim_modGS) {
  mod_CN_Nutcline_Nutlim_pred <- make_mod_CNP_Nutcline_Nutlim_pred(data_all,
                                                                 mod_CN_Nutcline_Nutlim_modGS)
  fig3i <- {ggplot(data = data_all, aes(Nutcline_GLODAP_1um, exp(logCN))) +
      annotate("text", x=210*0.95, y=9*0.95, label= "(I)") +
      geom_jitter(aes(color = Nutlim), size = 0.45, alpha = 1.0) +
      geom_line(aes(x = Nutcline_GLODAP_1um, y=exp(fit), group = Nutlim, color = Nutlim), data=mod_CN_Nutcline_Nutlim_pred, cex = 0.75) +
      coord_cartesian(xlim = c(0,210), ylim = c(5, 9.0)) +
      xlab("Nutricline (m)") +
      ylab("") +
      xlim(0,210) +
      ylim(5,9.0) +
      scale_x_continuous(breaks = scales::pretty_breaks(n = 5)) +
      theme_bw(base_size = 8, base_family = "Helvetica") +
      theme(panel.border = element_rect(color = "black"), panel.grid.major = element_blank(), panel.grid.minor = element_blank())
    }  + theme(axis.text.y=element_blank()) + theme(legend.position = "none") + scale_color_manual(values=rev(gg_color_hue(4)))
  fig3i <- ggplotly(fig3i)
}

# Legends are added manually
make_fig_3 <- function(dest, fig_out_folder, 
                       mod_CP,
                       mod_NP,
                       mod_CN,
                       mod_CP_Nutcline_Nutlim_modGS,
                       mod_NP_Nutcline_Nutlim_modGS,
                       mod_CN_Nutcline_Nutlim_modGS,
                       data_all) {
  fig3a <- make_fig_3a(data_all, mod_CP)
  fig3b <- make_fig_3b(data_all, mod_CP)
  fig3c <- make_fig_3c(data_all, mod_CP_Nutcline_Nutlim_modGS)
  fig3d <- make_fig_3d(data_all, mod_NP)
  fig3e <- make_fig_3e(data_all, mod_NP)
  fig3f <- make_fig_3f(data_all, mod_NP_Nutcline_Nutlim_modGS)
  fig3g <- make_fig_3g(data_all, mod_CN)
  fig3h <- make_fig_3h(data_all, mod_CN)
  fig3i <- make_fig_3i(data_all, mod_CN_Nutcline_Nutlim_modGS)
  fig3 <-plotly::subplot(fig3a, 
                       fig3b,
                       fig3c,
                       fig3d, 
                       fig3e,
                       fig3f,
                       fig3g, 
                       fig3h,
                       fig3i,
                       nrows = 3,
                       shareX = FALSE, shareY = FALSE, titleX = TRUE, titleY = TRUE, margin = 0.015,
                       heights = c(0.33, 0.34, 0.33)) %>% layout(height = 300, width = 450)
  fig3
  orca(fig3, dest)
  server <- orca_serve()
  server$close()
}

# Function to plot Figure 4 (for N:P, turn NP = TRUE)
make_fig_4 <- function(dest,
                       fig_out_folder,
                       cesm_lonlat_info,
                       CNP_gam_cesm,
                       diffcp_full_PosCount_grid,
		       plot_title_left,
		       plot_title_right,
		       colbarbreak,
		       colbartitle,
		       colbarlimits,
		       NP = FALSE) {  

  lonlat_grid <- cesm_lonlat_info$lonlat_grid
  lonlat_grid_lon <- cesm_lonlat_info$lonlat_grid_lon
  lonlat_grid_lat <- cesm_lonlat_info$lonlat_grid_lat
  
  # Left = Delta C:P of model GS (full)
  pred_cp_SSP370_full <- CNP_gam_cesm$pred_cp_SSP370_full
  pred_cp_historic_full <- CNP_gam_cesm$pred_cp_historic_full
  if (NP) {
	  pred_cp_SSP370_full <- CNP_gam_cesm$pred_np_SSP370_full
	  pred_cp_historic_full <- CNP_gam_cesm$pred_np_historic_full 
  }

  delcp_full <- pred_cp_SSP370_full - pred_cp_historic_full
  delcp_full_plot <- reshape2::melt(delcp_full)
  delcp_full_plot$lon <- as.vector(lonlat_grid_lon)
  delcp_full_plot$lat <- as.vector(lonlat_grid_lat)
  
  fig_left <- ggplot(data = delcp_full_plot, aes(x = lon, y = lat)) +
    geom_raster(aes(fill = value)) +
    coord_cartesian(xlim = c(-179.5, 179.5), ylim = c(-89.5, 89.5), expand = F) + 
  xlab("Longitude") +
  ylab("Latitude") + 
  scale_fill_cmocean(name = "balance", na.value = "black",breaks = colbarbreak,limits=colbarlimits,discrete = FALSE) + 
  ggtitle(plot_title_left) + 
  theme_bw(base_size = 10, base_family = "Helvetica") + 
  theme(panel.border = element_rect(fill = NA, colour = "black", size = 1),
          axis.text = element_text(colour = "black"),
          axis.ticks = element_line(colour = "black"), legend.position = "bottom") +
  guides(fill = guide_colorbar(barwidth = 14, barheight = 0.75,title = colbartitle, title.position = "bottom"))
  
  # Right = Model Confidence
  modelconf_cp_full <- diffcp_full_PosCount_grid/2000*100
  modelconf_cp_full_plot <- reshape2::melt(modelconf_cp_full)
  modelconf_cp_full_plot$lon <- as.vector(lonlat_grid_lon)
  modelconf_cp_full_plot$lat <- as.vector(lonlat_grid_lat)
  
  fig_right <- ggplot(data = modelconf_cp_full_plot, aes(x = lon, y = lat)) +
    geom_raster(aes(fill = value)) +
    coord_cartesian(xlim = c(-179.5, 179.5), ylim = c(-89.5, 89.5), expand = F) + 
  labs(y = NULL) +
  xlab("Longitude") + 
  scale_fill_fermenter(direction = -1, 
                       palette = "PuOr", 
                       na.value = "black", 
                       breaks = seq(0,100,100/7),
                       limits=c(0, 100),
                       labels = c("100%-",
                                  "86%-",
                                  "71%-",
                                  "57%-",
                                  "57%+",
                                  "71%+",
                                  "86%+",
                                  "100%+")) +
  ggtitle(plot_title_right) + 
  theme_bw(base_size = 10, base_family = "Helvetica") + 
  theme(panel.border = element_rect(fill = NA, colour = "black", size = 1),
          axis.text = element_text(colour = "black"),
          axis.ticks = element_line(colour = "black"), legend.position = "bottom") +
  guides(fill = guide_colorbar(barwidth = 14, barheight = 0.75, title = "Model agreement", title.position = "bottom"))
  
  # Convert the figures to grobs
  fig_left_grob <- ggplotGrob(fig_left)
  fig_right_grob <- ggplotGrob(fig_right)
  gg <- ggplot() +
  coord_equal(xlim = c(1, 10), ylim = c(1, 6), expand = F) +
    annotation_custom(fig_left_grob,
                      xmin = 1, xmax = 5.5, ymin = 1, ymax = 6) +  
      annotation_custom(fig_right_grob,
                      xmin = 5.5, xmax = 10, ymin = 1, ymax = 6) + 
    theme(panel.border = element_blank())
  # gg
  ggsave(dest,
        width = 8, # The width of the plot in inches
        height = 5)

}

###############
# EXTENDED DATA FIGURES
###############

# Function to Make ED_Fig. 2
make_ed_fig_2 <- function(dest,
                          fig_out_folder,
                          POM_genomes_selected_binned_w_highlat, 
                          nutlim_historic,
                          nutlim_SSP370,
                          cesm_lonlat_info) {
  
  lon <- cesm_lonlat_info$lon
  lat <- cesm_lonlat_info$lat
  lonlat_grid_lon <- cesm_lonlat_info$lonlat_grid_lon
  lonlat_grid_lat <- cesm_lonlat_info$lonlat_grid_lat
  
  # Left = Nutrient Limitation in 2010-2014 with observation overlaid
  nutlim_historic_plot <- reshape2::melt(nutlim_historic)
  nutlim_historic_plot$lon <- as.vector(lonlat_grid_lon)
  nutlim_historic_plot$lat <- as.vector(lonlat_grid_lat)
  pal <- alpha(gg_color_hue(4),0.8)
  

  
  fig_left <- ggplot(data = nutlim_historic_plot, aes(x = lon, y = lat)) +
    geom_raster(aes(fill = value)) +
    coord_cartesian(xlim = c(-179.5, 179.5), ylim = c(-89.5, 89.5), expand = F) + 
  labs(y = NULL) +
  xlab("Longitude") + 
  ylab("Latitude") + 
  scale_fill_fermenter_custom(pal,
                              na.value = "black",
                              limits = c(0, 4),
                              breaks = seq(1, 4 ,1.0),
                              labels = c("P","P/N","N","Fe"), guide = "none") +
  geom_point(data = POM_genomes_selected_binned_w_highlat, aes(x = Longitude, y = Latitude, color = Nutlim), size = 0.6,show.legend = T) + 
  ggtitle("(A) SP Nutrient limitation (2010s)") + 
  theme_bw(base_size = 10, base_family = "Helvetica") +
  theme(legend.position = "bottom", legend.box = "horizontal",legend.background = element_rect(colour = 'black', fill = 'white', linetype='solid'),legend.key.size = unit(0.3, "cm")) + 
  theme(panel.border = element_rect(color = "black"), panel.grid.major = element_blank(), panel.grid.minor = element_blank())
  fig_left
  
  # Right = Nutrient Limitation in SSP370
  nutlim_SSP370_plot <- reshape2::melt(nutlim_SSP370)
  nutlim_SSP370_plot$lon <- as.vector(lonlat_grid_lon)
  nutlim_SSP370_plot$lat <- as.vector(lonlat_grid_lat)
  pal <- alpha(gg_color_hue(4),0.8)
  
  fig_right <- ggplot(data = nutlim_SSP370_plot, aes(x = lon, y = lat)) +
    geom_raster(aes(fill = value)) +
    coord_cartesian(xlim = c(-179.5, 179.5), ylim = c(-89.5, 89.5), expand = F) + 
    labs(y = NULL) +
    xlab("Longitude") + 
    ylab("Latitude") + 
    scale_fill_fermenter_custom(pal,
                              na.value = "black",
                              limits = c(0, 4),
                              breaks = seq(1, 4 ,1.0),
                              labels = c("P","P/N","N","Fe"), guide = "none") +
    geom_point(data = POM_genomes_selected_binned_w_highlat, aes(x = Longitude, y = Latitude, color = Nutlim), size = 0.6,show.legend = T, alpha = 0.0) + 
    ggtitle("(B) SP Nutrient limitation (2090s)") + 
    theme_bw(base_size = 10, base_family = "Helvetica") +
    theme(legend.position = "bottom", legend.box = "horizontal",legend.background = element_rect(colour = 'black', fill = 'white', linetype='solid'),legend.key.size = unit(0.3, "cm")) + 
    theme(panel.border = element_rect(color = "black"), panel.grid.major = element_blank(), panel.grid.minor = element_blank())
  fig_right
  
 # Convert the figures to grobs
  fig_left_grob <- ggplotGrob(fig_left)
  fig_right_grob <- ggplotGrob(fig_right)
  gg <- ggplot() +
  coord_equal(xlim = c(1, 10), ylim = c(1, 6), expand = F) +
    annotation_custom(fig_left_grob,
                      xmin = 1, xmax = 5.5, ymin = 1, ymax = 6) +  
      annotation_custom(fig_right_grob,
                      xmin = 5.5, xmax = 10, ymin = 1, ymax = 6) + 
    theme(panel.border = element_blank())
  gg
  ggsave(dest,
        width = 8, # The width of the plot in inches
        height = 5) 
}

# Function to make ED_Fig. 6
make_ed_6 <- function(dest,
                      fig_out_folder,
                      sst_surf_historic,
                      sst_surf_SSP370,
                      nitrate_surf_historic,
                      nitrate_surf_SSP370,
                      nutcline_historic,
                      nutcline_SSP370,
		      cesm_lonlat_info) {

  lon <- cesm_lonlat_info$lon
  lat <- cesm_lonlat_info$lat
  lonlat_grid_lon <- cesm_lonlat_info$lonlat_grid_lon
  lonlat_grid_lat <- cesm_lonlat_info$lonlat_grid_lat
  
  # Top Left (3,3,1) = SST in 2010s
  sst_historic_plot <- reshape2::melt(sst_surf_historic)
  sst_historic_plot$lon <- as.vector(lonlat_grid_lon)
  sst_historic_plot$lat <- as.vector(lonlat_grid_lat)
  
  fig_331 <- ggplot(data = sst_historic_plot, aes(x = lon, y = lat)) +
    geom_raster(aes(fill = value)) +
    coord_cartesian(xlim = c(-179.5, 179.5), ylim = c(-89.5, 89.5), expand = F) +
  labs(y = NULL) +
  xlab("") +
  ylab("") +
  scale_fill_fermenter(palette = "Spectral",
                              na.value = "gray30",
                              limits = c(-5, 37),
                              breaks = seq(-5, 35 , 5)) +
  ggtitle("(A) SST (2010s)") +
  theme_bw(base_size = 18, base_family = "Helvetica") +
  theme(legend.position = "right",legend.key.size = unit(0.2, "cm")) +
  theme(panel.border =  element_rect(color = "gray30"), panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  guides(fill = guide_colorsteps(barwidth = 0.75, barheight = 14, title = "°C", title.position = "top"))

  fig_331
  
  # Top Middle (3,3,2) = SST in 2090s
  sst_SSP370_plot <- reshape2::melt(sst_surf_SSP370)
  sst_SSP370_plot$lon <- as.vector(lonlat_grid_lon)
  sst_SSP370_plot$lat <- as.vector(lonlat_grid_lat)
  
  fig_332 <- ggplot(data = sst_SSP370_plot, aes(x = lon, y = lat)) +
    geom_raster(aes(fill = value)) +
    coord_cartesian(xlim = c(-179.5, 179.5), ylim = c(-89.5, 89.5), expand = F) +
  labs(y = NULL) +
  xlab("") +
  ylab("") +
  scale_fill_fermenter(palette = "Spectral",
                              na.value = "gray30",
                              limits = c(-5, 37),
                              breaks = seq(-5, 35 , 5)) +
  ggtitle("(B) SST (2090s)") +
  theme_bw(base_size = 18, base_family = "Helvetica") +
  theme(legend.position = "right",legend.key.size = unit(0.2, "cm")) +
  theme(panel.border =  element_rect(color = "black"), panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  guides(fill = guide_colorsteps(barwidth = 0.75, barheight = 14, title = "°C", title.position = "top"))

  fig_332
  
  # Top Right (3,3,3) = DSST (2090s-2010s)
  sst_diffSSP370hist_plot <- sst_SSP370_plot - sst_historic_plot
  sst_diffSSP370hist_plot$lon <- as.vector(lonlat_grid_lon)
  sst_diffSSP370hist_plot$lat <- as.vector(lonlat_grid_lat)
  
  fig_333 <- ggplot(data = sst_diffSSP370hist_plot, aes(x = lon, y = lat)) +
    geom_raster(aes(fill = value)) +
    coord_cartesian(xlim = c(-179.5, 179.5), ylim = c(-89.5, 89.5), expand = F) +
  labs(y = NULL) +
  xlab("") +
  ylab("") +
  scale_fill_fermenter(palette = "RdBu",
                              na.value = "gray30",
                              limits = c(-5, 5),
                              breaks = seq(-4, 4 , 1)) +
  ggtitle("(C) DSST (2090s-2010s)") +
  theme_bw(base_size = 18, base_family = "Helvetica") +
  theme(legend.position = "right",legend.key.size = unit(0.2, "cm")) +
  theme(panel.border =  element_rect(color = "black"), panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  guides(fill = guide_colorsteps(barwidth = 0.75, barheight = 14, title = "°C", title.position = "top"))

  fig_333
  
  # Middle Left (3,3,4) = NO3 in 2010s
  nitrate_historic_plot <- reshape2::melt(nitrate_surf_historic)
  nitrate_historic_plot$lon <- as.vector(lonlat_grid_lon)
  nitrate_historic_plot$lat <- as.vector(lonlat_grid_lat)
  pal = cmocean("delta")(15)
  fig_334 <- ggplot(data = nitrate_historic_plot, aes(x = lon, y = lat)) +
    geom_raster(aes(fill = value)) +
    coord_cartesian(xlim = c(-179.5, 179.5), ylim = c(-89.5, 89.5), expand = F) +
  labs(y = NULL) +
  xlab("") +
  ylab("") +
  scale_fill_gradientn(colors = pal,
                              na.value = "gray30",
                      limits = c(0, 28),
                      breaks = c(0,1,2,4,6,8,10,12,14,16,18,20,22,24)) +
  ggtitle("(D) NO3 (2010s)") +
  theme_bw(base_size = 18, base_family = "Helvetica") +
  theme(legend.position = "right",legend.key.size = unit(0.2, "cm")) +
  theme(panel.border =  element_rect(color = "black"), panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  guides(fill = guide_colorsteps(barwidth = 0.75, barheight = 14, title = "uM", title.position = "top",show.limits = T))

  fig_334
  
  # Middle Middle (3,3,5) = NO3 in 2090s
  nitrate_SSP370_plot <- reshape2::melt(nitrate_surf_SSP370)
  nitrate_SSP370_plot$lon <- as.vector(lonlat_grid_lon)
  nitrate_SSP370_plot$lat <- as.vector(lonlat_grid_lat)
  pal = cmocean("delta")(15)
  
  fig_335 <- ggplot(data = nitrate_SSP370_plot, aes(x = lon, y = lat)) +
    geom_raster(aes(fill = value)) +
    coord_cartesian(xlim = c(-179.5, 179.5), ylim = c(-89.5, 89.5), expand = F) +
  labs(y = NULL) +
  xlab("") +
  ylab("") +
  scale_fill_gradientn(colors = pal,
                              na.value = "gray30",
                      limits = c(0, 28),
                      breaks = c(0,1,2,4,6,8,10,12,14,16,18,20,22,24)) +
  ggtitle("(E) NO3 (2090s)") +
  theme_bw(base_size = 18, base_family = "Helvetica") +
  theme(legend.position = "right",legend.key.size = unit(0.2, "cm")) +
  theme(panel.border =  element_rect(color = "black"), panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  guides(fill = guide_colorsteps(barwidth = 0.75, barheight = 14, title = "uM", title.position = "top",show.limits = T))

  fig_335
  
  # Middle Right (3,3,6) = DNO3 (2090s-2010s)
  nitrate_diffSSP370hist_plot <- nitrate_SSP370_plot - nitrate_historic_plot
  nitrate_diffSSP370hist_plot$lon <- as.vector(lonlat_grid_lon)
  nitrate_diffSSP370hist_plot$lat <- as.vector(lonlat_grid_lat)
  
  fig_336 <- ggplot(data = nitrate_diffSSP370hist_plot, aes(x = lon, y = lat)) +
    geom_raster(aes(fill = value)) +
    coord_cartesian(xlim = c(-179.5, 179.5), ylim = c(-89.5, 89.5), expand = F) +
  labs(y = NULL) +
  xlab("") +
  ylab("") +
  scale_fill_fermenter(palette = "RdBu",
                              na.value = "gray30",
                              limits = c(-5, 5),
                              breaks = c(-2, -1, -0.5, -0.1, 0.1, 0.5, 1, 2)) +
  ggtitle("(F) DNO3 (2090s-2010s)") +
  theme_bw(base_size = 18, base_family = "Helvetica") +
  theme(legend.position = "right",legend.key.size = unit(0.2, "cm")) +
  theme(panel.border =  element_rect(color = "black"), panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  guides(fill = guide_colorsteps(barwidth = 0.75, barheight = 14, title = "uM", title.position = "top",show.limits = T))

  fig_336
  
  # Bottom Left (3,3,7) = Nutricline in 2010s
  nutcline_historic_plot <- reshape2::melt(nutcline_historic)
  nutcline_historic_plot$lon <- as.vector(lonlat_grid_lon)
  nutcline_historic_plot$lat <- as.vector(lonlat_grid_lat)
  
  pal = cmocean("tempo", direction = -1)(6)
  fig_337 <- ggplot(data = nutcline_historic_plot, aes(x = lon, y = lat)) +
    geom_raster(aes(fill = value)) +
    coord_cartesian(xlim = c(-179.5, 179.5), ylim = c(-89.5, 89.5), expand = F) +
  labs(y = NULL) +
  xlab("") +
  ylab("") +
  scale_fill_gradientn(colors = pal,
                              na.value = "gray30",
                              limits = c(0, 250),
                              breaks = c(0, 20, 90, 120, 150, 200)) +
  ggtitle("(G) Nutricline (2010s)") +
  theme_bw(base_size = 18, base_family = "Helvetica") +
  theme(legend.position = "right",legend.key.size = unit(0.2, "cm")) +
  theme(panel.border =  element_rect(color = "black"), panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  guides(fill = guide_colorsteps(barwidth = 0.75, barheight = 14, title = "m", title.position = "top",show.limits = TRUE))
  fig_337
  
  # Bottom Middle (3,3,8) = Nutricline in 2090s
  nutcline_SSP370_plot <- reshape2::melt(nutcline_SSP370)
  nutcline_SSP370_plot$lon <- as.vector(lonlat_grid_lon)
  nutcline_SSP370_plot$lat <- as.vector(lonlat_grid_lat)
  pal = cmocean("tempo", direction = -1)(12)
  fig_338 <- ggplot(data = nutcline_SSP370_plot, aes(x = lon, y = lat)) +
    geom_raster(aes(fill = value)) +
    coord_cartesian(xlim = c(-179.5, 179.5), ylim = c(-89.5, 89.5), expand = F) +
  labs(y = NULL) +
  xlab("") +
  ylab("") +
  scale_fill_gradientn(colors = pal,
                              na.value = "gray30",
                              limits = c(0, 250),
                              breaks = c(0, 20, 90, 120, 150, 200)) +
  ggtitle("(H) Nutricline (2090s)") +
  theme_bw(base_size = 18, base_family = "Helvetica") +
  theme(legend.position = "right",legend.key.size = unit(0.2, "cm")) +
  theme(panel.border =  element_rect(color = "black"), panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  guides(fill = guide_colorsteps(barwidth = 0.75, barheight = 14, title = "m", title.position = "top",show.limits = TRUE))

  fig_338
  
  # Bottom Right (3,3,9) = DNutricline (2090s-2010s)
  nutcline_diffSSP370hist_plot <- nutcline_SSP370_plot - nutcline_historic_plot
  nutcline_diffSSP370hist_plot$lon <- as.vector(lonlat_grid_lon)
  nutcline_diffSSP370hist_plot$lat <- as.vector(lonlat_grid_lat)
  pal = cmocean("balance")(12)
  
  fig_339 <- ggplot(data = nutcline_diffSSP370hist_plot, aes(x = lon, y = lat)) +
    geom_raster(aes(fill = value)) +
    coord_cartesian(xlim = c(-179.5, 179.5), ylim = c(-89.5, 89.5), expand = F) +
  labs(y = NULL) +
  xlab("") +
  ylab("") +
  scale_fill_fermenter(palette = "RdBu",
                              na.value = "gray30",
                              limits = c(-150, 150),
                              breaks = c(-125,-100, -50, -20,-10,10,20,50,100,125,150)) +
  ggtitle("(I) DNutricline (2090s-2010s)") +
  theme_bw(base_size = 18, base_family = "Helvetica") +
  theme(legend.position = "right",legend.key.size = unit(0.2, "cm")) +
  theme(panel.border =  element_rect(color = "black"), panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  guides(fill = guide_colorsteps(barwidth = 0.75, barheight = 14, title = "m", title.position = "top",show.limits = TRUE))

  fig_339
  
  # Convert the figures to grobs
  fig_331_grob <- ggplotGrob(fig_331)
  fig_332_grob <- ggplotGrob(fig_332)
  fig_333_grob <- ggplotGrob(fig_333)
  fig_334_grob <- ggplotGrob(fig_334)
  fig_335_grob <- ggplotGrob(fig_335)
  fig_336_grob <- ggplotGrob(fig_336)
  fig_337_grob <- ggplotGrob(fig_337)
  fig_338_grob <- ggplotGrob(fig_338)
  fig_339_grob <- ggplotGrob(fig_339)
  gg <- ggplot() +
  coord_equal(xlim = c(1, 17.5), ylim = c(1, 16), expand = F) +
    annotation_custom(fig_331_grob,
                      xmin = 1, xmax = 6.5, ymin = 11, ymax = 16) +
    annotation_custom(fig_332_grob,
                      xmin = 6.5, xmax = 12, ymin = 11, ymax = 16) +
      annotation_custom(fig_333_grob,
                      xmin = 12, xmax = 17.5, ymin = 11, ymax = 16) +
    annotation_custom(fig_334_grob,
                      xmin = 1, xmax = 6.5, ymin = 6, ymax = 11) +
    annotation_custom(fig_335_grob,
                      xmin = 6.5, xmax = 12, ymin = 6, ymax = 11) +
      annotation_custom(fig_336_grob,
                      xmin = 12, xmax = 17.5, ymin = 6, ymax = 11) +
    annotation_custom(fig_337_grob,
                      xmin = 1, xmax = 6.5, ymin = 1, ymax = 6) +
    annotation_custom(fig_338_grob,
                      xmin = 6.5, xmax = 12, ymin = 1, ymax = 6) +
      annotation_custom(fig_339_grob,
                      xmin = 12, xmax = 17.5, ymin = 1, ymax = 6) +
    theme(panel.border = element_blank())

  ggsave(dest,
	 width = 18, # The width of the plot in inches
	 height = 15)

}

###############
# SUPPLEMENTARY INFO FIGURES
###############


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


