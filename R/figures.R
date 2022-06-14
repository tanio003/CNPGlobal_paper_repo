######################
# AUXILLIARY FUNCTIONS
######################

gg_color_hue <- function(n) {
  hues = seq(15, 375, length = n + 1)
  hcl(h = hues, l = 65, c = 100)[1:n]
}

gg_color_hue_4_rbgp <- c(gg_color_hue(4)[1],gg_color_hue(4)[3], gg_color_hue(4)[2], gg_color_hue(4)[4])

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

# make_fig_2 <- function(dest,
#                        fig_out_folder,
#                        M.POM_highlat_corr_selected,
#                        M.POM_lowlat_corr_selected,
#                        testRes_selected_highlat,
#                        testRes_selected_lowlat,
#                        CNP_highlat_gam_devexpl,
#                        CNP_lowlat_gam_devexpl) {
#   pdf(dest, width = 5.5, height = 4)
#   par(mfrow=c(2,2))
#   p1 <- fig_2a(M.POM_highlat_corr_selected,testRes_selected_highlat, title_name = "Polar & Subpolar")
#   p2 <- fig_2b(CNP_highlat_gam_devexpl)
#   p3 <- fig_2a(M.POM_lowlat_corr_selected,testRes_selected_lowlat, title_name = "Tropical & Subtropical")
#   p4 <- fig_2b(CNP_lowlat_gam_devexpl)
#   dev.off()
# }

make_fig_2 <- function(dest,
                       fig_out_folder,
                       M.POM_highlat_corr_selected,
                       testRes_selected_highlat,
                       CNP_highlat_gam_devexpl,
                       title_name) {
  pdf(dest, width = 5.5, height = 4)
  par(mfrow=c(2,2))
  p1 <- fig_2a(M.POM_highlat_corr_selected,testRes_selected_highlat, title_name = title_name)
  p2 <- fig_2b_100p(CNP_highlat_gam_devexpl)
  p3 <- fig_2a(M.POM_highlat_corr_selected,testRes_selected_highlat, title_name = title_name)
  p4 <- fig_2b_100p(CNP_highlat_gam_devexpl)
  dev.off()
}

make_fig_2b_only <- function(dest,
                       fig_out_folder,
                       CNP_highlat_gam_devexpl) {
  pdf(dest, width = 4, height = 3)
  # par(mfrow=c(2,2))
  fig_2b_100p(CNP_highlat_gam_devexpl)
  dev.off()
}

fig_2a <- function(M.POM_highlat_corr_selected,testRes_selected_highlat, title_name) {
  fig <- corrplot(M.POM_highlat_corr_selected, p.mat = testRes_selected_highlat,
          col = rev(brewer.rdbu(40)),
          # method = "circle", 
          method = "color", 
          cl.cex = 0.5,
          tl.cex = 0.5,
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

fig_2b_100p <- function(CNP_highlat_gam_devexpl) {
  # data_percentage <- apply(CNP_highlat_gam_devexpl[1:4,], 2, function(x){x*100/sum(x,na.rm=T)})
  data_percentage <- apply(CNP_highlat_gam_devexpl[4:1,], 2, function(x){x*100/sum(x,na.rm=T)})
  fig <- barplot(data_percentage, width = c(0.5,0.5,0.5,0.5),
                 # col = brewer.pal(nrow(CNP_highlat_gam_devexpl[1:4,]), "Set1"),
                 col = rev(brewer.pal(nrow(CNP_highlat_gam_devexpl[1:4,]), "Set1")),
                 # font.axis=2,
                 font.axis= 2,
                 cex.axis = 0.75,
                 cex.names = 0.75,
                 las = 1,
                 # legend = c('SST', 'Nitrate', 'Nutricline', 'Nutricline x Nutlim'),
                 legend = c('Nutricline x Nutlim', 'Nutricline', 'Nitrate', 'SST'),
                 xlim = c(0, ncol(CNP_highlat_gam_devexpl) + 0.2),
                 ylim = c(0, 100),
                 ylab = 'Explained Deviance',
                 args.legend = list(
                   x = ncol(CNP_highlat_gam_devexpl)*1.3,
                   y = max(colSums(CNP_highlat_gam_devexpl[1:4,])) + 120,
                   bty = "n", cex=0.75
                 )
  )
}

make_fig_3a <- function(data_all, mod_CP) {
  mod_CP_SST_pred <- make_mod_CNP_SST_pred(data_all, mod_CP)
  fig3a <- {ggplot(data = data_all, aes(SST, exp(logCP))) + 
      # annotate("text", x=33*0.95, y=255*0.95, label= "(A)") +
      geom_point(aes(color = Nutlim), size = 0.75) +   
      geom_ribbon(aes(ymin=exp(fit -2*se.fit), ymax=exp(fit +2*se.fit), x=SST),
                  data=mod_CP_SST_pred, 
                  alpha=0.4, 
                  inherit.aes=FALSE) +
      geom_line(aes(y=exp(fit)), data=mod_CP_SST_pred, cex = 0.75)+ 
      coord_cartesian(xlim = c(3,31.5), ylim = c(60, 280)) + 
      xlab("") +
      ylab("C:P") +
      xlim(3,31.5) + 
      ylim(60,280) + 
      scale_x_continuous(breaks = scales::pretty_breaks(n = 5)) + 
      scale_y_continuous(breaks = c(75, 125, 175, 225, 275),
                         labels = c(75, 125, 175, 225, 275)) + 
      theme_bw(base_size = 12, base_family = "Helvetica") + 
      theme(panel.border = element_rect(color = "black"), panel.grid.major = element_blank(),
            panel.grid.minor = element_blank())
    }  + 
    # theme(axis.title.x=element_blank(), axis.text.x=element_blank()) + 
    theme(legend.position ="none") + scale_color_manual(values=rev(gg_color_hue(4)))
  fig3a <- ggplotly(fig3a)
}

make_fig_3d <- function(data_all,mod_NP) {
    mod_NP_SST_pred <- make_mod_CNP_SST_pred(data_all, mod_NP)
    fig3d <- {ggplot(data = data_all, aes(SST, exp(logNP))) + 
        # annotate("text", x=33*0.95, y=36*0.95, label= "(D)") +
        geom_point(aes(color = Nutlim), size = 0.75) +   
        geom_ribbon(aes(ymin=exp(fit -2*se.fit), ymax=exp(fit +2*se.fit), x=SST),
              data=mod_NP_SST_pred, 
              alpha=0.4, 
              inherit.aes=FALSE) +
        geom_line(aes(y=exp(fit)), data=mod_NP_SST_pred, cex = 0.75)+ 
        coord_cartesian(xlim = c(3,31.5), ylim = c(10, 37)) + 
        xlab("SST (°C)") +
        ylab("N:P") +
        xlim(3,31.5) + 
        ylim(10,37) + 
        scale_x_continuous(breaks = scales::pretty_breaks(n = 5)) + 
        scale_y_continuous(breaks = c(12, 20, 28, 36),
                   labels = c(12, 20, 28, 36)) +       
        theme_bw(base_size = 12, base_family = "Helvetica") + 
        theme(panel.border = element_rect(color = "black"), panel.grid.major = element_blank(), panel.grid.minor = element_blank())
      }  + 
      # theme(axis.title.x=element_blank(), axis.text.x=element_blank()) + 
      theme(legend.position = "none") + scale_color_manual(values=rev(gg_color_hue(4)))
    fig3d <- ggplotly(fig3d)  
}  

make_fig_3g <- function(data_all,mod_CN) {
    mod_CN_SST_pred <- make_mod_CNP_SST_pred(data_all, mod_CN)  
    fig3g <- {ggplot(data = data_all, aes(SST, exp(logCN))) + 
        # annotate("text", x=33*0.95, y=9*0.95, label= "(G)") +
        geom_point(aes(color = Nutlim), size = 0.75) +   
        geom_ribbon(aes(ymin=exp(fit -2*se.fit), ymax=exp(fit +2*se.fit), x=SST),
              data=mod_CN_SST_pred, 
              alpha=0.4, 
              inherit.aes=FALSE) +
        geom_line(aes(y=exp(fit)), data=mod_CN_SST_pred, cex = 0.75)+ 
        coord_cartesian(xlim = c(3,31.5),ylim = c(5, 9.2)) + 
        xlab("SST (°C)") +
        ylab("C:N") +
        xlim(3,31.5) + 
        ylim(5,9.2) + 
        scale_x_continuous(breaks = scales::pretty_breaks(n = 5)) +        
        theme_bw(base_size = 12, base_family = "Helvetica") + 
        theme(panel.border = element_rect(color = "black"), panel.grid.major = element_blank(), panel.grid.minor = element_blank())
      }  + theme(legend.position = "none") + scale_color_manual(values=rev(gg_color_hue(4)))
    fig3g <- ggplotly(fig3g)
}  

make_fig_3b <- function(data_all,mod_CP) {
  mod_CP_Nitrate_pred <- make_mod_CNP_Nitrate_pred(data_all, mod_CP)
  fig3b <- {ggplot(data = data_all, aes(log10(exp(logNO3)), exp(logCP))) + 
      # annotate("text", x=1.5*0.95, y=255*0.95, label= "(B)") +
      geom_point(aes(color = Nutlim), size = 0.75) +   
      geom_ribbon(aes(ymin=exp(fit -2*se.fit), ymax=exp(fit +2*se.fit), x=log10(exp(logNO3))),
              data=mod_CP_Nitrate_pred, 
              alpha=0.4, 
              inherit.aes=FALSE) +
      geom_line(aes(y=exp(fit)), data=mod_CP_Nitrate_pred, cex = 0.75)+ 
      coord_cartesian(xlim = c(-1.0,1.5), ylim = c(60, 280)) + 
      xlab("log10[NO3]") +
      ylab("C:P") +
      xlim(-1.0,1.5) + 
      ylim(60,280) + 
      scale_x_continuous(breaks = scales::pretty_breaks(n = 5)) + 
      scale_y_continuous(breaks = c(75, 125, 175, 225, 275),
                   labels = c(75, 125, 175, 225, 275)) +     
      theme_bw(base_size = 12, base_family = "Helvetica") + 
      theme(panel.border = element_rect(color = "black"), panel.grid.major = element_blank(), panel.grid.minor = element_blank())
    }  + 
    # theme(axis.title.x=element_blank(), axis.text.x=element_blank(),axis.text.y=element_blank()) + 
    theme(legend.position = "none") + scale_color_manual(values=rev(gg_color_hue(4)))
  fig3b <- ggplotly(fig3b)
} 

make_fig_3e <- function(data_all,mod_NP) {
    mod_NP_Nitrate_pred <- make_mod_CNP_Nitrate_pred(data_all, mod_NP)
    fig3e <- {ggplot(data = data_all, aes(log10(exp(logNO3)), exp(logNP))) + 
        # annotate("text", x=1.5*0.95, y=36*0.95, label= "(E)") +
        geom_point(aes(color = Nutlim), size = 0.75) +   
        geom_ribbon(aes(ymin=exp(fit -2*se.fit), ymax=exp(fit +2*se.fit), x=log10(exp(logNO3))),
              data=mod_NP_Nitrate_pred, 
              alpha=0.4, 
              inherit.aes=FALSE) +
        geom_line(aes(y=exp(fit)), data=mod_NP_Nitrate_pred, cex = 0.75)+ 
        coord_cartesian(xlim = c(-1.0,1.5), ylim = c(10, 37)) + 
        xlab("log10[NO3]") +
        ylab("N:P") +
        xlim(-1.0,1.5) + 
        ylim(10,37) + 
        scale_x_continuous(breaks = scales::pretty_breaks(n = 5)) + 
        scale_y_continuous(breaks = c(12, 20, 28, 36),
                   labels = c(12, 20, 28, 36)) +       
        theme_bw(base_size = 12, base_family = "Helvetica") + 
        theme(panel.border = element_rect(color = "black"), panel.grid.major = element_blank(), panel.grid.minor = element_blank())
      }  + 
      # theme(axis.title.x=element_blank(), axis.text.x=element_blank(),axis.text.y=element_blank()) + 
      theme(legend.position = "none") + scale_color_manual(values=rev(gg_color_hue(4)))
    fig3e <- ggplotly(fig3e)
}  

make_fig_3h <- function(data_all,mod_CN) {
  mod_CN_Nitrate_pred <- make_mod_CNP_Nitrate_pred(data_all, mod_CN)
  # Tease it so that the shading does not go below C:N of 5 for display
  cn_shade_min <- exp(mod_CN_Nitrate_pred$fit - 2*mod_CN_Nitrate_pred$se.fit)
  cn_shade_min <- pmax(cn_shade_min,rep(5,length(cn_shade_min)))
  fig3h <- {ggplot(data = data_all, aes(log10(exp(logNO3)), exp(logCN))) + 
      # annotate("text", x=1.5*0.95, y=9*0.95, label= "(H)") +
      geom_point(aes(color = Nutlim), size = 0.75) +   
      geom_ribbon(aes(ymin=cn_shade_min, ymax=exp(fit +2*se.fit), x=log10(exp(logNO3))),
              data=mod_CN_Nitrate_pred, 
              alpha=0.4, 
              inherit.aes=FALSE) +
      geom_line(aes(y=exp(fit)), data=mod_CN_Nitrate_pred, cex = 0.75)+ 
      coord_cartesian(xlim = c(-1.0, 1.5),ylim = c(5, 9.2)) + 
      xlab("log10[NO3]") +
      ylab("C:N") +
      xlim(-1.0,1.5) + 
      ylim(5,9.2) + 
      scale_x_continuous(breaks = scales::pretty_breaks(n = 5)) + 
      theme_bw(base_size = 12, base_family = "Helvetica") + 
      theme(panel.border = element_rect(color = "black"), panel.grid.major = element_blank(), panel.grid.minor = element_blank())
    }  + 
    # theme(axis.text.y=element_blank()) + 
    theme(legend.position = "none") + scale_color_manual(values=rev(gg_color_hue(4)))
  fig3h <- ggplotly(fig3h)
}

make_fig_3c <- function(data_all,mod_CP_Nutcline_Nutlim_modGS) {
  mod_CP_Nutcline_Nutlim_pred <- make_mod_CNP_Nutcline_Nutlim_pred(data_all,
                                                                 mod_CP_Nutcline_Nutlim_modGS)
  fig3c <- {ggplot(data = data_all, aes(Nutcline_1uM_interp, exp(logCP))) + 
      # annotate("text", x=210*0.95, y=255*0.95, label= "(C)") +
      geom_point(aes(color = Nutlim), size = 0.75, alpha = 1.0) +   
      geom_line(aes(x = Nutcline_1uM_interp, y=exp(fit), group = Nutlim, color = Nutlim), data=mod_CP_Nutcline_Nutlim_pred, cex = 0.75) + 
      coord_cartesian(xlim = c(0,210), ylim = c(60, 275)) + 
      xlab("") +
      ylab("") +
      xlim(0,210) + 
      ylim(60,280) +
      scale_x_continuous(breaks = scales::pretty_breaks(n = 5)) + 
      scale_y_continuous(breaks = c(75, 125, 175, 225, 275),
                   labels = c(75, 125, 175, 225, 275)) + 
      theme_bw(base_size = 12, base_family = "Helvetica") + 
      theme(panel.border = element_rect(color = "black"), panel.grid.major = element_blank(), panel.grid.minor = element_blank())
    }  + theme(axis.title.x=element_blank(), axis.text.x=element_blank(),axis.title.y=element_blank(), axis.text.y=element_blank()) + theme(legend.position = "none") + scale_color_manual(values=rev(gg_color_hue(4)))
  fig3c <- ggplotly(fig3c)
}  

make_fig_3f <- function(data_all, mod_NP_Nutcline_Nutlim_modGS) {
  mod_NP_Nutcline_Nutlim_pred <- make_mod_CNP_Nutcline_Nutlim_pred(data_all,
                                                                 mod_NP_Nutcline_Nutlim_modGS)
  fig3f <- {ggplot(data = data_all, aes(Nutcline_1uM_interp, exp(logNP))) +
      # annotate("text", x=210*0.95, y=36*0.95, label= "(F)") +
      geom_point(aes(color = Nutlim), size = 0.75, alpha = 1.0) +   
      geom_line(aes(x = Nutcline_1uM_interp, y=exp(fit), group = Nutlim, color = Nutlim), data=mod_NP_Nutcline_Nutlim_pred, cex = 0.75) + 
      coord_cartesian(xlim = c(0,210), ylim = c(10, 37)) + 
      xlab("") +
      ylab("") +
      xlim(0,210) + 
      ylim(10,37) + 
      scale_x_continuous(breaks = scales::pretty_breaks(n = 5)) + 
      scale_y_continuous(breaks = c(12, 20, 28, 36),
                   labels = c(12, 20, 28, 36)) +      
      theme_bw(base_size = 12, base_family = "Helvetica") + 
      theme(panel.border = element_rect(color = "black"), panel.grid.major = element_blank(), panel.grid.minor = element_blank())
    }  + theme(axis.title.x=element_blank(), axis.text.x=element_blank(),axis.title.y=element_blank(), axis.text.y=element_blank()) + theme(legend.position = "none") + scale_color_manual(values=rev(gg_color_hue(4)))
  fig3f <- ggplotly(fig3f)
}

make_fig_3i <- function(data_all, mod_CN_Nutcline_Nutlim_modGS) {
  mod_CN_Nutcline_Nutlim_pred <- make_mod_CNP_Nutcline_Nutlim_pred(data_all,
                                                                 mod_CN_Nutcline_Nutlim_modGS)
  fig3i <- {ggplot(data = data_all, aes(Nutcline_1uM_interp, exp(logCN))) +
      # annotate("text", x=210*0.95, y=9*0.95, label= "(I)") +
      geom_point(aes(color = Nutlim), size = 0.75, alpha = 1.0) +
      geom_line(aes(x = Nutcline_1uM_interp, y=exp(fit), group = Nutlim, color = Nutlim), data=mod_CN_Nutcline_Nutlim_pred, cex = 0.75) +
      coord_cartesian(xlim = c(0,210), ylim = c(5, 9.2)) +
      xlab("Nutricline (m)") +
      ylab("") +
      xlim(0,210) +
      ylim(5,9.2) +
      scale_x_continuous(breaks = scales::pretty_breaks(n = 5)) +
      theme_bw(base_size = 12, base_family = "Helvetica") +
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
  # fig3
  orca(fig3, dest)
  server <- orca_serve()
  server$close()
}

# Version 2
# Function to plot C:P under different nutrient limitation under varying Nutricline for a given model and prediction data with observation
plot_CP_pred_Nutcline_nutlim_new <- function(mod_CP_pred, data, model_name) {
  data <- data %>% drop_na(Nutlim)
  data$Nutlim <- factor(data$Nutlim, levels = c("P-lim","N-lim","PN-colim","Fe-lim"))
  fig <- {ggplot(data, aes(x=Nutcline_1uM_interp, y=exp(logCP), group=Nutlim)) +
      facet_wrap(~Nutlim, drop = T) +
      geom_ribbon(aes(ymin=exp(fit - 2*se.fit), ymax=exp(fit + 2*se.fit), x=Nutcline_1uM_interp),
                  data=mod_CP_pred, 
                  alpha=0.3, 
                  inherit.aes=FALSE) +
      # xlim(0,210) + 
      # ylim(60, 280) +   
      coord_cartesian(xlim = c(0,200), ylim = c(75, 280)) + 
      scale_x_continuous(breaks = scales::pretty_breaks(n = 5)) + 
      scale_y_continuous(breaks = c(75, 125, 175, 225, 275),
                         labels = c(75, 125, 175, 225, 275)) +       
      geom_point(aes(color = Nutlim), size = 0.75) +
      geom_line(aes(y=exp(fit), color = Nutlim), data=mod_CP_pred, size = 0.75) +
      scale_color_manual(values = gg_color_hue_4_rbgp)+
      # ggtitle(model_name) + 
      # xlab("Nutricline (m)") +
      ylab("")
  } +
    theme_bw(base_size = 12, base_family = "Helvetica") + 
    theme(
      strip.background = element_blank(),
      strip.text.x = element_blank()) + 
    theme(panel.border = element_rect(color = "black"), panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + theme(legend.position = "none")
}

# Function to plot N:P under different nutrient limitation under varying Nutricline for a given model and prediction data with observation
plot_NP_pred_Nutcline_nutlim_new <- function(mod_NP_pred, data, model_name) {
  data <- data %>% drop_na(Nutlim)
  data$Nutlim <- factor(data$Nutlim, levels = c("P-lim","N-lim","PN-colim","Fe-lim"))
  fig <- {ggplot(data, aes(x=Nutcline_1uM_interp, y=exp(logNP), group=Nutlim)) +
      facet_wrap(~Nutlim) +
      geom_ribbon(aes(ymin=exp(fit - 2*se.fit), ymax=exp(fit + 2*se.fit), x=Nutcline_1uM_interp),
                  data=mod_NP_pred, 
                  alpha=0.3, 
                  inherit.aes=FALSE) +
      xlim(0,200) + 
      ylim(12, 37) +
      coord_cartesian(xlim = c(0,200), ylim = c(12, 37)) + 
      scale_x_continuous(breaks = scales::pretty_breaks(n = 5)) + 
      scale_y_continuous(breaks = c(12, 16, 20, 24, 28, 32, 36),
                         labels = c(12, 16, 20, 24, 28,32, 36)) +      
      geom_point(aes(color = Nutlim), size = 0.75) +
      geom_line(aes(y=exp(fit), color = Nutlim), data=mod_NP_pred, size = 0.75) +
      scale_color_manual(values = gg_color_hue_4_rbgp)+
      # ggtitle(model_name) + 
      labs(x = 'Nutricline (m)', y = 'N:P')} + 
    theme_bw(base_size = 12, base_family = "Helvetica") + 
    theme(
      strip.background = element_blank(),
      strip.text.x = element_blank()) + 
    theme(panel.border = element_rect(color = "black"), panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + theme(legend.position = "none")
}

# Function to plot C:N under different nutrient limitation under varying Nutricline for a given model and prediction data with observation
plot_CN_pred_Nutcline_nutlim_new <- function(mod_CN_pred, data, model_name) {
  data <- data %>% drop_na(Nutlim)
  data$Nutlim <- factor(data$Nutlim, levels = c("P-lim","N-lim","PN-colim","Fe-lim"))
  fig <- {ggplot(data, aes(x=Nutcline_1uM_interp, y=exp(logCN), group=Nutlim)) +
      facet_wrap(~Nutlim) +
      geom_ribbon(aes(ymin=exp(fit - 2*se.fit), ymax=exp(fit + 2*se.fit), x=Nutcline_1uM_interp),
                  data=mod_CN_pred, 
                  alpha=0.3, 
                  inherit.aes=FALSE) +
      xlim(0,200) + 
      ylim(5, 9.5) +
      xlab("Nutricline (m)") +
      ylab("") + 
      coord_cartesian(xlim = c(0,200), ylim = c(5, 9.5)) + 
      scale_x_continuous(breaks = scales::pretty_breaks(n = 5)) + 
      scale_y_continuous(breaks = c(5, 6, 7, 8, 9),
                         labels = c(5, 6, 7, 8, 9)) +  
      geom_point(aes(color = Nutlim), size = 0.75) +
      geom_line(aes(y=exp(fit), color = Nutlim), data=mod_CN_pred, size = 0.75) +
      scale_color_manual(values = gg_color_hue_4_rbgp) +
      # ggtitle(model_name) + 
      labs(x = 'Nutricline (m)', y = 'C:N')
    } + 
    theme_bw(base_size = 12, base_family = "Helvetica") + 
    theme(
      strip.background = element_blank(),
      strip.text.x = element_blank()) + 
    theme(panel.border = element_rect(color = "black"), panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + theme(legend.position = "none")
}

make_plot_CNP_pred_Nutcline_nutlim_new <- function(data, mod_CNP, stoich) {
  # Reorder factor level in nutrient limitation so P-limiation is plotted last.
  data$Nutlim <- factor(data$Nutlim, levels = c("P-lim","PN-colim", "N-lim","Fe-lim"))
  mod_CNP_pred <- with(data,
                       expand.grid(Nutcline_1uM_interp =seq(min(0), max(300), length=300),
                                   SST= mean(data$SST),
                                   logNO3 = mean(data$logNO3),
                                   Nutlim=levels(Nutlim)))
  # SST= 18))
  mod_CNP_pred <- pred_CNP_Nutcline_nutlim(mod_CNP,mod_CNP_pred)
  if (stoich == "CP") {
    fig <- plot_CP_pred_Nutcline_nutlim_new(mod_CNP_pred, data, stoich)
  } else if (stoich == "NP") {
    fig <- plot_NP_pred_Nutcline_nutlim_new(mod_CNP_pred, data, stoich)
  } else if (stoich == "CN") {
    fig <- plot_CN_pred_Nutcline_nutlim_new(mod_CNP_pred, data, stoich)
  }
  fig <- ggplotly(fig)
  # ggsave(dest,
  #        width = 8, # The width of the plot in inches
  #        height = 5)
}

make_fig_3_new <- function(dest, fig_out_folder, 
                       mod_CP,
                       mod_NP,
                       mod_CN,
                       mod_CP_Nutcline_Nutlim_modGS,
                       mod_NP_Nutcline_Nutlim_modGS,
                       mod_CN_Nutcline_Nutlim_modGS,
                       data_all,
                       data_gam) {
  fig3a <- make_fig_3a(data_all, mod_CP)
  fig3c <- make_fig_3d(data_all, mod_NP)
  fig3e <- make_fig_3g(data_all, mod_CN)
  
  fig3b <- make_plot_CNP_pred_Nutcline_nutlim_new(data_gam, 
                                                  mod_CP_Nutcline_Nutlim_modGS, 
                                                  stoich = "CP")
  fig3d <- make_plot_CNP_pred_Nutcline_nutlim_new(data_gam, 
                                                  mod_NP_Nutcline_Nutlim_modGS, 
                                                  stoich = "NP")
  fig3f <- make_plot_CNP_pred_Nutcline_nutlim_new(data_gam, 
                                                  mod_CN_Nutcline_Nutlim_modGS, 
                                                  stoich = "CN")
  fig3 <-plotly::subplot(fig3a, 
                         fig3b,
                         fig3e,
                         fig3f,
                         nrows = 2,
                         shareX = FALSE, shareY = FALSE, titleX = TRUE, titleY = TRUE, margin = 0.03,
                         heights = c(0.5, 0.5)) %>% layout(height = 600, width = 800)
  # fig3
  orca(fig3, dest)
  server <- orca_serve()
  server$close()
}

make_fig_3_new_NP <- function(dest, fig_out_folder, 
                           mod_NP,
                           mod_NP_Nutcline_Nutlim_modGS,
                           data_all,
                           data_gam) {
  fig3_left <- make_fig_3d(data_all, mod_NP)
  fig3_right <- make_plot_CNP_pred_Nutcline_nutlim_new(data_gam, 
                                                  mod_NP_Nutcline_Nutlim_modGS, 
                                                  stoich = "NP")
  fig3 <-plotly::subplot(fig3_left, 
                         fig3_right,
                         nrows = 1,
                         shareX = FALSE, shareY = FALSE, titleX = TRUE, titleY = TRUE, margin = 0.03,
                         heights = c(1.0)) %>% layout(height = 300, width = 800)
  # fig3
  orca(fig3, dest)
  server <- orca_serve()
  server$close()
}

make_fig_3_NO3only <- function(dest, fig_out_folder, 
                           mod_CP,
                           mod_NP,
                           mod_CN,
                           data_all,
                           data_gam) {
  fig3b <- make_fig_3b(data_all, mod_CP)
  fig3e <- make_fig_3e(data_all, mod_NP)
  fig3h <- make_fig_3h(data_all, mod_CN)
  
  fig3 <-plotly::subplot(fig3b, 
                         fig3e,
                         fig3h,
                         nrows = 1,
                         shareX = FALSE, shareY = FALSE, titleX = TRUE, titleY = TRUE, margin = 0.03,
                         heights = c(1.0)) %>% layout(height = 200, width = 800)
  # fig3
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
		       plot_title_topleft,
		       plot_title_topright,
		       colbarbreak,
		       colbartitle,
		       colbarlimits,
		       NP = FALSE) {  

  lonlat_grid <- cesm_lonlat_info$lonlat_grid
  lonlat_grid_lon <- cesm_lonlat_info$lonlat_grid_lon
  lonlat_grid_lat <- cesm_lonlat_info$lonlat_grid_lat
  
  # Top Left = Delta C:P of model GS (full) 2D map
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
  
  regions <- as.vector(cesm_lonlat_info$lonlat_regions) %>% 
    factor(levels = c("Polar", "Subpolar", "Subtropical","Tropical"))
  
  fig_topleft <- ggplot(data = delcp_full_plot, aes(x = lon, y = lat)) +
    geom_raster(aes(fill = value)) +
    coord_cartesian(xlim = c(-179.5, 179.5), ylim = c(-89.5, 89.5), expand = F) + 
  xlab("Longitude") +
  ylab("Latitude") + 
  scale_y_continuous(breaks=c(-70,-35, 0, 35, 70)) + 
  scale_fill_cmocean(name = "balance", na.value = "black",breaks = colbarbreak,limits=colbarlimits,discrete = FALSE) + 
  ggtitle(plot_title_topleft) + 
  theme_bw(base_size = 10, base_family = "Helvetica") + 
  theme(panel.border = element_rect(fill = NA, colour = "black", size = 1),
          axis.text = element_text(colour = "black"),
          axis.ticks = element_line(colour = "black"), legend.position = "bottom") +
  guides(fill = guide_colorbar(barwidth = 14, barheight = 0.75,title = colbartitle, title.position = "bottom"))
  
  # Top Right = Model Confidence 2D map
  modelconf_cp_full <- diffcp_full_PosCount_grid/2000*100
  modelconf_cp_full_plot <- reshape2::melt(modelconf_cp_full)
  modelconf_cp_full_plot$lon <- as.vector(lonlat_grid_lon)
  modelconf_cp_full_plot$lat <- as.vector(lonlat_grid_lat)
  
  fig_topright <- ggplot(data = modelconf_cp_full_plot, aes(x = lon, y = lat)) +
    geom_raster(aes(fill = value)) +
    coord_cartesian(xlim = c(-179.5, 179.5), ylim = c(-89.5, 89.5), expand = F) + 
  labs(y = NULL) +
  xlab("Longitude") + 
  scale_y_continuous(breaks=c(-70,-35, 0, 35, 70)) + 
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
  ggtitle(plot_title_topright) + 
  theme_bw(base_size = 10, base_family = "Helvetica") + 
  theme(panel.border = element_rect(fill = NA, colour = "black", size = 1),
          axis.text = element_text(colour = "black"),
          axis.ticks = element_line(colour = "black"), legend.position = "bottom") +
  guides(fill = guide_colorbar(barwidth = 14, barheight = 0.75, title = "Model agreement", title.position = "bottom"))
  
  # Put C:P changes, regions, and model agreement info into a single data-frame
  df_fig4 <- data.frame(regions, as.vector(delcp_full), as.vector(modelconf_cp_full))
  
  # Bottom Left = Delta C:P of model GS (full) Violin Plot by regions
  fig_bottomleft  <- {ggplot(data = df_fig4, mapping = aes(y = delcp_full, x = regions)) + 
      geom_violin(aes(fill = regions), show.legend = FALSE) +
      geom_hline(yintercept = 0, linetype = "dashed", color = "black") + 
      ggtitle("(C)") + 
      scale_fill_hue(direction = -1) + 
      scale_x_discrete(limits=rev) + 
      labs(y = 'DeltaC:P', x = '') + 
      {if (NP) labs(y = 'DeltaN:P', x = '')} + 
      # stat_summary(fun = "mean", geom="point",color = "black", width = 0.2) +
      scale_y_continuous(breaks = c(-30, -20, -10, 0, 10, 20, 30), limits = c(-30, 30)) + 
      {if(NP) scale_y_continuous(breaks = c(-6, -4, -2, 0, 2, 4, 6), limits = c(-6, 6))} + 
      theme_bw() + 
      theme(panel.border = element_rect(color = "black"), panel.grid.major = element_blank(), panel.grid.minor = element_blank())
  } + coord_flip() + theme(axis.text.y = element_text(angle = 45))
  
  # Bottom Right = Model agreement C:P of model GS (full) Violin Plot by regions
  fig_bottomright  <- {ggplot(data = df_fig4, mapping = aes(y = modelconf_cp_full, x = regions)) + 
      geom_violin(aes(fill = regions), show.legend = FALSE) +
      geom_hline(yintercept = 50, linetype = "dashed", color = "black") + 
      ggtitle("(D)") + 
      scale_fill_hue(direction = -1) + 
      scale_x_discrete(limits=rev) + 
      scale_y_continuous(breaks = c(0, 25, 50, 75, 100), 
                         labels = c("100%-", "75%-", "50%-/50%+", "75%+", "100%+"),
                         limits = c(0, 100)) + 
      labs(y = 'Model agreement', x = '') + 
      theme_bw() + 
      theme(panel.border = element_rect(color = "black"), panel.grid.major = element_blank(), panel.grid.minor = element_blank())
  } + coord_flip() + theme(axis.text.y = element_text(angle = 45))
  
  # Convert the figures to grobs
  fig_topleft_grob <- ggplotGrob(fig_topleft)
  fig_topright_grob <- ggplotGrob(fig_topright)
  fig_bottomleft_grob <- ggplotGrob(fig_bottomleft)
  fig_bottomright_grob <- ggplotGrob(fig_bottomright)
  gg <- ggplot() +
  # coord_equal(xlim = c(1, 10), ylim = c(1, 6), expand = F) +
  coord_equal(xlim = c(1, 10), ylim = c(1, 8), expand = F) +
    annotation_custom(fig_topleft_grob,
                      # xmin = 1, xmax = 5.5, ymin = 1, ymax = 6) +  
                      xmin = 1, xmax = 5.5, ymin = 4, ymax = 8) +  
    annotation_custom(fig_topright_grob,
                      # xmin = 5.5, xmax = 10, ymin = 1, ymax = 6) + 
                      xmin = 5.5, xmax = 10, ymin = 4, ymax = 8) + 
    annotation_custom(fig_bottomleft_grob,
                      # xmin = 1, xmax = 5.5, ymin = 1, ymax = 6) +  
                      xmin = 1, xmax = 5.5, ymin = 1, ymax = 4) +  
    annotation_custom(fig_bottomright_grob,
                      # xmin = 5.5, xmax = 10, ymin = 1, ymax = 6) + 
                      xmin = 5.5, xmax = 10, ymin = 1, ymax = 4) + 
    theme(panel.border = element_blank())
  # gg
  ggsave(dest,
        width = 8, # The width of the plot in inches
#        height = 5)
        height = 8)

}

###############
# EXTENDED DATA FIGURES
###############
# Function to Make Extended Figure 1a
plot_nutcline_glodap <- function(glodap_lonlat_surfdepth_info,
                                 nutcline_glodap_data,
                                 plot_title) {
  par(mar=c(1.5, 1, 1.5, 1))
  lonlim <- c(-180, 180)
  latlim <- c(-90, 90)
  Zlim = c(0,300)
  
  drawPalette(zlim = Zlim, 
              col=oce.colorsJet, 
              at = seq(0,300,20), 
              pos = 4, 
              drawTriangles = TRUE,
              las = 1,
              cex = 0.75)
  
  mapPlot(coastlineWorld, 
        projection="+proj=robin",
        col="black", 
        longitudelim=lonlim, 
        latitudelim=latlim, 
        axes = TRUE,
        drawBox = FALSE,
        grid = FALSE,
        clip = TRUE,
        border = "black",
        lonlabels = TRUE,
        latlabels = TRUE,
        geographical = 0,
        axisStyle = 1)
  
  mapImage(glodap_lonlat_surfdepth_info$lon,
           glodap_lonlat_surfdepth_info$lat,
           nutcline_glodap_data,
           zlim = Zlim, 
           col = oceColorsJet(240))
  
  title(plot_title, outer=FALSE, cex=0.75) # title for overall plot  
}

# Function to Make Extended Figure 1c = Difference in nutricline depth (GLODAP - CESM)
plot_nutcline_glodap_diff_cesm <- function(cesm_lonlat_info,
                                           nutcline_glodap_data,
                                           nutcline_cesm_data,
                                           plot_title) {
  par(mar=c(1.5, 1, 1.5, 1))
  lonlim <- c(-180, 180)
  latlim <- c(-90, 90)

  Zlim = c(-160,160)
  drawPalette(zlim = Zlim, 
              col=cmocean('balance')(240), 
              at = seq(-160,160,40), 
              pos = 4, 
              drawTriangles = TRUE,
              las = 1,
              cex = 0.75)

  mapPlot(coastlineWorld, 
        projection="+proj=robin",
        col="black", 
        longitudelim=lonlim, 
        latitudelim=latlim, 
        axes = TRUE,
        drawBox = FALSE,
        grid = FALSE,
        clip = TRUE,
        border = "black",
        lonlabels = TRUE,
        latlabels = TRUE,
        geographical = 0,
        axisStyle = 1)
  
  mapImage(cesm_lonlat_info$lon,
           cesm_lonlat_info$lat,
           nutcline_glodap_data - nutcline_cesm_data,
           zlim = Zlim, 
           col = cmocean('balance')(240))
  
  title(plot_title, outer=FALSE, cex=0.75) # title for overall plot
} 

# Function to Make Extended Figure 1d (Correlation between GLODAP Nutricline and CESM Nutricline)
plot_corr_nutcline_glodap_cesm <- function(cesm_lonlat_info,
                                           nutcline_glodap_data,
                                           nutcline_historic,
                                           plot_title) {
  par(mar=c(4.0, 1, 4.0, 1))
  lonlat_model_mask <- outer(rep(1,length(cesm_lonlat_info$lon)), ifelse(abs(cesm_lonlat_info$lat) < 55, 1, NA))
  nutcline_gt0_mask <- ifelse(nutcline_historic > 0 & 
                                   nutcline_glodap_data > 0, 
                                 1, NA)
  
  par(pty="s")
  smoothScatter(nutcline_historic*lonlat_model_mask*nutcline_gt0_mask,
                nutcline_glodap_data*lonlat_model_mask*nutcline_gt0_mask, asp = 1,
                xlim = c(0,300), ylim = c(0,300),
                xlab = "Nutricline depth CESM (2010-2014) [m]",
                ylab = "Nutricline depth GLODAP [m]",
		cex.axis = 0.5,
		cex.lab = 0.5
                )
  abline(a=0, b=1, lty = 2, cex = 1.5)
  
  # Plot log linear regression line that goes through origin
  y <- as.vector(nutcline_glodap_data*lonlat_model_mask*nutcline_gt0_mask)
  x <- as.vector(nutcline_historic*lonlat_model_mask*nutcline_gt0_mask)

  abline(lm(y ~ x + 0), col = 4, lwd = 3)
  Model<-lm(y~x + 0)
  coef <- round(coef(lm(y ~ x + 0)), 2)
  text(120, 300,  paste("GLODAP = ", coef[1], "CESM"),cex = 0.5, col = 4, font = 2)
  text(120, 280,  paste("R2 = ", format(summary(Model)$r.squared,digits=3)), cex = 0.5)
  text(290, 300, paste(plot_title), cex = 0.75, font = 2)
}

# Function to make Ed_Fig_1a
make_ed_fig_1 <- function(dest,
                          fig_out_folder,
                          glodap_lonlat_surfdepth_info,
                          cesm_lonlat_info,
                          nutcline_glodap_data,
                          nutcline_historic_uncorrected,
                          plot_title_1a,
                          plot_title_1b,
                          plot_title_1c,
                          plot_title_1d) {
  pdf(dest, width = 8, height = 5)
  par(mfrow = c(2,2))
  fig_a <- plot_nutcline_glodap(glodap_lonlat_surfdepth_info, 
                                nutcline_glodap_data, 
                                plot_title_1a)
  fig_b <- plot_nutcline_glodap(cesm_lonlat_info,
                                nutcline_historic_uncorrected,  
                                plot_title_1b)
  fig_c <- plot_nutcline_glodap_diff_cesm(cesm_lonlat_info,
                                          nutcline_glodap_data,
                                          nutcline_historic_uncorrected, 
                                          plot_title_1c)
  fig_d <- plot_corr_nutcline_glodap_cesm(cesm_lonlat_info, 
                                          nutcline_glodap_data,
                                          nutcline_historic_uncorrected,
                                          plot_title_1d) 
  fig_a
  fig_b
  fig_c
  fig_d
  dev.off()
}

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
make_ed_fig_6 <- function(dest,
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

# Function to make Extended Data Fig. 7 (Comparing C:P and N:P (historic, SSP370, and change))
make_ed_fig_7 <- function(dest,
                          fig_out_folder,
                          cesm_lonlat_info,
                          CNP_gam_cesm,
                          POM_all_binned) {
  
  lonlat_grid <- cesm_lonlat_info$lonlat_grid
  lonlat_grid_lon <- cesm_lonlat_info$lonlat_grid_lon
  lonlat_grid_lat <- cesm_lonlat_info$lonlat_grid_lat
  
  # Top Left (2,3,1) = C:P Historic (full)
  pred_cp_historic_full <- CNP_gam_cesm$pred_cp_historic_full
  cp_historic_plot <- reshape2::melt(pred_cp_historic_full)
  cp_historic_plot$lon <- as.vector(lonlat_grid_lon)
  cp_historic_plot$lat <- as.vector(lonlat_grid_lat)
  pal = turbo(20)
  fig_231 <- ggplot(data = cp_historic_plot, aes(x = lon, y = lat)) +
      coord_cartesian(xlim = c(-179.5, 179.5), ylim = c(-89.5, 89.5), expand = F) + 
      xlab("") +
      ylab("") + 
        geom_raster(aes(fill = value)) +
    scale_fill_gradientn(colors = pal,
                              na.value = "gray30",
                              limits = c(50, 230),
                              breaks = c(75, 100, 120, 140, 160,180,200,220),oob=squish) +
    geom_point(pch =21, data = POM_all_binned, aes(x = Longitude, y = Latitude, fill = exp(logCP)), size =2.0,show.legend = F, color = "transparent") +
    ggtitle("(A) C:P (2010s)") +
    theme_bw(base_size = 18, base_family = "Helvetica") +
    theme(legend.position = "right",legend.key.size = unit(0.2, "cm")) + 
    theme(panel.border =  element_rect(color = "black"), panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + 
    guides(fill = guide_colorsteps(barwidth = 0.75, barheight = 14, title = "", title.position = "top",show.limits = FALSE))
  
  # Top Middle  (2,3,2) = C:P SSP370 (full)
  pred_cp_SSP370_full <- CNP_gam_cesm$pred_cp_SSP370_full
  cp_SSP370_plot <- reshape2::melt(pred_cp_SSP370_full)
  cp_SSP370_plot$lon <- as.vector(lonlat_grid_lon)
  cp_SSP370_plot$lat <- as.vector(lonlat_grid_lat)
  pal = turbo(20)
  fig_232 <- ggplot(data = cp_SSP370_plot, aes(x = lon, y = lat)) +
      coord_cartesian(xlim = c(-179.5, 179.5), ylim = c(-89.5, 89.5), expand = F) + 
      xlab("") +
      ylab("") + 
    geom_raster(aes(fill = value)) +
    scale_fill_gradientn(colors = pal,
                              na.value = "gray30",
                              limits = c(50, 230),
                              breaks = c(75, 100, 120, 140, 160,180,200,220),oob=squish) +
    ggtitle("(B) C:P (2090s)") +
    theme_bw(base_size = 18, base_family = "Helvetica") +
    theme(legend.position = "right",legend.key.size = unit(0.2, "cm")) + 
    theme(panel.border =  element_rect(color = "black"), panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + 
    guides(fill = guide_colorsteps(barwidth = 0.75, barheight = 14, title = "", title.position = "top",show.limits = TRUE))
  
  # Top Left (2,3,3) = Delta C:P of model GS (full) = Same as Main Fig. 3
  delcp_full <- pred_cp_SSP370_full - pred_cp_historic_full
  delcp_full_plot <- reshape2::melt(delcp_full)
  delcp_full_plot$lon <- as.vector(lonlat_grid_lon)
  delcp_full_plot$lat <- as.vector(lonlat_grid_lat)
  
  fig_233<- ggplot(data = delcp_full_plot, aes(x = lon, y = lat)) +
    geom_raster(aes(fill = value)) +
    coord_cartesian(xlim = c(-179.5, 179.5), ylim = c(-89.5, 89.5), expand = F) + 
      xlab("") +
      ylab("") + 
    scale_fill_cmocean(name = "balance", na.value = "black",breaks = seq(-25,25,5),limits=c(-30, 30),discrete = FALSE) + 
    ggtitle("(C) DC:P (2090s - 2010s)") + 
    theme_bw(base_size = 18, base_family = "Helvetica") +
    theme(legend.position = "right",legend.key.size = unit(0.2, "cm")) + 
    theme(panel.border =  element_rect(color = "black"), panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + 
    guides(fill = guide_colorbar(barwidth = 0.75, barheight = 14, title = "", title.position = "top"))
  
  # Bottom Left (2,3,4) = N:P Historic (full)
  pred_np_historic_full <- CNP_gam_cesm$pred_np_historic_full
  np_historic_plot <- reshape2::melt(pred_np_historic_full)
  np_historic_plot$lon <- as.vector(lonlat_grid_lon)
  np_historic_plot$lat <- as.vector(lonlat_grid_lat)
  pal = turbo(20)
  fig_234 <- ggplot(data = np_historic_plot, aes(x = lon, y = lat)) +
      coord_cartesian(xlim = c(-179.5, 179.5), ylim = c(-89.5, 89.5), expand = F) + 
      xlab("") +
      ylab("") + 
    geom_raster(aes(fill = value)) +
    scale_fill_gradientn(colors = pal,
                              na.value = "gray30",
                              limits = c(10, 36),
                              breaks = c(12, 14, 16,18,20,22,24,28,32),oob=squish) +

    geom_point(pch =21, data = POM_all_binned, aes(x = Longitude, y = Latitude, fill = exp(logNP)), size =2.0,show.legend = F, color = "transparent") +
    ggtitle("(D) N:P (2010s)") +
    theme_bw(base_size = 18, base_family = "Helvetica") +
    theme(legend.position = "right",legend.key.size = unit(0.2, "cm")) + 
    theme(panel.border =  element_rect(color = "black"), panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + 
    guides(fill = guide_colorsteps(barwidth = 0.75, barheight = 14, title = "", title.position = "top",show.limits = F))
  
  # Bottom Middle (2,3,5) = N:P SSP370 (full)
  pred_np_SSP370_full <- CNP_gam_cesm$pred_np_SSP370_full
  np_SSP370_plot <- reshape2::melt(pred_np_SSP370_full)
  np_SSP370_plot$lon <- as.vector(lonlat_grid_lon)
  np_SSP370_plot$lat <- as.vector(lonlat_grid_lat)
  pal = turbo(20)
  fig_235 <- ggplot(data = np_SSP370_plot, aes(x = lon, y = lat)) +
      coord_cartesian(xlim = c(-179.5, 179.5), ylim = c(-89.5, 89.5), expand = F) + 
      xlab("") +
      ylab("") + 
    geom_raster(aes(fill = value)) +
    scale_fill_gradientn(colors = pal,
                              na.value = "gray30",
                              limits = c(10, 36),
                              breaks = c(12, 14, 16,18,20,22,24,28,32),oob=squish) +
    ggtitle("(E) N:P (2090s)") +
    theme_bw(base_size = 18, base_family = "Helvetica") +
    theme(legend.position = "right",legend.key.size = unit(0.2, "cm")) + 
    theme(panel.border =  element_rect(color = "black"), panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + 
    guides(fill = guide_colorsteps(barwidth = 0.75, barheight = 14, title = "", title.position = "top",show.limits = F))
  
  # Bottom Right (2,3,6) = Delta N:P of model GS (full) 
  delnp_full <- pred_np_SSP370_full - pred_np_historic_full
  delnp_full_plot <- reshape2::melt(delnp_full)
  delnp_full_plot$lon <- as.vector(lonlat_grid_lon)
  delnp_full_plot$lat <- as.vector(lonlat_grid_lat)
  
  fig_236 <- ggplot(data = delnp_full_plot, aes(x = lon, y = lat)) +
    geom_raster(aes(fill = value)) +
    coord_cartesian(xlim = c(-179.5, 179.5), ylim = c(-89.5, 89.5), expand = F) + 
      xlab("") +
      ylab("") + 
    scale_fill_cmocean(name = "balance", na.value = "black",breaks = seq(-5,5,1),limits=c(-6, 6),discrete = FALSE) + 
    ggtitle("(F) DN:P (2090s - 2010s)") + 
    theme_bw(base_size = 18, base_family = "Helvetica") +
    theme(legend.position = "right",legend.key.size = unit(0.2, "cm")) + 
    theme(panel.border =  element_rect(color = "black"), panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + 
    guides(fill = guide_colorbar(barwidth = 0.75, barheight = 14, title = "", title.position = "top"))
  
  
  # Convert the figures to grobs
  fig_231_grob <- ggplotGrob(fig_231)
  fig_232_grob <- ggplotGrob(fig_232)
  fig_233_grob <- ggplotGrob(fig_233)
  fig_234_grob <- ggplotGrob(fig_234)
  fig_235_grob <- ggplotGrob(fig_235)
  fig_236_grob <- ggplotGrob(fig_236)
  gg <- ggplot() +
    coord_equal(xlim = c(1, 17.5), ylim = c(1, 11), expand = F) +
    annotation_custom(fig_231_grob,
                      xmin = 1, xmax = 6.5, ymin = 11, ymax = 6) +  
    annotation_custom(fig_232_grob,
                      xmin = 6.5, xmax = 12, ymin = 11, ymax = 6) +  
      annotation_custom(fig_233_grob,
                      xmin = 12, xmax = 17.5, ymin = 11, ymax = 6) + 
    annotation_custom(fig_234_grob,
                      xmin = 1, xmax = 6.5, ymin = 6, ymax = 1) +  
    annotation_custom(fig_235_grob,
                      xmin = 6.5, xmax = 12, ymin = 6, ymax = 1) +  
    annotation_custom(fig_236_grob,
                      xmin = 12, xmax = 17.5, ymin = 6, ymax = 1) + 
    theme(panel.border = element_blank())
  
  ggsave(dest,
        width = 18, # The width of the plot in inches
        height = 10)  
}

# Function to plot DelCNP and broken down into different driver's effect (for N:P, turn NP = TRUE)
make_fig_delcnp_driver <- function(dest,
                                   fig_out_folder,
                                   cesm_lonlat_info,
                                   CNP_gam_cesm,
                                   maintitle,
                                   colbarbreak,
                                   colbarlimits,
                                   NP = FALSE) {
  lonlat_grid <- cesm_lonlat_info$lonlat_grid
  lonlat_grid_lon <- cesm_lonlat_info$lonlat_grid_lon
  lonlat_grid_lat <- cesm_lonlat_info$lonlat_grid_lat
  
  pred_cp_SSP370_Tonly <- CNP_gam_cesm$pred_cp_SSP370_Tonly
  pred_cp_historic_full <- CNP_gam_cesm$pred_cp_historic_full
  pred_cp_SSP370_Nonly <- CNP_gam_cesm$pred_cp_SSP370_Nonly
  pred_cp_SSP370_Nutclineonly <- CNP_gam_cesm$pred_cp_SSP370_Nutclineonly
  pred_cp_SSP370_Nutlimonly <- CNP_gam_cesm$pred_cp_SSP370_Nutlimonly
  pred_cp_SSP370_full <- CNP_gam_cesm$pred_cp_SSP370_full
  
  if (NP){
      pred_cp_SSP370_Tonly <- CNP_gam_cesm$pred_np_SSP370_Tonly
      pred_cp_historic_full <- CNP_gam_cesm$pred_np_historic_full
      pred_cp_SSP370_Nonly <- CNP_gam_cesm$pred_np_SSP370_Nonly
      pred_cp_SSP370_Nutclineonly <- CNP_gam_cesm$pred_np_SSP370_Nutclineonly
      pred_cp_SSP370_Nutlimonly <- CNP_gam_cesm$pred_np_SSP370_Nutlimonly
      pred_cp_SSP370_full <- CNP_gam_cesm$pred_np_SSP370_full
  }
  
  # Fig (5,1,1) = T effect
  delcp_plot <- reshape2::melt(pred_cp_SSP370_Tonly - pred_cp_historic_full)
  delcp_plot$lon <- as.vector(lonlat_grid_lon)
  delcp_plot$lat <- as.vector(lonlat_grid_lat)
  fig_511 <- ggplot(data = delcp_plot, aes(x = lon, y = lat)) +
    coord_cartesian(xlim = c(-179.5, 179.5), ylim = c(-89.5, 89.5), expand = F) + 
    xlab("") +
    ylab("") + 
    geom_raster(aes(fill = value)) +
    scale_fill_cmocean(name = "balance", na.value = "gray30", 
                       breaks = colbarbreak,
                       limits = colbarlimits,discrete = FALSE,
                       oob=squish) +
    ggtitle("(A) Temperature") +
    theme_bw(base_size = 18, base_family = "Helvetica") +
    theme(legend.position = "right",legend.key.size = unit(0.2, "cm")) + 
    theme(panel.border =  element_rect(color = "black"), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.text.x = element_blank(),  axis.text.y = element_blank()) + 
    guides(fill = guide_colorbar(barwidth = 0.75, barheight = 12, title = "", title.position = "top",show.limits = FALSE))
  
  # Fig (5,1,2) = NO3 effect
  delcp_plot <- reshape2::melt(pred_cp_SSP370_Nonly - pred_cp_historic_full)
  delcp_plot$lon <- as.vector(lonlat_grid_lon)
  delcp_plot$lat <- as.vector(lonlat_grid_lat)
  fig_512 <- ggplot(data = delcp_plot, aes(x = lon, y = lat)) +
    coord_cartesian(xlim = c(-179.5, 179.5), ylim = c(-89.5, 89.5), expand = F) + 
    xlab("") +
    ylab("") + 
    geom_raster(aes(fill = value)) +
    scale_fill_cmocean(name = "balance", na.value = "gray30", 
                       breaks = colbarbreak,
                       limits =colbarlimits,discrete = FALSE,
                       oob=squish) +
    ggtitle("(B) Nitrate") +
    theme_bw(base_size = 18, base_family = "Helvetica") +
    theme(legend.position = "right",legend.key.size = unit(0.2, "cm")) + 
    theme(panel.border =  element_rect(color = "black"), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.text.x = element_blank(),  axis.text.y = element_blank()) + 
    guides(fill = guide_colorbar(barwidth = 0.75, barheight = 12, title = "", title.position = "top",show.limits = FALSE))
  
  # Fig (5,1,3) = Nutricline effect
  delcp_plot <- reshape2::melt(pred_cp_SSP370_Nutclineonly - pred_cp_historic_full)
  delcp_plot$lon <- as.vector(lonlat_grid_lon)
  delcp_plot$lat <- as.vector(lonlat_grid_lat)
  fig_513 <- ggplot(data = delcp_plot, aes(x = lon, y = lat)) +
    coord_cartesian(xlim = c(-179.5, 179.5), ylim = c(-89.5, 89.5), expand = F) + 
    xlab("") +
    ylab("") + 
    geom_raster(aes(fill = value)) +
    scale_fill_cmocean(name = "balance", na.value = "gray30", 
                       breaks = colbarbreak,
                      limits = colbarlimits,discrete = FALSE,
                      oob=squish) +
    ggtitle("(C) Nutricline") +
    theme_bw(base_size = 18, base_family = "Helvetica") +
    theme(legend.position = "right",legend.key.size = unit(0.2, "cm")) + 
    theme(panel.border =  element_rect(color = "black"), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.text.x = element_blank(),  axis.text.y = element_blank()) + 
    guides(fill = guide_colorbar(barwidth = 0.75, barheight = 12, title = "", title.position = "top",show.limits = FALSE))  
  
  # Fig (5,1,4) = Nutricline x Nutlim effect
  delcp_plot <- reshape2::melt(pred_cp_SSP370_Nutlimonly - pred_cp_historic_full)
  delcp_plot$lon <- as.vector(lonlat_grid_lon)
  delcp_plot$lat <- as.vector(lonlat_grid_lat)
  fig_514 <- ggplot(data = delcp_plot, aes(x = lon, y = lat)) +
    coord_cartesian(xlim = c(-179.5, 179.5), ylim = c(-89.5, 89.5), expand = F) + 
    xlab("") +
    ylab("") + 
    geom_raster(aes(fill = value)) +
    scale_fill_cmocean(name = "balance", na.value = "gray30", 
                       breaks = colbarbreak,
                       limits = colbarlimits,discrete = FALSE,
                       oob=squish) +
    ggtitle("(D) Nutricline x Nutlim") +
    theme_bw(base_size = 18, base_family = "Helvetica") +
    theme(legend.position = "right",legend.key.size = unit(0.2, "cm")) + 
    theme(panel.border =  element_rect(color = "black"), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.text.x = element_blank(),  axis.text.y = element_blank()) + 
    guides(fill = guide_colorbar(barwidth = 0.75, barheight = 12, title = "", title.position = "top",show.limits = FALSE))
  
  # Fig (5,1,5) = All effects
  delcp_plot <- reshape2::melt(pred_cp_SSP370_full - pred_cp_historic_full)
  delcp_plot$lon <- as.vector(lonlat_grid_lon)
  delcp_plot$lat <- as.vector(lonlat_grid_lat)
  fig_515 <- ggplot(data = delcp_plot, aes(x = lon, y = lat)) +
    coord_cartesian(xlim = c(-179.5, 179.5), ylim = c(-89.5, 89.5), expand = F) + 
    xlab("") +
    ylab("") + 
    geom_raster(aes(fill = value)) +
    scale_fill_cmocean(name = "balance", na.value = "gray30", 
                       breaks = colbarbreak,
                       limits = colbarlimits,discrete = FALSE,
                       oob=squish) +
    ggtitle("(E) All effects") +
    theme_bw(base_size = 18, base_family = "Helvetica") +
    theme(legend.position = "right",legend.key.size = unit(0.2, "cm")) + 
    theme(panel.border =  element_rect(color = "black"), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.text.x = element_blank(),  axis.text.y = element_blank()) + 
    guides(fill = guide_colorbar(barwidth = 0.75, barheight = 12, title = "", title.position = "top",show.limits = FALSE))  
  
  # Save all in one figure using grob
  fig_511_grob <- ggplotGrob(fig_511)
  fig_512_grob <- ggplotGrob(fig_512)
  fig_513_grob <- ggplotGrob(fig_513)
  fig_514_grob <- ggplotGrob(fig_514)
  fig_515_grob <- ggplotGrob(fig_515)
  gg <- ggplot() +
    coord_equal(xlim = c(1, 17), ylim = c(1, 8), expand = F) +
    annotation_custom(fig_511_grob,
                      xmin = 1, xmax = 5, ymin = 4.5, ymax = 8) +  
    annotation_custom(fig_512_grob,
                      xmin = 5, xmax = 9, ymin = 4.5, ymax = 8) +  
    annotation_custom(fig_513_grob,
                      xmin = 9, xmax = 13, ymin = 4.5, ymax = 8) + 
    annotation_custom(fig_514_grob,
                      xmin = 13, xmax = 17, ymin = 4.5, ymax = 8) +  
    annotation_custom(fig_515_grob,
                      xmin = 1, xmax = 5, ymin = 1, ymax = 4.5) +  
    theme(panel.border = element_blank()) +
    ggtitle(maintitle)
  
  ggsave(dest,
       width = 17, # The width of the plot in inches
       height = 8)
  
}

# Function to plot C:P under different nutrient limitation under varying Nutricline for a given model and prediction data with observation
plot_CP_pred_Nutcline_nutlim <- function(mod_CP_pred, data, model_name) {
  data <- data %>% drop_na(Nutlim)
  data$Nutlim <- factor(data$Nutlim, levels = c("P-lim","N-lim","PN-colim","Fe-lim"))
  {ggplot(data, aes(x=Nutcline_1uM_interp, y=exp(logCP), group=Nutlim)) +
  facet_wrap(~Nutlim, drop = T) +
  geom_ribbon(aes(ymin=exp(fit - 2*se.fit), ymax=exp(fit + 2*se.fit), x=Nutcline_1uM_interp),
              data=mod_CP_pred, 
              alpha=0.3, 
              inherit.aes=FALSE) +
  xlim(0,210) + 
  ylim(60, 280) +   
  coord_cartesian(xlim = c(0,210), ylim = c(60, 280)) + 
  scale_x_continuous(breaks = scales::pretty_breaks(n = 5)) + 
  scale_y_continuous(breaks = c(75, 100, 125, 150, 175, 200, 225, 250, 275),
                   labels = c(75, 100, 125, 150, 175, 200, 225, 250, 275)) +       
  geom_jitter(aes(color = Nutlim), size = 0.8) +
  geom_line(aes(y=exp(fit), color = Nutlim), data=mod_CP_pred, size = 0.75) +
  scale_color_manual(values = gg_color_hue_4_rbgp)+
  ggtitle(model_name) + 
  labs(x = 'Nutricline (m)', y = 'C:P')} + 
  theme_bw() + 
  theme(panel.border = element_rect(color = "black"), panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + theme(legend.position = "none")
}

# Function to plot N:P under different nutrient limitation under varying Nutricline for a given model and prediction data with observation
plot_NP_pred_Nutcline_nutlim <- function(mod_NP_pred, data, model_name) {
  data <- data %>% drop_na(Nutlim)
  data$Nutlim <- factor(data$Nutlim, levels = c("P-lim","N-lim","PN-colim","Fe-lim"))
  {ggplot(data, aes(x=Nutcline_1uM_interp, y=exp(logNP), group=Nutlim)) +
  facet_wrap(~Nutlim) +
  geom_ribbon(aes(ymin=exp(fit - 2*se.fit), ymax=exp(fit + 2*se.fit), x=Nutcline_1uM_interp),
              data=mod_NP_pred, 
              alpha=0.3, 
              inherit.aes=FALSE) +
  xlim(0,210) + 
  ylim(10, 37) +
  coord_cartesian(xlim = c(0,210), ylim = c(10, 37)) + 
  scale_x_continuous(breaks = scales::pretty_breaks(n = 5)) + 
  scale_y_continuous(breaks = c(12, 16, 20, 24, 28, 32, 36),
                   labels = c(12, 16, 20, 24, 28,32, 36)) +      
  geom_jitter(aes(color = Nutlim), size = 0.8) +
  geom_line(aes(y=exp(fit), color = Nutlim), data=mod_NP_pred, size = 0.75) +
  scale_color_manual(values = gg_color_hue_4_rbgp)+
  ggtitle(model_name) + 
  labs(x = 'Nutricline (m)', y = 'N:P')} + 
  theme_bw() + 
  theme(panel.border = element_rect(color = "black"), panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + theme(legend.position = "none")
}

# Function to plot C:N under different nutrient limitation under varying Nutricline for a given model and prediction data with observation
plot_CN_pred_Nutcline_nutlim <- function(mod_CN_pred, data, model_name) {
  data <- data %>% drop_na(Nutlim)
  data$Nutlim <- factor(data$Nutlim, levels = c("P-lim","N-lim","PN-colim","Fe-lim"))
  {ggplot(data, aes(x=Nutcline_1uM_interp, y=exp(logCN), group=Nutlim)) +
  facet_wrap(~Nutlim) +
  geom_ribbon(aes(ymin=exp(fit - 2*se.fit), ymax=exp(fit + 2*se.fit), x=Nutcline_1uM_interp),
              data=mod_CN_pred, 
              alpha=0.3, 
              inherit.aes=FALSE) +
  xlim(0,210) + 
  ylim(5, 9) +
  coord_cartesian(xlim = c(0,210), ylim = c(5, 9)) + 
  geom_jitter(aes(color = Nutlim), size = 0.8) +
  geom_line(aes(y=exp(fit), color = Nutlim), data=mod_CN_pred, size = 0.75) +
  scale_color_manual(values = gg_color_hue_4_rbgp)+
  ggtitle(model_name) + 
  labs(x = 'Nutricline (m)', y = 'C:N')} + 
  theme_bw() + 
  theme(panel.border = element_rect(color = "black"), panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + theme(legend.position = "none")
}

# Function make Nutrient vs CNP for different nutrient limitation types
make_plot_CNP_pred_Nutcline_nutlim <- function(dest,
                                               fig_out_folder,
                                               data, mod_CNP, stoich) {
  # Reorder factor level in nutrient limitation so P-limiation is plotted last.
  data$Nutlim <- factor(data$Nutlim, levels = c("P-lim","PN-colim", "N-lim","Fe-lim"))
  mod_CNP_pred <- with(data,
                      expand.grid(Nutcline_1uM_interp =seq(min(0), max(300), length=300),
                                  SST= mean(data$SST),
                                  logNO3 = mean(data$logNO3),
                                  Nutlim=levels(Nutlim)))
                                  # SST= 18))
  mod_CNP_pred <- pred_CNP_Nutcline_nutlim(mod_CNP,mod_CNP_pred)
  if (stoich == "CP") {
    plot_CP_pred_Nutcline_nutlim(mod_CNP_pred, data, stoich)
  } else if (stoich == "NP") {
    plot_NP_pred_Nutcline_nutlim(mod_CNP_pred, data, stoich)
  } else if (stoich == "CN") {
    plot_CN_pred_Nutcline_nutlim(mod_CNP_pred, data, stoich)
    }
  ggsave(dest,
        width = 8, # The width of the plot in inches
        height = 5)
}

###############
# SUPPLEMENTARY INFO FIGURES
###############
# Function to make cross validation comparison violin plot
make_plot_model_cv_CNP <- function(dest,
                                   fig_out_folder,
                                   cv_df,
                                   plottitle) {
  ggplot(cv_df,aes(x = model, y = rmse)) + 
    geom_violin() + 
    stat_summary(fun.data = mean_sdl, geom="point", size=4, color="red") + 
    stat_summary(aes(label=round(..y..,3)), fun=mean, geom="text", size=6, vjust = -0.5) + 
    ggtitle(plottitle) 
  ggsave(dest,
         width = 8, # The width of the plot in inches
         height = 5)
}

# Function to plot comparing samples from Version 1 and Version 2
make_map_ver1ver2 <- function(dest,
                              fig_out_folder,
                              dataPOM_all_ver1_surface_binned_1deg,
                              dataPOM_all_ver2_surface_binned_1deg) {
  YK <- map_data("world")
  fig <- ggplot() +
    coord_cartesian(xlim = c(-179.5, 179.5), ylim = c(-89.5, 89.5), expand = F) + 
    labs(y = NULL) +
    xlab("Longitude") + 
    ylab("Latitude") + 
    scale_y_continuous(breaks=c(-70,-35, 0, 35, 70)) + 
    geom_polygon(data = YK, aes(x=long, y = lat, group = group), color = "black", fill = "black") + 
    geom_point(data = drop_na(dataPOM_all_ver2_surface_binned_1deg,POCavg,PONavg,POPavg), 
               mapping = aes(x = Longitude, y = Latitude, color="r"), 
               size = 1.0, 
               shape = 15) + 
    geom_point(data = drop_na(dataPOM_all_ver1_surface_binned_1deg,POCavg_uM,PONavg_uM,POPavg_nM), 
               mapping = aes(x = Longitude, y = Latitude, color = "b"), 
               size = 1.0, 
               shape = 15) + 
    scale_colour_manual(name = "group", values=c("b" = "blue", "r"="red"), 
                        labels=c("b"="Martiny et al. 2013", "r"="This study")) +
    theme_bw(base_size = 10, base_family = "Helvetica") +
    theme(legend.position = "right", legend.box = "horizontal",legend.key.size = unit(0.5, "cm"),
          legend.title = element_blank()) +
    theme(panel.border = element_rect(color = "black"), panel.grid.major = element_blank(), panel.grid.minor = element_blank())
  ggsave(dest,
         width = 8, 
         height = 5)
}


# make_sp_fig_1 <- function(dest, fig_out_folder,POM_all) {
	
#	figa2 <- cp_boxplot(POM_all)
#	figa2_ply <- ggplotly(figa2)
#	figb2 <- np_boxplot(POM_all)
#	figb2_ply <- ggplotly(figb2)
#	figc2 <- cn_boxplot(POM_all)
#	figc2_ply <- ggplotly(figc2)

#        sp_fig_1 <-plotly::subplot(figa2_ply,figb2_ply, figc2_ply, nrows = 1, widths = c(0.33,0.34,0.33), shareX = FALSE, shareY = TRUE, titleX = TRUE, titleY = TRUE, margin = 0.01) 
#        sp_fig_1
#	orca(sp_fig_1, dest)
#	server <- orca_serve()
#	server$close()
#
#}


