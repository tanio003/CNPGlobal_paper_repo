######################
# FUNCTIONS for calculating deviances of each variables
######################
# Additive contribution was calculated by a sequential removal of the different parameters and a normalization with a null model. 
# See Note by S. Wood (https://stat.ethz.ch/pipermail/r-help/2009-July/397343.html) for the basic principles

#--------------------------------------------------------------
# Function to calculate % deviance explained for 2 variable case:
#---------------------------------------------------------------
# To test that this function works, try running the following case study and you should not warning
# dat <- gamSim(1,n=400,dist="normal",scale=2)
# test_result <- deviance2variables(xvar1 = "x0", xvar2 = "x1", yvar = "y", data=dat)
#--------------------------------------------------------------
deviance2variables <- function(xvar1, xvar2, yvar, data) {
  b1 <- gam(as.formula(paste(yvar, "~", "s(",xvar1,")","+", "s(",xvar2,")")),data = data, method = "REML")
  b2 <- gam(as.formula(paste(yvar, "~", "s(",xvar2,")")),data = data, method = "REML", sp = b1$sp[c(2)])
  b3 <- gam(as.formula(paste(yvar, "~", "s(",xvar1,")")),data = data, method = "REML", sp = b1$sp[c(1)])
  
  b4 <- gam(as.formula(paste(yvar, "~", 1)),data = data, method = "REML")
  
  # Calculating deviance normalized by deviance of null model
  dev.1 <- (deviance(b2)-deviance(b1))/deviance(b4)
  dev.2 <- (deviance(b3)-deviance(b1))/deviance(b4)
  
  dev.1[2] <- (deviance(b4)-deviance(b3))/deviance(b4)
  dev.2[2] <- (deviance(b4)-deviance(b2))/deviance(b4)
  
  dev.1_mean <- mean(dev.1)
  dev.2_mean <- mean(dev.2)
  
  # Checking to make sure that these two match
  totaldev_def <- summary(b1)$dev.expl 
  totaldev_calc <- dev.1_mean+dev.2_mean
  if (abs(totaldev_def - totaldev_calc) > 1.0e-12) warning("Total deviance does not match the sum of individual deviance")
  
  # Returning output  
  deviance_array <- c(dev.1_mean, dev.2_mean,totaldev_def, totaldev_calc)
}

#---------------------------------------------------------------
# Generic Function to calculate % deviance explained for 3 variable case:
#---------------------------------------------------------------
# To test that this function works, try running the following case study and you should not warning
# dat <- gamSim(1,n=400,dist="normal",scale=2)
# test_result <- deviance3variables(xvar1 = "x0", xvar2 = "x1", xvar3 = "x2", yvar = "y", data=dat)
#--------------------------------------------------------------
deviance3variables <- function(xvar1, xvar2, xvar3, yvar, data) {
  b1 <- gam(as.formula(paste(yvar, "~", "s(",xvar1,")","+", "s(",xvar2,")","+", "s(",xvar3,")")),data = data, method = "REML")
  b2 <- gam(as.formula(paste(yvar, "~", "s(",xvar2,")","+", "s(",xvar3,")")),data = data, method = "REML", sp = b1$sp[c(2,3)])
  b3 <- gam(as.formula(paste(yvar, "~", "s(",xvar1,")","+", "s(",xvar3,")")),data = data, method = "REML", sp = b1$sp[c(1,3)])
  b4 <- gam(as.formula(paste(yvar, "~", "s(",xvar1,")","+", "s(",xvar2,")")),data = data, method = "REML", sp = b1$sp[c(1,2)])
  
  b5 <- gam(as.formula(paste(yvar, "~","s(",xvar3,")")),data = data, method = "REML", sp = b1$sp[c(3)])
  b6 <- gam(as.formula(paste(yvar, "~","s(",xvar2,")")),data = data, method = "REML", sp = b1$sp[c(2)])
  b7 <- gam(as.formula(paste(yvar, "~","s(",xvar1,")")),data = data, method = "REML", sp = b1$sp[c(1)])
  
  b8 <- gam(as.formula(paste(yvar, "~", 1)),data = data, method = "REML")
  
  # Calculating deviance normalized by deviance of null model
  dev.1 <- (deviance(b2)-deviance(b1))/deviance(b8)
  dev.2 <- (deviance(b3)-deviance(b1))/deviance(b8)
  dev.3 <- (deviance(b4)-deviance(b1))/deviance(b8)
  
  dev.1[2] <- ((deviance(b5)-deviance(b3)) + (deviance(b6) - deviance(b4)))/deviance(b8) * (1/2)
  dev.2[2] <- ((deviance(b5)-deviance(b2)) + (deviance(b7) - deviance(b4)))/deviance(b8) * (1/2)
  dev.3[2] <- ((deviance(b6)-deviance(b2)) + (deviance(b7) - deviance(b3)))/deviance(b8) * (1/2)
  
  dev.1[3] <- (deviance(b8)-deviance(b7))/deviance(b8)
  dev.2[3] <- (deviance(b8)-deviance(b6))/deviance(b8)
  dev.3[3] <- (deviance(b8)-deviance(b5))/deviance(b8)
  
  dev.1_mean <- mean(dev.1)
  dev.2_mean <- mean(dev.2)
  dev.3_mean <- mean(dev.3)
  
  # Checking to make sure that these two match
  totaldev_def <- summary(b1)$dev.expl 
  totaldev_calc <- dev.1_mean+dev.2_mean+dev.3_mean
  if (abs(totaldev_def - totaldev_calc) > 1.0e-12) warning("Total deviance does not match the sum of individual deviance")
  
  # Returning output  
  deviance_array <- c(dev.1_mean, dev.2_mean, dev.3_mean, totaldev_def, totaldev_calc)
}

#--------------------------------------------------------------
# Generic Function to calculate % deviance explained for 4 variable case:
#---------------------------------------------------------------
# To test that this function works, try running the following case study and you should not get warning
# dat <- gamSim(1,n=400,dist="normal",scale=2)
# test_result <- deviance4variables(xvar1 = "x0", xvar2 = "x1", xvar3 = "x2", xvar4 = "x3", yvar = "y", data=dat)
#--------------------------------------------------------------
deviance4variables <- function(xvar1, xvar2, xvar3, xvar4, yvar, data) {
  b1 <- gam(as.formula(paste(yvar, "~", "s(",xvar1,")","+", "s(",xvar2,")","+", "s(",xvar3,")","+",
                             "s(",xvar4,")")),data = data, method = "REML",
            na.action = na.omit)
  b2 <- gam(as.formula(paste(yvar, "~", "s(",xvar2,")","+", "s(",xvar3,")","+", "s(",xvar4,")")),data = data, method
            = "REML", sp = b1$sp[c(2,3,4)],
            na.action = na.omit)
  b3 <- gam(as.formula(paste(yvar, "~", "s(",xvar1,")","+", "s(",xvar3,")","+", "s(",xvar4,")")),data = data, method
            = "REML", sp = b1$sp[c(1,3,4)],
            na.action = na.omit)
  b4 <- gam(as.formula(paste(yvar, "~", "s(",xvar1,")","+", "s(",xvar2,")","+", "s(",xvar4,")")),data = data, method
            = "REML", sp = b1$sp[c(1,2,4)],
            na.action = na.omit)
  b5 <- gam(as.formula(paste(yvar, "~", "s(",xvar1,")","+", "s(",xvar2,")","+", "s(",xvar3,")")),data = data, method
            = "REML", sp = b1$sp[c(1,2,3)],
            na.action = na.omit)
  
  b6 <- gam(as.formula(paste(yvar, "~", "s(",xvar3,")","+", "s(",xvar4,")")),data = data, method = "REML", sp
            = b1$sp[c(3,4)],
            na.action = na.omit)
  b7 <- gam(as.formula(paste(yvar, "~", "s(",xvar2,")","+", "s(",xvar4,")")),data = data, method = "REML", sp
            = b1$sp[c(2,4)],
            na.action = na.omit)
  b8 <- gam(as.formula(paste(yvar, "~", "s(",xvar2,")","+", "s(",xvar3,")")),data = data, method = "REML", sp
            = b1$sp[c(2,3)],
            na.action = na.omit)
  b9 <- gam(as.formula(paste(yvar, "~", "s(",xvar1,")","+", "s(",xvar4,")")),data = data, method = "REML", sp
            = b1$sp[c(1,4)],
            na.action = na.omit)
  b10 <- gam(as.formula(paste(yvar, "~", "s(",xvar1,")","+", "s(",xvar3,")")),data = data, method = "REML",
             sp = b1$sp[c(1,3)],
             na.action = na.omit)
  b11 <- gam(as.formula(paste(yvar, "~", "s(",xvar1,")","+", "s(",xvar2,")")),data = data, method = "REML",sp
             = b1$sp[c(1,2)],
             na.action = na.omit)
  
  b12 <- gam(as.formula(paste(yvar, "~","s(",xvar4,")")),data = data, method = "REML", sp = b1$sp[c(4)],
             na.action = na.omit)
  b13 <- gam(as.formula(paste(yvar, "~","s(",xvar3,")")),data = data, method = "REML", sp = b1$sp[c(3)],
             na.action = na.omit)
  b14 <- gam(as.formula(paste(yvar, "~","s(",xvar2,")")),data = data, method = "REML", sp = b1$sp[c(2)],
             na.action = na.omit)
  b15 <- gam(as.formula(paste(yvar, "~","s(",xvar1,")")),data = data, method = "REML", sp = b1$sp[c(1)],
             na.action = na.omit)
  
  b16 <- gam(as.formula(paste(yvar, "~", 1)),data = data, method = "REML",
  )
  
  # Calculating deviance normalized by deviance of null model
  dev.1 <- (deviance(b2)-deviance(b1))/deviance(b16)
  dev.2 <- (deviance(b3)-deviance(b1))/deviance(b16)
  dev.3 <- (deviance(b4)-deviance(b1))/deviance(b16)
  dev.4 <- (deviance(b5)-deviance(b1))/deviance(b16)
  
  dev.1[2] <- ((deviance(b6)-deviance(b3)) + (deviance(b7) - deviance(b4)) + (deviance(b8) - deviance(b5)))/
    deviance(b16) * (1/3)
  dev.2[2] <- ((deviance(b6)-deviance(b2)) + (deviance(b9) - deviance(b4)) + (deviance(b10) - deviance(b5)))/
    deviance(b16) * (1/3)
  dev.3[2] <- ((deviance(b7)-deviance(b2)) + (deviance(b9) - deviance(b3)) + (deviance(b11) - deviance(b5)))/
    deviance(b16) * (1/3)
  dev.4[2] <- ((deviance(b8)-deviance(b2)) + (deviance(b10) - deviance(b3)) + (deviance(b11) -deviance(b4)))/
    deviance(b16) * (1/3)
  
  dev.1[3] <- ((deviance(b12)-deviance(b9)) + (deviance(b13) - deviance(b10)) + (deviance(b14) - deviance(b11)))/
    deviance(b16) * (1/3)
  dev.2[3] <- ((deviance(b12)-deviance(b7)) + (deviance(b13) - deviance(b8)) + (deviance(b15) - deviance(b11)))/
    deviance(b16) * (1/3)
  dev.3[3] <- ((deviance(b12)-deviance(b6)) + (deviance(b14) - deviance(b8)) + (deviance(b15) - deviance(b10)))/
    deviance(b16) * (1/3)
  dev.4[3] <- ((deviance(b13)-deviance(b6)) + (deviance(b14) - deviance(b7)) + (deviance(b15) - deviance(b9)))/
    deviance(b16) * (1/3)
  
  dev.1[4] <- (deviance(b16)-deviance(b15))/deviance(b16)
  dev.2[4] <- (deviance(b16)-deviance(b14))/deviance(b16)
  dev.3[4] <- (deviance(b16)-deviance(b13))/deviance(b16)
  dev.4[4] <- (deviance(b16)-deviance(b12))/deviance(b16)
  
  dev.1_mean <- mean(dev.1)
  dev.2_mean <- mean(dev.2)
  dev.3_mean <- mean(dev.3)
  dev.4_mean <- mean(dev.4)
  
  # Checking to make sure that these two match
  totaldev_def <- summary(b1)$dev.expl 
  totaldev_calc <- dev.1_mean+dev.2_mean+dev.3_mean+dev.4_mean
  if (abs(totaldev_def - totaldev_calc) > 1.0e-12) warning("Total deviance does not match the sum of individual deviance")
  
  # Returning output  
  deviance_array <- c(dev.1_mean, dev.2_mean, dev.3_mean, dev.4_mean, totaldev_def,totaldev_calc)
}

#--------------------------------------------------------------
# Function to calculate % deviance explained for 4 variable case:
# 4th variable is a linear categorical variable (e.g., Nutrient limitation type)
#---------------------------------------------------------------

deviance4variables_nutlim <- function(xvar1, xvar2, xvar3, xvar4, yvar, data) {
  b1 <- gam(as.formula(paste(yvar, "~", "s(",xvar1,")","+", "s(",xvar2,")","+", "s(",xvar3,")","+",
                             "",xvar4,"")),data = data, method = "REML",
            na.action = na.omit)
  b2 <- gam(as.formula(paste(yvar, "~", "s(",xvar2,")","+", "s(",xvar3,")","+", "",xvar4,"")),data = data, method
            = "REML", sp = b1$sp[c(2,3,4)],
            na.action = na.omit)
  b3 <- gam(as.formula(paste(yvar, "~", "s(",xvar1,")","+", "s(",xvar3,")","+", "",xvar4,"")),data = data, method
            = "REML", sp = b1$sp[c(1,3,4)],
            na.action = na.omit)
  b4 <- gam(as.formula(paste(yvar, "~", "s(",xvar1,")","+", "s(",xvar2,")","+", "",xvar4,"")),data = data, method
            = "REML", sp = b1$sp[c(1,2,4)],
            na.action = na.omit)
  b5 <- gam(as.formula(paste(yvar, "~", "s(",xvar1,")","+", "s(",xvar2,")","+", "s(",xvar3,")")),data = data, method
            = "REML", sp = b1$sp[c(1,2,3)],
            na.action = na.omit)
  
  b6 <- gam(as.formula(paste(yvar, "~", "s(",xvar3,")","+", "",xvar4,"")),data = data, method = "REML", sp
            = b1$sp[c(3,4)],
            na.action = na.omit)
  b7 <- gam(as.formula(paste(yvar, "~", "s(",xvar2,")","+", "",xvar4,"")),data = data, method = "REML", sp
            = b1$sp[c(2,4)],
            na.action = na.omit)
  b8 <- gam(as.formula(paste(yvar, "~", "s(",xvar2,")","+", "s(",xvar3,")")),data = data, method = "REML", sp
            = b1$sp[c(2,3)],
            na.action = na.omit)
  b9 <- gam(as.formula(paste(yvar, "~", "s(",xvar1,")","+", "",xvar4,"")),data = data, method = "REML", sp
            = b1$sp[c(1,4)],
            na.action = na.omit)
  b10 <- gam(as.formula(paste(yvar, "~", "s(",xvar1,")","+", "s(",xvar3,")")),data = data, method = "REML",
             sp = b1$sp[c(1,3)],
             na.action = na.omit)
  b11 <- gam(as.formula(paste(yvar, "~", "s(",xvar1,")","+", "s(",xvar2,")")),data = data, method = "REML",sp
             = b1$sp[c(1,2)],
             na.action = na.omit)
  
  b12 <- gam(as.formula(paste(yvar, "~","",xvar4,"")),data = data, method = "REML", sp = b1$sp[c(4)],
             na.action = na.omit)
  b13 <- gam(as.formula(paste(yvar, "~","s(",xvar3,")")),data = data, method = "REML", sp = b1$sp[c(3)],
             na.action = na.omit)
  b14 <- gam(as.formula(paste(yvar, "~","s(",xvar2,")")),data = data, method = "REML", sp = b1$sp[c(2)],
             na.action = na.omit)
  b15 <- gam(as.formula(paste(yvar, "~","s(",xvar1,")")),data = data, method = "REML", sp = b1$sp[c(1)],
             na.action = na.omit)
  
  b16 <- gam(as.formula(paste(yvar, "~", 1)),data = data, method = "REML",
  )
  
  # Calculating deviance normalized by deviance of null model
  dev.1 <- (deviance(b2)-deviance(b1))/deviance(b16)
  dev.2 <- (deviance(b3)-deviance(b1))/deviance(b16)
  dev.3 <- (deviance(b4)-deviance(b1))/deviance(b16)
  dev.4 <- (deviance(b5)-deviance(b1))/deviance(b16)
  
  dev.1[2] <- ((deviance(b6)-deviance(b3)) + (deviance(b7) - deviance(b4)) + (deviance(b8) - deviance(b5)))/
    deviance(b16) * (1/3)
  dev.2[2] <- ((deviance(b6)-deviance(b2)) + (deviance(b9) - deviance(b4)) + (deviance(b10) - deviance(b5)))/
    deviance(b16) * (1/3)
  dev.3[2] <- ((deviance(b7)-deviance(b2)) + (deviance(b9) - deviance(b3)) + (deviance(b11) - deviance(b5)))/
    deviance(b16) * (1/3)
  dev.4[2] <- ((deviance(b8)-deviance(b2)) + (deviance(b10) - deviance(b3)) + (deviance(b11) -deviance(b4)))/
    deviance(b16) * (1/3)
  
  dev.1[3] <- ((deviance(b12)-deviance(b9)) + (deviance(b13) - deviance(b10)) + (deviance(b14) - deviance(b11)))/
    deviance(b16) * (1/3)
  dev.2[3] <- ((deviance(b12)-deviance(b7)) + (deviance(b13) - deviance(b8)) + (deviance(b15) - deviance(b11)))/
    deviance(b16) * (1/3)
  dev.3[3] <- ((deviance(b12)-deviance(b6)) + (deviance(b14) - deviance(b8)) + (deviance(b15) - deviance(b10)))/
    deviance(b16) * (1/3)
  dev.4[3] <- ((deviance(b13)-deviance(b6)) + (deviance(b14) - deviance(b7)) + (deviance(b15) - deviance(b9)))/
    deviance(b16) * (1/3)
  
  dev.1[4] <- (deviance(b16)-deviance(b15))/deviance(b16)
  dev.2[4] <- (deviance(b16)-deviance(b14))/deviance(b16)
  dev.3[4] <- (deviance(b16)-deviance(b13))/deviance(b16)
  dev.4[4] <- (deviance(b16)-deviance(b12))/deviance(b16)
  
  dev.1_mean <- mean(dev.1)
  dev.2_mean <- mean(dev.2)
  dev.3_mean <- mean(dev.3)
  dev.4_mean <- mean(dev.4)
  
  # Checking to make sure that these two match
  totaldev_def <- summary(b1)$dev.expl 
  totaldev_calc <- dev.1_mean+dev.2_mean+dev.3_mean+dev.4_mean
  if (abs(totaldev_def - totaldev_calc) > 1.0e-12) warning("Total deviance does not match the sum of individual deviance")
  
  # Returning output  
  deviance_array <- c(dev.1_mean, dev.2_mean, dev.3_mean, dev.4_mean, totaldev_def,totaldev_calc)
}

#---------------------------------------------------------------
# Function to calculate % deviance explained for 4 variable case:
# This is specifically for model GI and C:P with SST, NO3, Nutricline and nutrient limitation
# For hierarchical GAM, refer to
# Pedersen, E. J., Miller, D. L., Simpson, G. L., & Ross, N. (2019). Hierarchical generalized additive models in ecology: an introduction with mgcv. PeerJ, 7, e6876.
# Treat nutrient limitation term as s(Nutcline_1uM_interp, by = Nutlim, bs = "tp", m = 1) + s(Nutlim, bs = "re", k = 4)
#---------------------------------------------------------------
deviance4variables_nutlim_modGI_CP <- function(data) {
  b1 <- gam(logCP ~ s(SST, bs = "tp", k = 4) + 
              s(logNO3, bs = "tp", k = 4) +                               
              s(Nutcline_1uM_interp, bs = "tp", k = 4, m = 2) +
              s(Nutcline_1uM_interp, by = Nutlim, bs = "tp", m = 1) + 
              s(Nutlim, bs = "re", k = 4),
            data, method = "REML", family = "gaussian", na.action = na.omit)
  b2 <- gam(logCP ~ s(logNO3, bs = "tp", k = 4) +
              s(Nutcline_1uM_interp, bs = "tp", k = 4, m = 2) +
              s(Nutcline_1uM_interp, by = Nutlim, bs = "tp", m = 1) +  s(Nutlim, bs = "re", k = 4),
            data = data, method = "REML", family = "gaussian", na.action = na.omit, sp = b1$sp[c(2,3,4,5,6,7,8)])
  b3 <- gam(logCP ~ s(SST, bs = "tp", k = 4) + 
              s(Nutcline_1uM_interp, bs = "tp", k = 4, m = 2) +
              s(Nutcline_1uM_interp, by = Nutlim, bs = "tp", m = 1) +  s(Nutlim, bs = "re", k = 4),
            data = data, method = "REML", family = "gaussian", na.action = na.omit, sp = b1$sp[c(1,3,4,5,6,7,8)])
  b4 <- gam(logCP ~ s(SST, bs = "tp", k = 4) + 
              s(logNO3, bs = "tp", k = 4) +
              s(Nutcline_1uM_interp, by = Nutlim, bs = "tp", m = 1) +  s(Nutlim, bs = "re", k = 4),
            data = data, method = "REML", family = "gaussian", na.action = na.omit, sp = b1$sp[c(1,2,4,5,6,7,8)])
  b5 <- gam(logCP ~ s(SST, bs = "tp", k = 4) + 
              s(logNO3, bs = "tp", k = 4) +
              s(Nutcline_1uM_interp, bs = "tp", k = 4, m = 2),
            data = data, method = "REML", family = "gaussian", na.action = na.omit,sp = b1$sp[c(1,2,3)])
  
  b6 <- gam(logCP ~ s(Nutcline_1uM_interp, bs = "tp", k = 4, m = 2) +
              s(Nutcline_1uM_interp, by = Nutlim, bs = "tp", m = 1) +  s(Nutlim, bs = "re", k = 4),
            data = data, method = "REML", family = "gaussian", na.action = na.omit, sp = b1$sp[c(3,4,5,6,7,8)])   
  b7 <- gam(logCP ~ s(logNO3, bs = "tp", k = 4) +
              s(Nutcline_1uM_interp, by = Nutlim, bs = "tp", m = 1) +  s(Nutlim, bs = "re", k = 4),
            data = data, method = "REML", family = "gaussian", na.action = na.omit, sp = b1$sp[c(2,4,5,6,7,8)])  
  b8 <- gam(logCP ~ s(logNO3, bs = "tp", k = 4) +
              s(Nutcline_1uM_interp, bs = "tp", k = 4, m = 2),
            data = data, method = "REML", family = "gaussian", na.action = na.omit, sp = b1$sp[c(2,3)])
  b9 <- gam(logCP ~ s(SST, bs = "tp", k = 4) + 
              s(Nutcline_1uM_interp, by = Nutlim, bs = "tp", m = 1) +  s(Nutlim, bs = "re", k = 4),
            data = data, method = "REML", family = "gaussian", na.action = na.omit, sp = b1$sp[c(1,4,5,6,7,8)])  
  b10 <- gam(logCP ~ s(SST, bs = "tp", k = 4) + 
               s(Nutcline_1uM_interp, bs = "tp", k = 4, m = 2),
             data = data, method = "REML", family = "gaussian", na.action = na.omit, sp = b1$sp[c(1,3)])  
  b11 <- gam(logCP ~ s(SST, bs = "tp", k = 4) + 
               s(logNO3, bs = "tp", k = 4),
             data = data, method = "REML", family = "gaussian", na.action = na.omit, sp = b1$sp[c(1,2)])  
  
  b12 <- gam(logCP ~ s(Nutcline_1uM_interp, by = Nutlim, bs = "tp", m = 1) +  s(Nutlim, bs = "re", k = 4),
             data = data, method = "REML", family = "gaussian", na.action = na.omit, sp = b1$sp[c(4,5,6,7,8)])   
  b13 <- gam(logCP ~ s(Nutcline_1uM_interp, bs = "tp", k = 4, m = 2), 
             data = data, method = "REML", family = "gaussian", na.action = na.omit, sp = b1$sp[c(3)])     
  b14 <- gam(logCP ~ s(logNO3, bs = "tp", k = 4),
             data = data, method = "REML", family = "gaussian", na.action = na.omit, sp = b1$sp[c(2)])    
  b15 <- gam(logCP ~ s(SST, bs = "tp", k = 4),
             data = data, method = "REML", family = "gaussian", na.action = na.omit, sp = b1$sp[c(1)])   
  
  b16 <- gam(logCP ~ 1,data = data, method = "REML")
  
  # Calculating deviance normalized by deviance of null model
  dev.1 <- (deviance(b2)-deviance(b1))/deviance(b16)
  dev.2 <- (deviance(b3)-deviance(b1))/deviance(b16)
  dev.3 <- (deviance(b4)-deviance(b1))/deviance(b16)
  dev.4 <- (deviance(b5)-deviance(b1))/deviance(b16)
  
  dev.1[2] <- ((deviance(b6)-deviance(b3)) + (deviance(b7) - deviance(b4)) + (deviance(b8) - deviance(b5)))/
    deviance(b16) * (1/3)
  dev.2[2] <- ((deviance(b6)-deviance(b2)) + (deviance(b9) - deviance(b4)) + (deviance(b10) - deviance(b5)))/
    deviance(b16) * (1/3)
  dev.3[2] <- ((deviance(b7)-deviance(b2)) + (deviance(b9) - deviance(b3)) + (deviance(b11) - deviance(b5)))/
    deviance(b16) * (1/3)
  dev.4[2] <- ((deviance(b8)-deviance(b2)) + (deviance(b10) - deviance(b3)) + (deviance(b11) -deviance(b4)))/
    deviance(b16) * (1/3)
  
  dev.1[3] <- ((deviance(b12)-deviance(b9)) + (deviance(b13) - deviance(b10)) + (deviance(b14) - deviance(b11)))/
    deviance(b16) * (1/3)
  dev.2[3] <- ((deviance(b12)-deviance(b7)) + (deviance(b13) - deviance(b8)) + (deviance(b15) - deviance(b11)))/
    deviance(b16) * (1/3)
  dev.3[3] <- ((deviance(b12)-deviance(b6)) + (deviance(b14) - deviance(b8)) + (deviance(b15) - deviance(b10)))/
    deviance(b16) * (1/3)
  dev.4[3] <- ((deviance(b13)-deviance(b6)) + (deviance(b14) - deviance(b7)) + (deviance(b15) - deviance(b9)))/
    deviance(b16) * (1/3)
  
  dev.1[4] <- (deviance(b16)-deviance(b15))/deviance(b16)
  dev.2[4] <- (deviance(b16)-deviance(b14))/deviance(b16)
  dev.3[4] <- (deviance(b16)-deviance(b13))/deviance(b16)
  dev.4[4] <- (deviance(b16)-deviance(b12))/deviance(b16)
  
  dev.1_mean <- mean(dev.1)
  dev.2_mean <- mean(dev.2)
  dev.3_mean <- mean(dev.3)
  dev.4_mean <- mean(dev.4)
  
  # Checking to make sure that these two match
  totaldev_def <- summary(b1)$dev.expl 
  totaldev_calc <- dev.1_mean+dev.2_mean+dev.3_mean+dev.4_mean
  if (abs(totaldev_def - totaldev_calc) > 1.0e-12) warning("Total deviance does not match the sum of individual deviance")
  
  # Returning output  
  deviance_array <- c(dev.1_mean, dev.2_mean, dev.3_mean, dev.4_mean, totaldev_def,totaldev_calc)
}

#---------------------------------------------------------------
# Function to calculate % deviance explained for 4 variable case:
# This is specifically for model GI and N:P with SST, NO3, Nutricline and nutrient limitation
# For hierarchical GAM, refer to
# Pedersen, E. J., Miller, D. L., Simpson, G. L., & Ross, N. (2019). Hierarchical generalized additive models in ecology: an introduction with mgcv. PeerJ, 7, e6876.
# Treat nutrient limitation term as s(Nutcline_1uM_interp, by = Nutlim, bs = "tp", m = 1) + s(Nutlim, bs = "re", k = 4)
#---------------------------------------------------------------
deviance4variables_nutlim_modGI_NP <- function(data) {
  b1 <- gam(logNP ~ s(SST, bs = "tp", k = 4) + 
              s(logNO3, bs = "tp", k = 4) +                               
              s(Nutcline_1uM_interp, bs = "tp", k = 4, m = 2) +
              s(Nutcline_1uM_interp, by = Nutlim, bs = "tp", m = 1) + 
              s(Nutlim, bs = "re", k = 4),
            data, method = "REML", family = "gaussian", na.action = na.omit)
  b2 <- gam(logNP ~ s(logNO3, bs = "tp", k = 4) +
              s(Nutcline_1uM_interp, bs = "tp", k = 4, m = 2) +
              s(Nutcline_1uM_interp, by = Nutlim, bs = "tp", m = 1) +  s(Nutlim, bs = "re", k = 4),
            data = data, method = "REML", family = "gaussian", na.action = na.omit, sp = b1$sp[c(2,3,4,5,6,7,8)])
  b3 <- gam(logNP ~ s(SST, bs = "tp", k = 4) + 
              s(Nutcline_1uM_interp, bs = "tp", k = 4, m = 2) +
              s(Nutcline_1uM_interp, by = Nutlim, bs = "tp", m = 1) +  s(Nutlim, bs = "re", k = 4),
            data = data, method = "REML", family = "gaussian", na.action = na.omit, sp = b1$sp[c(1,3,4,5,6,7,8)])
  b4 <- gam(logNP ~ s(SST, bs = "tp", k = 4) + 
              s(logNO3, bs = "tp", k = 4) +
              s(Nutcline_1uM_interp, by = Nutlim, bs = "tp", m = 1) +  s(Nutlim, bs = "re", k = 4),
            data = data, method = "REML", family = "gaussian", na.action = na.omit, sp = b1$sp[c(1,2,4,5,6,7,8)])
  b5 <- gam(logNP ~ s(SST, bs = "tp", k = 4) + 
              s(logNO3, bs = "tp", k = 4) +
              s(Nutcline_1uM_interp, bs = "tp", k = 4, m = 2),
            data = data, method = "REML", family = "gaussian", na.action = na.omit,sp = b1$sp[c(1,2,3)])
  
  b6 <- gam(logNP ~ s(Nutcline_1uM_interp, bs = "tp", k = 4, m = 2) +
              s(Nutcline_1uM_interp, by = Nutlim, bs = "tp", m = 1) +  s(Nutlim, bs = "re", k = 4),
            data = data, method = "REML", family = "gaussian", na.action = na.omit, sp = b1$sp[c(3,4,5,6,7,8)])   
  b7 <- gam(logNP ~ s(logNO3, bs = "tp", k = 4) +
              s(Nutcline_1uM_interp, by = Nutlim, bs = "tp", m = 1) +  s(Nutlim, bs = "re", k = 4),
            data = data, method = "REML", family = "gaussian", na.action = na.omit, sp = b1$sp[c(2,4,5,6,7,8)])  
  b8 <- gam(logNP ~ s(logNO3, bs = "tp", k = 4) +
              s(Nutcline_1uM_interp, bs = "tp", k = 4, m = 2),
            data = data, method = "REML", family = "gaussian", na.action = na.omit, sp = b1$sp[c(2,3)])
  b9 <- gam(logNP ~ s(SST, bs = "tp", k = 4) + 
              s(Nutcline_1uM_interp, by = Nutlim, bs = "tp", m = 1) +  s(Nutlim, bs = "re", k = 4),
            data = data, method = "REML", family = "gaussian", na.action = na.omit, sp = b1$sp[c(1,4,5,6,7,8)])  
  b10 <- gam(logNP ~ s(SST, bs = "tp", k = 4) + 
               s(Nutcline_1uM_interp, bs = "tp", k = 4, m = 2),
             data = data, method = "REML", family = "gaussian", na.action = na.omit, sp = b1$sp[c(1,3)])  
  b11 <- gam(logNP ~ s(SST, bs = "tp", k = 4) + 
               s(logNO3, bs = "tp", k = 4),
             data = data, method = "REML", family = "gaussian", na.action = na.omit, sp = b1$sp[c(1,2)])  
  
  b12 <- gam(logNP ~ s(Nutcline_1uM_interp, by = Nutlim, bs = "tp", m = 1) +  s(Nutlim, bs = "re", k = 4),
             data = data, method = "REML", family = "gaussian", na.action = na.omit, sp = b1$sp[c(4,5,6,7,8)])   
  b13 <- gam(logNP ~ s(Nutcline_1uM_interp, bs = "tp", k = 4, m = 2), 
             data = data, method = "REML", family = "gaussian", na.action = na.omit, sp = b1$sp[c(3)])     
  b14 <- gam(logNP ~ s(logNO3, bs = "tp", k = 4),
             data = data, method = "REML", family = "gaussian", na.action = na.omit, sp = b1$sp[c(2)])    
  b15 <- gam(logNP ~ s(SST, bs = "tp", k = 4),
             data = data, method = "REML", family = "gaussian", na.action = na.omit, sp = b1$sp[c(1)])   
  
  b16 <- gam(logNP ~ 1,data = data, method = "REML")
  
  # Calculating deviance normalized by deviance of null model
  dev.1 <- (deviance(b2)-deviance(b1))/deviance(b16)
  dev.2 <- (deviance(b3)-deviance(b1))/deviance(b16)
  dev.3 <- (deviance(b4)-deviance(b1))/deviance(b16)
  dev.4 <- (deviance(b5)-deviance(b1))/deviance(b16)
  
  dev.1[2] <- ((deviance(b6)-deviance(b3)) + (deviance(b7) - deviance(b4)) + (deviance(b8) - deviance(b5)))/
    deviance(b16) * (1/3)
  dev.2[2] <- ((deviance(b6)-deviance(b2)) + (deviance(b9) - deviance(b4)) + (deviance(b10) - deviance(b5)))/
    deviance(b16) * (1/3)
  dev.3[2] <- ((deviance(b7)-deviance(b2)) + (deviance(b9) - deviance(b3)) + (deviance(b11) - deviance(b5)))/
    deviance(b16) * (1/3)
  dev.4[2] <- ((deviance(b8)-deviance(b2)) + (deviance(b10) - deviance(b3)) + (deviance(b11) -deviance(b4)))/
    deviance(b16) * (1/3)
  
  dev.1[3] <- ((deviance(b12)-deviance(b9)) + (deviance(b13) - deviance(b10)) + (deviance(b14) - deviance(b11)))/
    deviance(b16) * (1/3)
  dev.2[3] <- ((deviance(b12)-deviance(b7)) + (deviance(b13) - deviance(b8)) + (deviance(b15) - deviance(b11)))/
    deviance(b16) * (1/3)
  dev.3[3] <- ((deviance(b12)-deviance(b6)) + (deviance(b14) - deviance(b8)) + (deviance(b15) - deviance(b10)))/
    deviance(b16) * (1/3)
  dev.4[3] <- ((deviance(b13)-deviance(b6)) + (deviance(b14) - deviance(b7)) + (deviance(b15) - deviance(b9)))/
    deviance(b16) * (1/3)
  
  dev.1[4] <- (deviance(b16)-deviance(b15))/deviance(b16)
  dev.2[4] <- (deviance(b16)-deviance(b14))/deviance(b16)
  dev.3[4] <- (deviance(b16)-deviance(b13))/deviance(b16)
  dev.4[4] <- (deviance(b16)-deviance(b12))/deviance(b16)
  
  dev.1_mean <- mean(dev.1)
  dev.2_mean <- mean(dev.2)
  dev.3_mean <- mean(dev.3)
  dev.4_mean <- mean(dev.4)
  
  # Checking to make sure that these two match
  totaldev_def <- summary(b1)$dev.expl 
  totaldev_calc <- dev.1_mean+dev.2_mean+dev.3_mean+dev.4_mean
  if (abs(totaldev_def - totaldev_calc) > 1.0e-12) warning("Total deviance does not match the sum of individual deviance")
  
  # Returning output  
  deviance_array <- c(dev.1_mean, dev.2_mean, dev.3_mean, dev.4_mean, totaldev_def,totaldev_calc)
}

#---------------------------------------------------------------
# Function to calculate % deviance explained for 4 variable case:
# This is specifically for model GI and C:N with SST, NO3, Nutricline and nutrient limitation
# For hierarchical GAM, refer to
# Pedersen, E. J., Miller, D. L., Simpson, G. L., & Ross, N. (2019). Hierarchical generalized additive models in ecology: an introduction with mgcv. PeerJ, 7, e6876.
# Treat nutrient limitation term as s(Nutcline_1uM_interp, by = Nutlim, bs = "tp", m = 1) + s(Nutlim, bs = "re", k = 4)
#---------------------------------------------------------------
deviance4variables_nutlim_modGI_CN <- function(data) {
  b1 <- gam(logCN ~ s(SST, bs = "tp", k = 4) + 
              s(logNO3, bs = "tp", k = 4) +                               
              s(Nutcline_1uM_interp, bs = "tp", k = 4, m = 2) +
              s(Nutcline_1uM_interp, by = Nutlim, bs = "tp", m = 1) + 
              s(Nutlim, bs = "re", k = 4),
            data, method = "REML", family = "gaussian", na.action = na.omit)
  b2 <- gam(logCN ~ s(logNO3, bs = "tp", k = 4) +
              s(Nutcline_1uM_interp, bs = "tp", k = 4, m = 2) +
              s(Nutcline_1uM_interp, by = Nutlim, bs = "tp", m = 1) +  s(Nutlim, bs = "re", k = 4),
            data = data, method = "REML", family = "gaussian", na.action = na.omit, sp = b1$sp[c(2,3,4,5,6,7,8)])
  b3 <- gam(logCN ~ s(SST, bs = "tp", k = 4) + 
              s(Nutcline_1uM_interp, bs = "tp", k = 4, m = 2) +
              s(Nutcline_1uM_interp, by = Nutlim, bs = "tp", m = 1) +  s(Nutlim, bs = "re", k = 4),
            data = data, method = "REML", family = "gaussian", na.action = na.omit, sp = b1$sp[c(1,3,4,5,6,7,8)])
  b4 <- gam(logCN ~ s(SST, bs = "tp", k = 4) + 
              s(logNO3, bs = "tp", k = 4) +
              s(Nutcline_1uM_interp, by = Nutlim, bs = "tp", m = 1) +  s(Nutlim, bs = "re", k = 4),
            data = data, method = "REML", family = "gaussian", na.action = na.omit, sp = b1$sp[c(1,2,4,5,6,7,8)])
  b5 <- gam(logCN ~ s(SST, bs = "tp", k = 4) + 
              s(logNO3, bs = "tp", k = 4) +
              s(Nutcline_1uM_interp, bs = "tp", k = 4, m = 2),
            data = data, method = "REML", family = "gaussian", na.action = na.omit,sp = b1$sp[c(1,2,3)])
  
  b6 <- gam(logCN ~ s(Nutcline_1uM_interp, bs = "tp", k = 4, m = 2) +
              s(Nutcline_1uM_interp, by = Nutlim, bs = "tp", m = 1) +  s(Nutlim, bs = "re", k = 4),
            data = data, method = "REML", family = "gaussian", na.action = na.omit, sp = b1$sp[c(3,4,5,6,7,8)])   
  b7 <- gam(logCN ~ s(logNO3, bs = "tp", k = 4) +
              s(Nutcline_1uM_interp, by = Nutlim, bs = "tp", m = 1) +  s(Nutlim, bs = "re", k = 4),
            data = data, method = "REML", family = "gaussian", na.action = na.omit, sp = b1$sp[c(2,4,5,6,7,8)])  
  b8 <- gam(logCN ~ s(logNO3, bs = "tp", k = 4) +
              s(Nutcline_1uM_interp, bs = "tp", k = 4, m = 2),
            data = data, method = "REML", family = "gaussian", na.action = na.omit, sp = b1$sp[c(2,3)])
  b9 <- gam(logCN ~ s(SST, bs = "tp", k = 4) + 
              s(Nutcline_1uM_interp, by = Nutlim, bs = "tp", m = 1) +  s(Nutlim, bs = "re", k = 4),
            data = data, method = "REML", family = "gaussian", na.action = na.omit, sp = b1$sp[c(1,4,5,6,7,8)])  
  b10 <- gam(logCN ~ s(SST, bs = "tp", k = 4) + 
               s(Nutcline_1uM_interp, bs = "tp", k = 4, m = 2),
             data = data, method = "REML", family = "gaussian", na.action = na.omit, sp = b1$sp[c(1,3)])  
  b11 <- gam(logCN ~ s(SST, bs = "tp", k = 4) + 
               s(logNO3, bs = "tp", k = 4),
             data = data, method = "REML", family = "gaussian", na.action = na.omit, sp = b1$sp[c(1,2)])  
  
  b12 <- gam(logCN ~ s(Nutcline_1uM_interp, by = Nutlim, bs = "tp", m = 1) +  s(Nutlim, bs = "re", k = 4),
             data = data, method = "REML", family = "gaussian", na.action = na.omit, sp = b1$sp[c(4,5,6,7,8)])   
  b13 <- gam(logCN ~ s(Nutcline_1uM_interp, bs = "tp", k = 4, m = 2), 
             data = data, method = "REML", family = "gaussian", na.action = na.omit, sp = b1$sp[c(3)])     
  b14 <- gam(logCN ~ s(logNO3, bs = "tp", k = 4),
             data = data, method = "REML", family = "gaussian", na.action = na.omit, sp = b1$sp[c(2)])    
  b15 <- gam(logCN ~ s(SST, bs = "tp", k = 4),
             data = data, method = "REML", family = "gaussian", na.action = na.omit, sp = b1$sp[c(1)])   
  
  b16 <- gam(logCN ~ 1,data = data, method = "REML")
  
  # Calculating deviance normalized by deviance of null model
  dev.1 <- (deviance(b2)-deviance(b1))/deviance(b16)
  dev.2 <- (deviance(b3)-deviance(b1))/deviance(b16)
  dev.3 <- (deviance(b4)-deviance(b1))/deviance(b16)
  dev.4 <- (deviance(b5)-deviance(b1))/deviance(b16)
  
  dev.1[2] <- ((deviance(b6)-deviance(b3)) + (deviance(b7) - deviance(b4)) + (deviance(b8) - deviance(b5)))/
    deviance(b16) * (1/3)
  dev.2[2] <- ((deviance(b6)-deviance(b2)) + (deviance(b9) - deviance(b4)) + (deviance(b10) - deviance(b5)))/
    deviance(b16) * (1/3)
  dev.3[2] <- ((deviance(b7)-deviance(b2)) + (deviance(b9) - deviance(b3)) + (deviance(b11) - deviance(b5)))/
    deviance(b16) * (1/3)
  dev.4[2] <- ((deviance(b8)-deviance(b2)) + (deviance(b10) - deviance(b3)) + (deviance(b11) -deviance(b4)))/
    deviance(b16) * (1/3)
  
  dev.1[3] <- ((deviance(b12)-deviance(b9)) + (deviance(b13) - deviance(b10)) + (deviance(b14) - deviance(b11)))/
    deviance(b16) * (1/3)
  dev.2[3] <- ((deviance(b12)-deviance(b7)) + (deviance(b13) - deviance(b8)) + (deviance(b15) - deviance(b11)))/
    deviance(b16) * (1/3)
  dev.3[3] <- ((deviance(b12)-deviance(b6)) + (deviance(b14) - deviance(b8)) + (deviance(b15) - deviance(b10)))/
    deviance(b16) * (1/3)
  dev.4[3] <- ((deviance(b13)-deviance(b6)) + (deviance(b14) - deviance(b7)) + (deviance(b15) - deviance(b9)))/
    deviance(b16) * (1/3)
  
  dev.1[4] <- (deviance(b16)-deviance(b15))/deviance(b16)
  dev.2[4] <- (deviance(b16)-deviance(b14))/deviance(b16)
  dev.3[4] <- (deviance(b16)-deviance(b13))/deviance(b16)
  dev.4[4] <- (deviance(b16)-deviance(b12))/deviance(b16)
  
  dev.1_mean <- mean(dev.1)
  dev.2_mean <- mean(dev.2)
  dev.3_mean <- mean(dev.3)
  dev.4_mean <- mean(dev.4)
  
  # Checking to make sure that these two match
  totaldev_def <- summary(b1)$dev.expl 
  totaldev_calc <- dev.1_mean+dev.2_mean+dev.3_mean+dev.4_mean
  if (abs(totaldev_def - totaldev_calc) > 1.0e-12) warning("Total deviance does not match the sum of individual deviance")
  
  # Returning output  
  deviance_array <- c(dev.1_mean, dev.2_mean, dev.3_mean, dev.4_mean, totaldev_def,totaldev_calc)
}
