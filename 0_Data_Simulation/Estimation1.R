################################################################
# We will first try to use functional regression
# to get back the treatment effect.

################################################################
# setup
      library(refund)
      library(ggplot2)
# load data
      load(file="0_Data_Simulation/Simulation1.Rdata")

# matrix of outcomes
      Y = rbind(A1, A0)
# model matrix of predictors
      A = c(rep(1,nrow(A1)), rep(0,nrow(A0)))
      X = cbind(1,A)
# subsample
      i = sample(1:nrow(Y), 30)
      Y = Y[i,]
      X = X[i,]
      
################################################################
# Function on Scalar Regression (fosr)
      fit3 = fosr(Y = Y, X = X, 
                  argvals = 1:1440,
                  method = "OLS", 
                  lambda = 3, 
                  nbasis = 24, #24 basis functions
                  norder = 4) #cubic splines
      plot(fit3)
      lines(average.effect ~ t)
      
      t = 1:1440
      f = Y[100,]
      plot(f~t)
      ggplot() + geom_point(aes(x=t, y=f)) + stat_smooth(aes(x=t,y=f))
      
################################################################
# Function on Scalar Regression 2-Step (fosr2s)

################################################################
# Penalized Flexible Functional Regression (pffr)