################################################################
# We will first try to use functional regression
# to get back the treatment effect.

################################################################
# setup
      library(refund)
      library(ggplot2)
# load data
      load(file="0_Data_Simulation/Simulation1.Rdata")

# model matrix of predictors
      X = cbind(1,A)
# subsample
      i = sample(1:nrow(Y), 30)
      Y = Y[i,]
      X = X[i,]
# t
      t = 1:1440
      
################################################################
# Function on Scalar Regression (fosr)
      fit3 = fosr(Y = Y, X = X, 
                  argvals = t,
                  method = "OLS", 
                  lambda = 3,  # smoothing parameter
                  nbasis = 24, # 24 basis functions
                  norder = 4,  # cubic splines
                  cov.method = "naive")
      # grab coefficient functions from model fit
            beta0 = fit3$est.func[,"reps 1"]
            beta1 = fit3$est.func[,"reps 2"]
            
            plot(average.a0~t, type="l")
            lines(beta0~t, type="l", col="red")          
            legend(x=0, y=30, box.lwd = 0, fill = c("red","black"), legend = c("Fitted B0", "True B0"))
            
            plot(average.effect~t, type="l")
            lines(beta1~t, type="l", col="blue")
            legend(x=700, y=-15, box.lwd = 0, fill = c("blue","black"), legend = c("Fitted Effect", "True Effect"))
      
      
      lines(average.effect ~ t, col="red")
      fit4 = fosr(Y = Y, X = X, 
                  argvals = 1:1440,
                  method = "OLS", 
                  lambda = 4, 
                  nbasis = 12, #24 basis functions
                  norder = 4) #cubic splines
      plot(fit4)
      lines(average.effect ~ t, col="red")
