################################################################
# We will first try to use functional regression
# to get back the treatment effect.

# We will try using various functions from the refund package.

################################################################
# load data
      load(file="0_Data_Simulation/Simulation1.Rdata")
# matrix of outcomes
      Y = rbind(A1, A0)
# model matrix of predictors
      A = c(rep(1,nrow(A1)), rep(0,nrow(A0)))
      X = cbind(1,A)

################################################################
# Function on Scalar Regression (fosr)
fit1 = fosr(Y = Y, X = X, method = "OLS")
################################################################
# Function on Scalar Regression 2-Step (fosr2s)

################################################################
# Penalized Flexible Functional Regression (pffr)