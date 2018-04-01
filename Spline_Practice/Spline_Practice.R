require(dplyr)

# create data
      t = 1:1440
      y = sin(t/1440*2*pi) + rnorm(1440) + 5
      plot(y~t)

# create knots and design matrix
      knots = 120*(0:11)
      knot.t = function(t, k) pmax(t-k,0)^2
      Tx = lapply(knots, knot.t, t=t) %>% do.call(what="cbind")
      Tx = cbind(1, t, Tx)

# fit model
      yhat = Tx %*% solve(t(Tx) %*% Tx) %*% t(Tx) %*% y
      x =t(Tx) %*% Tx
      lines(yhat~t, col="red", lwd=3)
      
