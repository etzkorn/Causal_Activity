# plot curves
plot.curves = function(mat, title="Individual-Specific Curves"){
      t = 1:1440
      plot(mat[,1]~t, type="l", col="black", ylim=c(0,40),
           main = title)
      for(i in 2:ncol(mat)){
            lines(mat[,i]~t, type="l", col=co[i%%5+1])
      }
}

library(RSkittleBrewer)
co = RSkittleBrewer("tropical")
