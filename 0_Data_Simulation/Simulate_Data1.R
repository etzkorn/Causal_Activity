###############################################
# This is the most simple data-generating scenario:
# step counts only depend on the treatment (amputation)

# In this script we generate data for the 
# following scenario:

# We sample N people from a population of 10,000
# and each person has recorded step counts on one-minute
# intervals over the course of a day. Values for amputation
# status are randomly and independently 
# assigned to each person with probability 0.5.
# We observe only step counts for the assigned “treatment”.
# Step counts are not dependent on any other unobserved 
# factors. 

###############################################
# Set Parameters

# number of subjects (<10000)
N = 30

###############################################
# Population Generating Curves 
# (Not expectations, just a place to start)

# population under A = 1
t = 1:1440
population.a1 = 7 * (sin(t*pi/1440)^9) * (sin(t*pi/1440*8)+1) + 3*sin(t*pi/1440)^3 + 0.1
      # plot(population.a1~t, type="l", col="blue")

# population under A = 0
population.a0 = 3 * (sin(t*pi/1440)^10) * (sin(t*pi/1440*10+150)+1) + 5*sin(t*pi/1440)^5 + 0.1
      # lines(population.a0~t, type="l", col="red")

legend(x=0, y=20, box.lwd = 0,
       fill = c("red","blue"), 
       legend = c("A = 0", "A = 1"))

# population proportion between curves
pop.prop = population.a1 / (population.a0 +  population.a1)
      # plot(pop.prop~t, type="l")

###############################################
# Individual E(Steps(i)|A)

# simulate curves
generate.curve = function(population.a1){
      t = 1:1440
      z = sample(1:180,1)
      population.a1 = c(population.a1[z:length(population.a1)], population.a1[1:z-1])
      a = abs(rnorm(1,10,1))
      b = rnorm(1, 0, 500)
      c = abs(rnorm(1,0,1))
      ((sin((t+b)*pi/1440*a)/(1+c))^2 + 1) * population.a1
}

# expected steps under amputation
individual.a1 = replicate(10000, generate.curve(population.a1))

# expected steps under salvage (everone has the same treatment proportion)
individual.a0 = individual.a1 / pop.prop - individual.a1

      # par(mfrow=c(1,2)) 
      # plot.curves(individual.a1[,1:10], "Individual Curves (A = 1)")
      # plot.curves(individual.a0[,1:10], "Individual Curves (A = 0)")

###############################################
# Simulate Steps
generate.steps = function(individual.a1){
      n = nrow(individual.a1)
      m = ncol(individual.a1)
      matrix(rpois(n * m, individual.a1),n,m)
}
steps.a1 = generate.steps(individual.a1)
steps.a0 = generate.steps(individual.a0)
      # plot.curves(steps.a0[,1:10])
      # plot.curves(steps.a1[,1:10])

###############################################      
# Get correct answers from entire population
individual.effects = individual.a1-individual.a0
average.effect = rowMeans(individual.effects)
      # plot(average.effect, type="l")
      # plot.curves(individual.effects)

###############################################
# Save Data
random.samp = sample(1:10000, N)
a = sample(0:1, N, replace=T)==1

A1 = t(steps.a1[,random.samp][,a])
A0 = t(steps.a0[,random.samp][,!a])

rm(list = setdiff(ls(), c("A1", "A0", "average.effect")))
save.image(file="0_Data_Simulation/Simulation1.Rdata")
