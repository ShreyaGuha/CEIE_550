#specify energy change without human interference
Energy0 <- 0

#time steps, number of steps and time vector
dt <- 1
n.steps <- seq(50)
t <- seq(1, 50, dt)

#human caused forcings and their normal distributions
CO2_mean <- 1.75
CO2_sd <- 0.2
CH4_mean <- 1.0
CH4_sd <- 0.1
Aer_mean <- -0.3
Aer_sd <- 0.5
clouds_mean <- -0.5
clouds_sd <- 0.5
alb_mean <- -0.2
alb_sd <- 0.1

#sample flow of forcings
CO2_pdf <- dnorm(n.steps+1, mean = CO2_mean, sd = CO2_sd)
CH4_pdf <- dnorm(n.steps+1, mean = CH4_mean, sd = CH4_sd)
Aer_pdf <- dnorm(n.steps+1, mean = Aer_mean, sd = Aer_sd)
clouds_pdf <- dnorm(n.steps+1, mean = clouds_mean, sd = clouds_sd)
alb_pdf <- dnorm(n.steps+1, mean = alb_mean, sd = alb_sd)

#initialize energy vector
Energy <- c()
Energy[1] <- Energy0

#calculate energy
for (t in 1:50){
  Energy[t+1] <- Energy[t] + ((CO2_pdf[t+1] + CH4_pdf[t+1] + Aer_pdf[t+1] + clouds_pdf[t+1] + alb_pdf[t+1])*dt)
}

#plots
plot(x=time, y=Energy, 
     type = 'l', main = 'Monte Carlo Simulation',
     ylab = 'Change in energy (W/m2)',
     xlab = 'time, year')
plot(x=time, y=Energy, type = 'l', 
     main = 'Monte Carlo Simulation',
     ylab = 'Energy Change (W/m2)', xlab = 'time, days')

plot(Energy, type = 'l', 
     main = 'Monte Carlo Simulation',
     ylab = 'Energy Change (W/m2)', xlab = 'time, years')

