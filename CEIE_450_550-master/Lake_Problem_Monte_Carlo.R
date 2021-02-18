##lake problem##

#Define some initial values
#n.steps is number of time steps after t = 0

#volume at time zero
Vmax <- 1e6
V0 <- Vmax * 0.80

#time steps, number of steps and time vector
dt <- 1
n.steps <- 100000
t <- seq(from = 0, by = dt, length.out = n.steps+1)

#convert cfs to cfd
cfs_to_cfd <- (60*60*24)

#define some flow values
#note: length(Qi) == length(Qo) == n.steps+1

#log normal distribution
Q1_mean_log <- 15 
Q1_sd_log <- 1.7 
Q2_mean_log <- 10 
Q2_sd_log <- 2.2 

#sample flow values for R1 and R2
R1_samples <- ((rlnorm(n.steps+1, meanlog = Q1_mean_log, sdlog = Q1_sd_log))/(10^9))*cfs_to_cfd
R2_samples <- ((rlnorm(n.steps+1, meanlog = Q2_mean_log, sdlog = Q2_sd_log))/(10^9))*cfs_to_cfd

#calculate the volume in the lake across time
#initialize volume vector
V <- c()
V[1] <- V0

#initialize Qout vector
Qo <- (rep (1e8, n.steps+1)/10^9)*cfs_to_cfd

#check out the histogram of the flows
hist(log(R1_samples))
hist(log(R2_samples))

#calculate volumes
for (t in 1:n.steps){
  #V[t+1] <- V[t] + (R1_samples[t+1] + R2_samples[t+1] - Qo[t+1])*dt
  Qo[t+1] <- ifelse (V[t] > Vmax*0.90, 5.0*Qo[t+1], Qo[t+1])
  Qo[t+1] <- ifelse (V[t] < Vmax*0.75, 0.1*Qo[t+1], Qo[t+1])
  V[t+1] <- V[t] + (R1_samples[t+1] + R2_samples[t+1] - Qo[t+1])*dt
}

#plots
#plot the flows
par(mfrow = c(1,3))
plot(x=time, y=R1_samples, type = 'l', log = 'y',
     main = 'lake base model', ylab = 'Inflow 1,10^9 cfd', xlab = 'time, days')
plot(x=time, y=R2_samples, type = 'l', log = 'y',
     main = 'lake base model', ylab = 'Inflow 2,10^9 cfd', xlab = 'time, days')
plot(x=time, y=Qo, type = 'l', log = 'y',
     main = 'lake base model', ylab = 'Outflow,10^9 cfd', xlab = 'time, days')
par(mfrow = c(1,1))

#calculate quantiles
quantile(R2_samples, c(0.25, 0.50, 0.975))

#plot volume across time in base model
length(time)
length(V)
plot(x=time, y=V, ylim = c(0, max(V,Vmax)),
     type = 'l', main = 'lake base case',
     ylab = 'volume, 10^9 cubic feet',
     xlab = 'time, day')
abline(h = Vmax, col = 'red')

#calculate probabilities
#probability of overflowing the lake
length(V[V>Vmax])/n.steps*100

#probability of opening the spillway
length(Qo[Qo>1e8*cfs_to_cfd])/n.steps*100

#max.V
max(V)
max(V)/Vmax

#Day when Qo is bigger than usual
open.days <- which(Qo>Qo[1])
length(open.days)
