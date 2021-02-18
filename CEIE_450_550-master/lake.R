#Capacity of lake
X <- 1*10^15
V <- 0.8*X
time <- seq(1,100,1)

l <- vector("list", 100)
m <- vector("list", 100)
n <- vector("list", 100)

for(i in 1:100){
  if(V <= 0.9*X) Qo <- 1.1*86400*10^9 else Qo <- 1.5*86400*10^9
  if(i%%7 == 0){
    Qi <- 1.54*86400*10^9
    V <- V + (Qi - Qo)
    l[[i]] <- V
    m[[i]] <- Qo
    n[[i]] <- Qi
  }else{
    Qi <- 1.1*86400*10^9
    V <- V + (Qi - Qo)
    l[[i]] <- V
    m[[i]] <- Qo
    n[[i]] <- Qi
    }
}

print(l)


plot(time, l, type = 'l', # 'l' here means line plot
                  main = 'Lake',
                  ylab = 'Volume, cubic feet',
                  xlab = 'time, days')

