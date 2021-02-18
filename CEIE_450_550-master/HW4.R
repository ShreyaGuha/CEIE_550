#read and summarize data
aq_data <- read.csv("air_quality_models_hw.csv")
summary(aq_data)

#plot data against observations
par (mfrow = c(1,2), oma = c(0, 0, 2, 0)) #combining plots
plot(x = aq_data$obs, y = aq_data$model1, xlab = "obs", ylab = "model1")
plot(x = aq_data$obs, y = aq_data$model2, xlab = "obs", ylab = "model2")
mtext("PM2.5 concentrations in units of µg m-3", outer = TRUE, cex = 1.2)

#define vectors to match equations
O <- aq_data$obs
P_n1 <- aq_data$model1
P_n2 <- aq_data$model2
N <- nrow(aq_data)

#mean bias
mb_1 <- (1/N)*sum(P_n1-O)
mb_2 <- (1/N)*sum(P_n2-O)

#mean error
me_1 <- (1/N)*sum(abs(P_n1-O))
me_2 <- (1/N)*sum(abs(P_n2-O))

#normalized mean bias
nmb_1 <- (1/sum(O))*sum(P_n1-O)*100
nmb_2 <- (1/sum(O))*sum(P_n2-O)*100

#normalized mean error
nme_1 <- (1/sum(O))*sum(abs(P_n1-O))*100
nme_2 <- (1/sum(O))*sum(abs(P_n2-O))*100

#root mean square error
meanO <- mean(O)
meanP_n1 <- mean(P_n1)
meanP_n2 <- mean(P_n2)

rmse_1 <- sqrt((1/N)*sum(((P_n1-meanP_n1)-(O-meanO))^2))
rmse_2 <- sqrt((1/N)*sum(((P_n2-meanP_n2)-(O-meanO))^2))

#correlation
r_1 <- sum((P_n1-meanP_n1)*(O-meanO))/sqrt(sum((P_n1-meanP_n1)^2)*sum((O-meanO)^2))
r_2 <- sum((P_n2-meanP_n2)*(O-meanO))/sqrt(sum((P_n2-meanP_n2)^2)*sum((O-meanO)^2))

#r squared
rsquared_1 <- r_1^2
rsquared_2 <- r_2^2

#print values
mb_1
mb_2
me_1
me_2
nmb_1
nmb_2
nme_1
nme_2
rmse_1
rmse_2
r_1
r_2
rsquared_1
rsquared_2

#plotting against days
aq_data$days <- seq(1,1037,1) #create days column

par (mar = c(0.5,0.5,0.5,0.5), oma = c(1, 1, 1, 1)) #combining plots
plot(x = aq_data$days, y = aq_data$model1, xlab = "no of days", ylab = "model1", cex.main = "0.5")
plot(x = aq_data$days, y = aq_data$model2, xlab = "no of days", ylab = "model2", cex.main = "0.5")
plot(x = aq_data$days, y = aq_data$obs, xlab = "no of days", ylab = "obs", cex.main = "0.5")
mtext("PM2.5 concentrations in units of µg m-3", outer = TRUE, cex = 0.8)

#filtering data
data.subset <- subset( aq_data, aq_data$obs > 3)

#again define vectors to match equations
O <- data.subset$obs
P_n1 <- data.subset$model1
P_n2 <- data.subset$model2
N <- nrow(data.subset)

#mean bias
mb_1 <- (1/N)*sum(P_n1-O)
mb_2 <- (1/N)*sum(P_n2-O)

#mean error
me_1 <- (1/N)*sum(abs(P_n1-O))
me_2 <- (1/N)*sum(abs(P_n2-O))

#normalized mean bias
nmb_1 <- (1/sum(O))*sum(P_n1-O)*100
nmb_2 <- (1/sum(O))*sum(P_n2-O)*100

#normalized mean error
nme_1 <- (1/sum(O))*sum(abs(P_n1-O))*100
nme_2 <- (1/sum(O))*sum(abs(P_n2-O))*100

#root mean square error
meanO <- mean(O)
meanP_n1 <- mean(P_n1)
meanP_n2 <- mean(P_n2)

rmse_1 <- sqrt((1/N)*sum(((P_n1-meanP_n1)-(O-meanO))^2))
rmse_2 <- sqrt((1/N)*sum(((P_n2-meanP_n2)-(O-meanO))^2))

#correlation
r_1 <- sum((P_n1-meanP_n1)*(O-meanO))/sqrt(sum((P_n1-meanP_n1)^2)*sum((O-meanO)^2))
r_2 <- sum((P_n2-meanP_n2)*(O-meanO))/sqrt(sum((P_n2-meanP_n2)^2)*sum((O-meanO)^2))

#r squared
rsquared_1 <- r_1^2
rsquared_2 <- r_2^2

#print values
mb_1
mb_2
me_1
me_2
nmb_1
nmb_2
nme_1
nme_2
rmse_1
rmse_2
r_1
r_2
rsquared_1
rsquared_2

#plot data against observations
dev.off()
par (mfrow = c(1,2), oma = c(0, 0, 2, 0) )
plot(x = data.subset$obs, y = data.subset$model1, xlab = "obs", ylab = "model1")
plot(x = data.subset$obs, y = data.subset$model2, xlab = "obs", ylab = "model2")
mtext("PM2.5 concentrations in units of µg m-3", outer = TRUE, cex = 1.2)
