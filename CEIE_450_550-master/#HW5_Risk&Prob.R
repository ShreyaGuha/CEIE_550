#read and summarize data
dulles_data = read.csv("dulles_airport_hw.csv")
summary(dulles_data)

N <- length (dulles_data$MonthlyMeanTemperature)
T <- dulles_data$MonthlyMeanTemperature

#plot pdf of raw data as histogram
hist(T, freq = F, breaks = 85,
     xlab = "Monthly mean temperatures(F)",
     main ="Histogram of PDF of Monthly Mean Temperatures")

#plot pdf of data taking log values as histogram
hist(log(T), freq = F, breaks = 85,
     xlab = "Log of Monthly mean temperatures(F)",
     main ="Histogram of PDF of Log of Monthly Mean Temperatures")

#plot histograms with less breaks
hist(T, freq = F, breaks = 5,
     xlab = "Monthly mean temperatures(F)",
     main ="Histogram of PDF of Monthly Mean Temperatures with less breaks",
     cex.main = "0.75", cex.lab = "0.75", cex.axis = "0.50")

#repeat same with log values
hist(log(T), freq = F, breaks = 5,
     xlab = "Log of Monthly mean temperatures(F)",
     main ="Histogram of PDF of Log of Monthly Mean Temperatures with less breaks",
     cex.main = "0.75", cex.lab = "0.75", cex.axis = "0.50")

#Calculating mean and Standard deviation of monthly mean temperatures
mean(T)
sd(T)

#probability that monthly mean temperature is greater than 70
T_greater_than_70 <- T [T > 70]
summary(T_greater_than_70)
Pr1 <- (length(T_greater_than_70)/N)*100 #in percent
Pr1

#Pr[ 30 < Temperature < 40 ]
T_within_30_and_40 <- T [T > 30 & T < 40]
summary(T_within_30_and_40)
Pr2 <- (length(T_within_30_and_40)/N)*100 #in percent
Pr2

#Pr[ Temperature = 50 ]
T_equal_to_50 <- T [T = 50]
summary(T_equal_to_50)
Pr3 <- (length(T_equal_to_50)/N)*100 #in percent
Pr3

#plot seasonal trend
plot(x = 1:N, y = T, type = 'l',
     xlab = "time (months)", ylab = "monthly mean temperatures (F)",
     main = "Seasonal trend of Monthly mean temperatures over Dulles in F",
     cex.main = "1.00")

