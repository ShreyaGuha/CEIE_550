#Virtual Lab
#reading data
install.packages("readxl")
library("readxl")
temp <- read_excel("Temperature_NOVA.xlsx")

#part 1: plotting time series
plot(x = temp$`Dataset 1`,
     xlab = "Time (Days)", ylab = "Temperature (ºC)",
     main = "Temperature variability over Northern Virginia for the past 10 years, Location 1",
     cex.main = "0.85", cex.lab = "0.75", cex.axis ="0.50",
     pch = 18, col = "slateblue4", las = 1)

plot(x = temp$`Dataset 2`,
     xlab = "Time (Days)", ylab = "Temperature (ºC)",
     main = "Temperature variability over Northern Virginia for the past 10 years, Location 2",
     cex.main = "0.85", cex.lab = "0.75", cex.axis ="0.50",
     pch = 18, col = "slateblue4", las = 1)

plot(x = temp$`Dataset 3`,
     xlab = "Time (Days)", ylab = "Temperature (ºC)",
     main = "Temperature variability over Northern Virginia for the past 10 years, Location 3",
     cex.main = "0.85", cex.lab = "0.75", cex.axis ="0.50",
     pch = 18, col = "slateblue4", las = 1)

#part 2: computing statistics
summary(temp)
mean(temp$`Dataset 1`)
mean(temp$`Dataset 2`)
mean(temp$`Dataset 3`)
sd(temp$`Dataset 1`)
sd(temp$`Dataset 2`)
sd(temp$`Dataset 3`)

#part 3: box and whisker plots
boxplot(temp$`Dataset 1`, temp$`Dataset 2`, temp$`Dataset 3`,
        main="Temperature variability over NOVA for the past 10 years",
        names = c("Location 1", "Location 2", "Location 3"),
        ylab="temperature (ºC)",
        col="orange",
        border="brown",
        cex.main = "0.85", cex.lab = "0.75", cex.axis = "0.75")

