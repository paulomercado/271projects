#Loading dataset
setwd("D:/School/ADMU/M AMF/SEM 2/271.7")
data=read.csv("rates.csv")

#Fixing Date
data$Date <- as.Date(data$Date, format = "%m/%d/%Y")
dates <- data$Date

#get dy
data_diff <- data.frame(lapply(data[ , sapply(data, is.numeric)], diff))
data_diff$Date <- dates[-1]

#filter
data_filtered <- subset(data_diff, Date <= as.Date("1996-12-31"))

#express in bps
data.pc <- data_filtered[ , -ncol(data_filtered)] / 100

names(data.pc)

#perform PCA on scaled data
pc1 = prcomp(data.pc, scale=T)
summary(pc1)
plot(pc1, type="l")
pc1$rotation
#get cashflows of portfolio
CF <- matrix(c(
  5, 105, 0,   0,   0,   # 2Y
  5, 5, 105,   0,   0,   # 3Y
  -5,-5, -5,  -5, -105    # 5Y
), nrow=3, byrow=TRUE)

total_CF <- colSums(CF)

# PV 
y <- 0.05
years <- 1:5
DF <- exp(-y*years)

# Present value of each cash flow: Bi = CF * DF
Bi <- total_CF * DF  

BiTi <- -Bi * years/10000  # in bps

weights = c(0,0,1,1,1,1,1,0,0,0)
a1 <- (weights * pc1$rotation[ , "PC1"])[weights != 0]
a2 <- (weights * pc1$rotation[ , "PC2"])[weights != 0]
a3 <- (weights * pc1$rotation[ , "PC3"])[weights != 0]

f1 <- sum(a1*BiTi)
f2 <- sum(a2*BiTi)
f3 <- sum(a3*BiTi)

VaR1 <- qnorm(0.95)*(f1^2)*pc1$sdev[1]^2
VaR2 <- qnorm(0.95)*((f1^2)*pc1$sdev[1]^2+(f2^2)*pc1$sdev[2]^2)
VaR3 <- qnorm(0.95)*((f1^2)*pc1$sdev[1]^2+(f2^2)*pc1$sdev[2]^2+(f3^2)*pc1$sdev[3]^2)

#backtesting using 1997 data
backtestdata <- subset(data_diff, Date > as.Date("1996-12-31"))[, 3:7]
backtestdates <- dates[dates > as.Date("1996-12-31")]

#using bond duration to calculate change in bond price i.e dB=B*D*dy=sum(BiTi)*dy
dailyPL <- as.matrix(backtestdata) %*% BiTi

backtest_results <- data.frame(
  Date = backtestdates,  
  dailyPL = dailyPL,
  exceed_1 = dailyPL < -VaR1,
  exceed_2 = dailyPL < -VaR2,
  exceed_3 = dailyPL < -VaR3
)

colSums(backtest_results[ , 3:5])/length(backtestdates) # % of days where VaR has been breached
