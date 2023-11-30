#cntrl enter run
#cnrl l clear console
#cnrl s save

# read data
crime=read.csv("crimedata.csv")
glimpse(crime) #str(crime)

# extract variables we care about
data = crime[c("communityname","LandArea","population","racepctblack","agePct12t29","perCapInc","PctLess9thGrade","PctUnemployed","ViolentCrimesPerPop","nonViolPerPop")]

# drop all rows with NA
data = na.omit(data)

# look at dataset details
str(data)

# summary
summary(data)

# histgram and boxplot for distribution
par(mfrow=c(1,2))
hist(data$ViolentCrimesPerPop)
hist(data$nonViolPerPop)

par(mfrow=c(1,2))
boxplot(data$racepctblack, xlab = "race percent of black population", ylab = "%")
boxplot(data$agePct12t29, xlab = "age precent 12-29", ylab = "%")


# corelation: violent crimes
cor(data$ViolentCrimesPerPop, data[c("population","racepctblack","agePct12t29","perCapInc","PctLess9thGrade","PctUnemployed")])

# corelation: non-violent crimes
cor(data$nonViolPerPop, data[c("population","racepctblack","agePct12t29","perCapInc","PctLess9thGrade","PctUnemployed")])

# scatter plot: violent crimes
par(mfrow=c(2,3))
plot(log(data$population), data$ViolentCrimesPerPop)
plot(data$racepctblack, data$ViolentCrimesPerPop)
plot(data$agePct12t29, data$ViolentCrimesPerPop)
plot(data$perCapInc, data$ViolentCrimesPerPop)
plot(data$PctLess9thGrade, data$ViolentCrimesPerPop)
plot(data$PctUnemployed, data$ViolentCrimesPerPop)

# scatter plot: non-violent crimes
par(mfrow=c(2,3))
plot(log(data$population), data$nonViolPerPop)
plot(data$racepctblack, data$nonViolPerPop)
plot(data$agePct12t29, data$nonViolPerPop)
plot(data$perCapInc, data$nonViolPerPop)
plot(data$PctLess9thGrade, data$nonViolPerPop)
plot(data$PctUnemployed, data$nonViolPerPop)

# Outlier - Data Transformation
par(mfrow=c(1,2))
hist(data$population)
hist(log(data$population))


# Linear Regression: Violent crimes
summary(lm(log(population) ~ ViolentCrimesPerPop, data=data))
summary(lm(racepctblack ~ ViolentCrimesPerPop, data=data))
summary(lm(agePct12t29 ~ ViolentCrimesPerPop, data=data))
summary(lm(perCapInc ~ ViolentCrimesPerPop, data=data))
summary(lm(PctLess9thGrade ~ ViolentCrimesPerPop, data=data))
summary(lm(PctUnemployed ~ ViolentCrimesPerPop, data=data))


# Linear Regression: Non-Violent crimes
summary(lm(log(population) ~ nonViolPerPop, data=data))
summary(lm(racepctblack ~ nonViolPerPop, data=data))
summary(lm(agePct12t29 ~ nonViolPerPop, data=data))
summary(lm(perCapInc ~ nonViolPerPop, data=data))
summary(lm(PctLess9thGrade ~ nonViolPerPop, data=data))
summary(lm(PctUnemployed ~ nonViolPerPop, data=data))

