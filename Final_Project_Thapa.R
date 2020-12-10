require(mosaic)

library(readxl)
Crime = read_excel("Desktop/Crime_R.xlsx")

#ii & iii) To compare the relationship between Southern States and crime rate we will use 
#a paired t-test to compare the average crime rate of Northern to Southern states before and after
#a 10 year period because we are comparing the same groups but in different time periods. (Samip) 

# Crime rate for Southern state before and after 10 years
S.Crime = Crime$CrimeRate[Crime$Southern==1]
S.Crime10 = Crime$CrimeRate10[Crime$Southern==1]

df = data.frame(S.Crime,S.Crime10)

summary(df)

sd(S.Crime)
sd(S.Crime10)

df = transform(df, DIFF = S.Crime - S.Crime10)
favstats(~DIFF, data = df)

par(mfrow=c(1,2))
boxplot(df$DIFF, horizontal = TRUE)
d = density(df$DIFF)
plot(d, main="Difference in Crime rates")

t.test(df$S.Crime, df$S.Crime10, paired = TRUE)

#Removing one outlier 
df3 = df[-c(1), ]
boxplot(df3$DIFF, horizontal = TRUE)
d = density(df3$DIFF)
plot(d, main="Difference in Crime rates")

t.test(df3$S.Crime, df3$S.Crime10, paired = TRUE)

#Removing two outliers 
df2 = df[-c(1,2), ]
boxplot(df2$DIFF, horizontal = TRUE)
densityplot(df2$DIFF)

t.test(df2$S.Crime, df2$S.Crime10, paired = TRUE)

#Crime rate for Northern state before and after 10 years
N.Crime = Crime$CrimeRate[Crime$Southern==0]
N.Crime10 = Crime$CrimeRate10[Crime$Southern==0]

df = data.frame(N.Crime,N.Crime10)

summary(df)

sd(N.Crime)
sd(N.Crime10)

df = transform(df, DIFF = N.Crime - N.Crime10)
favstats(~DIFF, data = df)

boxplot(df$DIFF, horizontal = TRUE)
d = density(df$DIFF)
plot(d, main="Difference in Crime rates")

t.test(df$N.Crime, df$N.Crime10, paired = TRUE)

par(mfrow=c(1,1))


#iv) To check the relationship between crime rate and police expenditure, 
#we will be using correlation techniques which are appropriate for answering the question 
#as correlation helps to identify the strength among those two variables. (Samip)

#For year 0 
par(mfrow=c(2,2))

plot(Crime$CrimeRate~Crime$ExpenditureYear0)
cor.test(Crime$ExpenditureYear0, Crime$CrimeRate, method=c("spearman"))
cor(Crime$ExpenditureYear0, Crime$CrimeRate, method=c("spearman"))

lm1 = lm(Crime$CrimeRate~Crime$ExpenditureYear0)
summary(lm1)
plot(Crime$CrimeRate~Crime$ExpenditureYear0)
abline(lm1, col = "blue")
plot(lm1, which = 1)
plot(lm1, which = 2)

#Log of Expenditure 
lm2 = lm(Crime$CrimeRate~log(Crime$ExpenditureYear0))
summary(lm2)
plot(Crime$CrimeRate~log(Crime$ExpenditureYear0))
plot(Crime$CrimeRate~log(Crime$ExpenditureYear0))
abline(lm2, col = "blue")
plot(lm2, which = 1)
plot(lm2, which = 2)

#Log of crime rate 
lm3 = lm(log(Crime$CrimeRate)~Crime$ExpenditureYear0)
summary(lm3)
plot(log(Crime$CrimeRate)~Crime$ExpenditureYear0)
plot(log(Crime$CrimeRate)~Crime$ExpenditureYear0)
abline(lm3, col = "blue")
plot(lm3, which = 1)
plot(lm3, which = 2)

#Log of both expenditure and crime rate 
lm3 = lm(log(Crime$CrimeRate)~log(Crime$ExpenditureYear0))
summary(lm3)
plot(log(Crime$CrimeRate)~log(Crime$ExpenditureYear0))
plot(log(Crime$CrimeRate)~log(Crime$ExpenditureYear0))
abline(lm3, col = "blue")
plot(lm3, which = 1)
plot(lm3, which = 2)
  
#For year 10
plot(Crime$CrimeRate10~Crime$ExpenditureYear10)
cor.test(Crime$ExpenditureYear10, Crime$CrimeRate10, method=c("spearman"))
cor(Crime$ExpenditureYear10, Crime$CrimeRate10, method=c("spearman"))

lm2 = lm(Crime$CrimeRate10~Crime$ExpenditureYear10)
summary(lm2)
plot(Crime$CrimeRate10~Crime$ExpenditureYear10)
abline(lm2, col = "blue")
plot(lm2, which = 1)
plot(lm2, which = 2)

#Log of Expenditure 
plot(Crime$CrimeRate~log(Crime$ExpenditureYear10))
lm4 = lm(Crime$CrimeRate~log(Crime$ExpenditureYear10))
summary(lm4)
plot(Crime$CrimeRate~log(Crime$ExpenditureYear10))
abline(lm4, col = "blue")
plot(lm4, which = 1)
plot(lm4, which = 2)

#Log of crime rate 
lm3 = lm(log(Crime$CrimeRate)~Crime$ExpenditureYear10)
summary(lm3)
plot(log(Crime$CrimeRate)~Crime$ExpenditureYear10)
plot(log(Crime$CrimeRate)~Crime$ExpenditureYear10)
abline(lm3, col = "blue")
plot(lm3, which = 1)
plot(lm3, which = 2)

#Log of both expenditure and crime rate 
plot(log(Crime$CrimeRate)~log(Crime$ExpenditureYear10))
lm4 = lm(log(Crime$CrimeRate)~log(Crime$ExpenditureYear10))
summary(lm4)
plot(log(Crime$CrimeRate)~log(Crime$ExpenditureYear10))
abline(lm4, col = "blue")
plot(lm4, which = 1)
plot(lm4, which = 2)

#v) To see whether the number of families below half wage predict crime rate, 
#we can use simple linear regression. This analysis technique is appropriate to 
#address this question because we can see the relationship between the two variables 
#and also see if the independent variable (number of families) is a good predictor 
#for crime rate. (Samip)

#For year 0 
par(mfrow=c(2,2))
plot(Crime$CrimeRate~Crime$BelowWage)

lm3 = lm(Crime$CrimeRate~Crime$BelowWage)
summary(lm3)
plot(Crime$CrimeRate~Crime$BelowWage)
abline(lm3, col = "blue")
plot(lm3, which = 1)
plot(lm3, which = 2)

#Log of below wage 
plot(Crime$CrimeRate~log(Crime$BelowWage))
lm3 = lm(Crime$CrimeRate~log(Crime$BelowWage))
summary(lm3)
plot(Crime$CrimeRate~log(Crime$BelowWage))
abline(lm3, col = "blue")
plot(lm3, which = 1)
plot(lm3, which = 2)

#Log of crime rate
plot(log(Crime$CrimeRate)~Crime$BelowWage)
lm3 = lm(log(Crime$CrimeRate)~Crime$BelowWage)
summary(lm3)
plot(log(Crime$CrimeRate)~Crime$BelowWage)
abline(lm3, col = "blue")
plot(lm3, which = 1)
plot(lm3, which = 2)

#Log of both
plot(log(Crime$CrimeRate)~log(Crime$BelowWage))
lm3 = lm(log(Crime$CrimeRate)~log(Crime$BelowWage))
summary(lm3)
plot(log(Crime$CrimeRate)~log(Crime$BelowWage))
abline(lm3, col = "blue")
plot(lm3, which = 1)
plot(lm3, which = 2)

#For year 10 
plot(Crime$CrimeRate10~Crime$BelowWage10)
lm4 = lm(Crime$CrimeRate10~Crime$BelowWage10)
summary(lm4)
plot(Crime$CrimeRate10~Crime$BelowWage10)
abline(lm4, col = "blue")
plot(lm4, which = 1)
plot(lm4, which = 2)

#Log of below wage
plot(Crime$CrimeRate10~log(Crime$BelowWage10))
lm4 = lm(Crime$CrimeRate10~log(Crime$BelowWage10))
summary(lm4)
plot(Crime$CrimeRate10~log(Crime$BelowWage10))
abline(lm4, col = "blue")
plot(lm4, which = 1)
plot(lm4, which = 2)

#Log of crime rate
plot(log(Crime$CrimeRate10)~Crime$BelowWage10)
lm4 = lm(log(Crime$CrimeRate10)~Crime$BelowWage10)
summary(lm4)
plot(log(Crime$CrimeRate10)~Crime$BelowWage10)
abline(lm4, col = "blue")
plot(lm4, which = 1)
plot(lm4, which = 2)

#Log of both 
plot(log(Crime$CrimeRate10)~log(Crime$BelowWage10))
lm4 = lm(log(Crime$CrimeRate10)~log(Crime$BelowWage10))
summary(lm4)
plot(log(Crime$CrimeRate10)~log(Crime$BelowWage10))
abline(lm4, col = "blue")
plot(lm4, which = 1)
plot(lm4, which = 2)

