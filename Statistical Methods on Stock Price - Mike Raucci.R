setwd("C:/Users/raucc/OneDrive/Documents/R Files")
library(readxl)
MA_541_Course_Project_Data <- read_excel("MA 541 Course Project Data.xlsx")
head(MA_541_Course_Project_Data)
View(MA_541_Course_Project_Data)

ETF_return <- (MA_541_Course_Project_Data$Close_ETF)
oil_price <- (MA_541_Course_Project_Data$oil)
gold_price <- (MA_541_Course_Project_Data$gold)
JPM_return <- (MA_541_Course_Project_Data$JPM)


##############################PART 1#############################################################

# Sample mean and standard deviation: daily ETF return
mean(ETF_return)
sd(ETF_return)

# Sample mean and standard deviation: relative change in price of oil
mean(oil_price)
sd(oil_price)

# Sample mean and standard deviation: relative change in price of gold
mean(gold_price)
sd(gold_price)

# Sample mean and standard deviation: daily return of JPM stock
mean(JPM_return)
sd(JPM_return)

# Sample Correlation: daily ETF return to relative change in price of oil
cor(ETF_return, oil_price)

#Sample Correlation: daily ETF return to relative change in price of gold
cor(ETF_return, gold_price)

#Sample Correlation: daily ETF return to daily return of JPM stock
cor(ETF_return, JPM_return)

#Sample Correlation: relative change in price of oil to relative change in price of gold
cor(oil_price, gold_price)

#Sample Correlation: relative change in price of oil to daily return of JPM stock
cor(oil_price, JPM_return)

#Sample Correlation: daily change in price of gold to daily return of JPM stock
cor(gold_price, JPM_return)


###############################PART2#####################################################

# Histogram for daily ETF return
hist(ETF_return, breaks = 100, col="blue", main= "Daily ETF Return", xlab = "Price in $")

# Histogram for change in price of oil
hist(oil_price, breaks = 40, col="grey", main = "Change in the price of Oil", xlab = "Price in $")

# Histogram for change in price of gold
hist(gold_price, breaks = 40, col="orange", main = "Change in the price of Gold", xlab = "Price in $")

# Histogram for daily return of JPM stock 
hist(JPM_return, breaks = 40, col="darkgreen", main = "Daily Return of JPM Stock", xlab = "Price in $")

# Time Series plot daily ETF return
library(ggplot2)
tsETF_return <- ggplot(MA_541_Course_Project_Data, aes(x=seq(1,1000,1), y=Close_ETF))+geom_line()
tsETF_return  

# Time Series plot change in price of crude oil
tsoil_price <- ggplot(MA_541_Course_Project_Data, aes(x=seq(1,1000,1), y=oil))+geom_line()
tsoil_price

# Time Series plot change in price of gold
tsgold_price <- ggplot(MA_541_Course_Project_Data, aes(x=seq(1,1000,1), y=gold))+geom_line()
tsgold_price


# Time Series plot daily return of JPM stock
tsJPM_return <- ggplot(MA_541_Course_Project_Data, aes(x=seq(1,1000,1), y=JPM))+geom_line()
tsJPM_return

# Time Series plot all 4 columns 
ggplot()+ geom_line(data= MA_541_Course_Project_Data, aes(x=seq(1,1000,1), y=Close_ETF), color = "red") + 
  geom_line(data= MA_541_Course_Project_Data, aes(x=seq(1,1000,1), y=oil), color = "darkgreen") +
  geom_line(data= MA_541_Course_Project_Data, aes(x=seq(1,1000,1), y=gold), color = "darkorange") +
  geom_line(data= MA_541_Course_Project_Data, aes(x=seq(1,1000,1), y=JPM), color = "navyblue") 



# Scatter plot for relationship between daily ETF return and daily relative change in price of crude oil
plot(x = ETF_return, y = oil_price, xlab= "Daily ETF Return (in $)",
     ylab="Change in Oil price (in $)", 
     main = "Correlation between ETF return and relative change in price of oil")

# Scatter plot for relationship between daily ETF return and daily relative change in price of gold 
plot(x = ETF_return, y = gold_price, xlab= "Daily ETF Return (in $)",
     ylab="Change in Gold price (in $)",
     main = "Correlation between ETF return and change in price of gold")

# Scatter plot for relationship between daily ETF return and daily return of JPM stock
plot(x = ETF_return, y = JPM_return, xlab= "Daily ETF Return (in $)", 
     ylab="Daily return in JPM stock (in $)",
     main = "Correlation between ETF return and return of JPM stock")


##################################PART 3###########################################

##Propose an assumption/hypothesis regarding the type of distribution each column of 
##of the data set may follow:

##ETF: Normal Distribution
##H_0: mu = 121.15
##H_a: mu does not equal 121.15


##Oil: Normal Distribution
##H_0: mu = 0.001 (0)
##H_a: mu does not equal 0.001(0)


##Gold: Right-tailed t-test
##H_0: mu = 0.001 (0)
##H_a: mu > 0.001 (0)


##JPM: Left-tailed t-test
#H_0: mu = 0.001 (0)
#H_a: mu < 0.001 (<0)


##Then verify or object that assumption/hypothesis with appropriate tests

##ETF: Shapiro-Wilk Test
shapiro.test(ETF_return)
##  1.166e-10 < 0.05; Therefore, Reject null hypothesis. 

##Oil: Shapiro-Wilk Test
shapiro.test(oil_price)
##  5.487e-07 < 0.05; Therefore, Reject null hypothesis.

##Gold: Right-tailed test
t.test(gold_price, mu = 0.001, alternative = "greater")
##  0.827 > 0.05; Therefore, fail to reject null hypothesis. 

##JPM: Left-tailed t-test
t.test(JPM_return, mu = 0.001, alternative = "less")
## 0.09 > 0.05; Therefore, fail to reject null hypothesis. 


#############################PART 4##########################################################

##Consider the ETF column (1000 values) as the population(x)
x<-MA_541_Course_Project_Data$Close_ETF

##calculate the mean and standard deviation of the population
mu_x <- mean(x)
mu_x
sigma_x <- sd(x)
sigma_x

##break the population into 50 groups sequentially (each group includes 20 values)
x <- x[order(x)]
groupsfif_size <- length(x)/50
groupsfif<- split(x, rep(1:50, each = 20))
View(groupsfif)

##Calculate the sample mean of each group
groupsfif_means <- sapply(groupsfif, mean)
View(groupsfif_means)

##Draw a histogram of all the sample means
hist(groupsfif_means, breaks = 15, col = "blue", main = "Sample Means for each Group")

##Calculate the mean and standard deviation of the sample means
mu_xbar1 <- mean(groupsfif_means)
mu_xbar1
sigma_xbar1 <-sd(groupsfif_means)
sigma_xbar1

##Make  comparison between mu_x and mu_xbar, and between (sigma_x/sqrt(n)) and sigma_xbar
mu_x
mu_xbar1
sigma_x/(sqrt(groupsfif_size))
sigma_xbar1

##Break the population into ten groups sequentially (each group includes 100 values)
groupsten_size <- length(x)/10
groupsten<- split(x, rep(1:10, each = 100))
View(groupsten)

##Calculate the sample mean of each group
groupsten_means <- sapply(groupsten, mean)
View(groupsten_means)

##Draw a histogram of all the sample means
hist(groupsfif_means, breaks = 15, col = "yellow", main = "Sample Means for each Group")

##Calculate the mean and standard deviation of the sample means
mu_xbar2 <-mean(groupsten_means)
mu_xbar2
sigma_xbar2 <- sd(groupsten_means)
sigma_xbar2

##Make  comparison between mu_x and mu_xbar, and between (sigma_x/sqrt(n)) and sigma_xbar
mu_x
mu_xbar2
sigma_x/(sqrt(groupsten_size))
sigma_xbar2

##Generate 50 random samples, with replacement, from the population (each sample is size 20)
samplesfif <- list()
for(i in 1:50)
  samplesfif[[i]] <- sample(x, size=20, replace=TRUE)
View(samplesfif)

##Calculate the sample mean of each group
samplesfif_means <- sapply(samplesfif, mean)


##draw a histogram of all the sample means
hist(samplesfif_means, breaks = 6, col = "green", main = "Sample Means for each Group")

##Calculate the mean and standard deviation of the sample means
mu_xbar3 <- mean(samplesfif_means)
mu_xbar3
sigma_xbar3 <- sd(samplesfif_means)
sigma_xbar3

##Make  comparison between mu_x and mu_xbar, and between (sigma_x/sqrt(n)) and sigma_xbar
mu_x
mu_xbar3
sigma_x / (sqrt(20))
sigma_xbar3

##Generate 10 random samples from the population (each group is size 100)
samplesten <- list()
for(i in 1:10)
  samplesten[[i]] <- sample(x, size=100, replace=TRUE)
View(samplesten)

##Calculate the sample mean of each group
samplesten_means <- sapply(samplesten, mean)

##Draw a histogram of all the sample means
hist(samplesten_means, breaks = 5, col = "orange", main = "Sample Means for each Group")

#calculate the mean and standard deviation of the sample means
mu_xbar4 <- mean(samplesten_means)
mu_xbar4
sigma_xbar4 <- sd(samplesten_means)
sigma_xbar4

##Make  comparison between mu_x and mu_xbar, and between (sigma_x/sqrt(n)) and sigma_xbar
mu_x
mu_xbar4
sigma_x / (sqrt(100))
sigma_xbar4

#######Part 5####################################################################################

##pick one of the 10 random samples generated in step 10 of part 4
xbarsample <- sample(samplesten_means, 1)
xbarpop <- sample(samplesten_means, 1000, replace = TRUE)

#Construct an appropriate 95% confidence interval of the mean 
n <- 100

marginten <- qt(0.95, df=n-1)*sigma_xbar4/sqrt(n)
marginten

lowten <- xbarsample - marginten
lowten
highten <- xbarsample + marginten
highten

##pick on of the 50 random samples generated in step 8 of part 4
lbarsample <- sample(samplesfif_means, 1)
lbarpop <- sample(samplesfif_means, 1000, replace = TRUE)

##Construct an appropriate 95% confidence interval of the mean
m <- 20

marginfif <- qt(0.95, df=n-1)*sigma_xbar3/sqrt(m)
marginfif

lowfif <- lbarsample - marginfif
lowfif
highfif <- lbarsample + marginfif
highfif


##########Part 6##############################################################################

##Use the sample picked in step 1 of part 5 to test
## H_0: mu equals 100 ; H_a: mu does not equal 100 at significance level 0.05
t.test(xbarpop, alternative = "two.sided", mu = 100)

##Use the sample picked in step 2 of part 5 to test
##H_0: mu equals 100 ; H_a: mu does not equal 100 at significance level 0.05
t.test(lbarpop, alternative = "two.sided", mu = 100)

##Use the sample picked in step 2 of part 5 to test
## H_0: sigma equals 15 ; H_a: sigma does not equal 15
chisq.test(lbarpop)

##Use the sample picked in step 2 of part 5 to test
## H_0: sigma equals 15 ; H_a: sigma is less than 15
t.test(lbarpop, alternative = "less", sigma = 15)
########################################PART 7################################################

#1.
#Consider the entire Gold column as a random sample from the first population.

goldX<-sample(MA_541_Course_Project_Data$gold, 450)

#Consider the entire Oil column as a random sample from the second population.

oilY<-sample(MA_541_Course_Project_Data$oil, 450)

#form a hypothesis and test it to see if Gold and Oil have equal means in the significance level 0.05

t.test(goldX, oilY, paired = TRUE)

#2.
#Subtract the entire Gold column from the entire Oil column

new_sample <- (MA_541_Course_Project_Data$oil - MA_541_Course_Project_Data$gold)

#Generate a random sample of differences

differences <- sample(new_sample, 200, replace = TRUE)

#form a hypothesis and test it to see if Gold and Oil have equal means in the significance level 0.05

t.test(new_sample, differences, conf.level = 0.95)

#3.
#Consider the entire Gold column as a random sample from the first population. 
gold_sample <- sample(MA_541_Course_Project_Data$gold, 200)
oil_sample <- sample(MA_541_Course_Project_Data$oil, 200)

#form a hypothesis and test it to see if Gold and Oil have equal standard deviations in the significance level 0.05

chisq.test(gold_sample, oil_sample)

#########################################PART 8###############################################


# 1. Draw a scatter plot of ETF(Y) and Gold(X)
X<-MA_541_Course_Project_Data$gold
Y<-MA_541_Course_Project_Data$Close_ETF
plot(X, Y, xlab = "Gold", ylab = "ETF Closing Price", col = "purple")

# 2. Calculate the coefficient of correlation between ETF and Gold
correl<-cor(X, Y)
correl

# 3. Fit the regression line to the scatter plot 
line<- abline(lm(Y~X, data=MA_541_Course_Project_Data), col = "red")
model<-lm(Y~X)
model
model_summ<-summary(model)
model_summ

## What are the intercept and slope of the line
intercept_val<-model_summ$coefficients[1,1]
intercept_val
sx<-sd(X)
sy<-sd(Y)
slope<-(correl)*(sy/sx)
slope

#4 Conduct a two tailed t-test with H_0: Beta_1 = 0. 
t.test(X, Y, alternative = "two.sided", conf.level = 0.99)

## What is the p-value?? Is the relationship significant at significance level 0.01?
pVal <- t.test(X, Y, alternative = "two.sided", conf.level = 0.99)$p.value
pVal

#5. Suppose you use the coefficient of determination to asses the quality of this fitting. 

determination <- correl *correl
determination


#6 What are the Assumptions made for this model fitting:

##-1. the residuals are independent
##-2. the residuals are normally distributed
##-3. the residuals have constant variance
##-4. the relationship between the independent variable is linear

#7. Given the daily relative change in gold price is 0.005127
fixed_X <- data.frame(x = rep(0.005127, 1000))
View(fixed_X)

##Calculate the 99% confidence interval of the mean daily ETF return
t.test(fixed_X, Y, alternative = "two.sided", conf.level = 0.99)

##Calculate the 99% prediction interval of the individual daily ETF return
new_data = data.frame(fixed_X, Y)

prediction<-predict(model, newdata = new_data, interval = "predict", conf.level = 0.99)
head(prediction)

#######################################PART 9##################################################
ETF<-MA_541_Course_Project_Data$Close_ETF
OIL<-MA_541_Course_Project_Data$oil
GOLD<-MA_541_Course_Project_Data$gold
new_data<- data.frame(ETF, OIL, GOLD)
View(new_data)

##Fit a linear regression model to the data with the ETF variable as the response.
model<-lm(ETF~OIL+GOLD, data = new_data)
model

##Evaluate your model with adjusted R^2
summary(model)
summary(model)$adj.r.squared

##################################PART 10#################################################

##Calculate the residuals of the model fitting you did in Part 9
resid(model)

##Residual standard error
stanerr <- sqrt(deviance(model)/df.residual(model))
stanerr

##Check the four assumptions made for the error terms of the multiple regression model
##1. no multicollinearity
cor(OIL, GOLD)


##2. normality
shapiro.test(residuals(model))

##3. constant variance
plot(model)

##4. independence
plot(model)



