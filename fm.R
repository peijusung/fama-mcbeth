#In this exercise, we apply the Fama-MacBeth methodology by using 360 monthly excess returns for market and 456 stocks. Fama and MacBeth (1973) suggest a three-steps approach to test the beta risk-return relationship. 

#import data from csv
setwd("/Users/abusung/Dropbox/Abu's Dropbox/R_practice/R")
mydata = read.csv("FM.csv")





#The first step
#The first step is to separate the time series data by three periods and estimate the beta risk for each stock from the first period.
#Subsample the data using only first 120 months
newdata <- mydata[1:120, ]
# regress the excess return for each stock on market return from the first 120 months by the ordinary least squares (OLS)
#n=numbers of stocks
n <- 456

#return is a matrix of returns of the stocks overtime 
return <- newdata[c(2:457)]
#market is the market return
market <- newdata$market

#Regress the excess return for each stock on market return from the first 120 months by the ordinary least squares. Note: There are 456 regressions in this estimation. "supply()" produce the coefficients for each regression.
my_coefs <- sapply(1:n, function(beta) coef(lm(return[, beta] ~ market)))

#Obtain the coefficients (beta) :The coefficients β_i are the estimated beta risk for the stocks.  
beta <- my_coefs[-c(1), ]

#generate a series from 1 to 456
n <- 1:456
x <- "a"
#Create ID =a1, ...., a456
ID = paste(x, n)

#rank the beta risk: high value of risk has greater ranking
risk.rank <- rank(beta, ties.method = "min")

# Group the stocks into10 portfolios by the beta ranking such that the first group has the smallest estimated beta risks and the tenth group has the largest estimated beta risks. In this exercise, the first six portfolios contain 46 stocks in each portfolio and the other four portfolios have 45 stocks in each one.

#Generate a "group" variable to indicate each portfolio based the risk ranking
group <- findInterval(risk.rank, c(0, 47, 93, 139, 185, 231, 277, 322, 367, 412, 457))
# Add variables "ID", "risk.rank",and "group" into beta 
beta <- cbind(beta, ID, risk.rank, group)

#Subsample the data using the second sub-period (120 months):
newdata <- mydata[121:240, ]
#Remove the market variable from the data
newdata <- newdata[-c(1)]


####################################################################

#The second step
#generate a series from 121 to 240
n <- 121:240
x <- "t"
#Create rownames =t1, ...., t240
rownames = paste(x, n, sep = ".")
#replace row.names with rownames
newdata <- cbind(row.names = rownames, newdata)
#transpose the data
newdata <- t(newdata)
#transfer the data from autometic to data frame
newdata <- as.data.frame(newdata)
#add variable "ID"  into the data
newdata <- cbind(newdata, ID)
#Merge "newdata" with "beta" by "ID"
newdata <- merge(newdata, beta, by = "ID")

#Generate portfolio mean for each group (portfolio)
p.mean <- aggregate(newdata[, 2:121], list(newdata$group), mean)

# Create a variable for group
Group.1 <- p.mean[c(1)]
#Remove "Group.1"from p.mean
p.mean <- p.mean[-c(1)]
#Transpose p.mean
p.mean <- t(p.mean)

#Subsample the data using the second sub-period (120 months):
newdata <- mydata[121:240, ]

#replace row.names with rownames
newdata <- cbind(row.names = rownames, newdata)
#Keep "market" 
newdata <- newdata[c(1)]
#Merge market with p.mean
newdata <- merge(newdata, p.mean, by = "row.names")


#Remove "Row.names"
newdata <- newdata[-c(1)]


# estimate the portfolio betas by regressing the average excess return for each portfolio on market return from month121 to240
# n=numbers of regressions that will be estimated
n <- 10
#p.mean is a matrix of the dependent variables
p.mean <- newdata[c(2:11)]
market <- newdata$market

my_coefs <- sapply(1:n, function(beta) coef(lm(p.mean[, beta] ~ market)))

#Transpose the coefficient metrix   
beta.2 <- t(my_coefs)


#Add "Group.1" into beta.2
beta.2 <- cbind(Group.1 = Group.1, beta.2)



####################################################################


#The third step
#Subsample the data using the last sub-period:
newdata <- mydata[241:360, ]
#Remove "market"
newdata <- newdata[-c(1)]

#generate a series from 241 to 360
n <- 241:360
x <- "t"
#generate rownames =t.241, ..., t.360
rownames = paste(x, n, sep = ".")
#replace row.names = rownames
newdata <- cbind(row.names = rownames, newdata)
#transpose the data
newdata <- t(newdata)
#transfer the data from autometic to data frame
newdata <- as.data.frame(newdata)


#add "ID"
newdata <- cbind(ID, newdata)
#merge the data with "beta"
newdata <- merge(newdata, beta, by = "ID")


#estimate the portfolio mean for the stocks in the last period
p.mean <- aggregate(newdata[, 2:121], list(newdata$group), mean)



#merge the data with "beta.2" by "Group.1"
newdata <- merge(p.mean, beta.2, by = "Group.1")



#Estimate cross-sectional OLS regression of average excess returns for the portfolios on the estimated portfolio betas from the previous step for each month of the final sub-period:
#n=numbers of regressions that will be estimated
n <- 120
#return is a metrix of the dependent variables
return <- newdata[c(2:121)]
#b is a vector of the estimated portfolio betas (independent variables)
b <- newdata$market

my_coefs <- sapply(1:n, function(beta) coef(lm(return[, beta] ~ b)))

#Transpose "my_coefs" to "r"
r <- t(my_coefs)

#transfer "r" to data frame
r <- as.data.frame(r)
#transfer "b" to numeric
r$b <- as.numeric(r$b)




###########################################


#test the null:b=0
t.test(r$b)

#The statistics presents the results of Fama-MacBeth three-step approach. The average of the estimated coefficients is 0.003 with large standard deviation. Given the t-value, we fail to reject the null hypothesis that the beta risk-return relationship is insignificant. The excess returns for the stocks have no significant dependence on their beta risks. To sum it up, the result conflicts with the Capital Asset Pricing Model (CAPM), which suggests that average returns on the stocks do not reflect the attempts of risk-averse investors to hold efficient portfolio. However, we should keep in mind that since we do not have the actual beta risk, error in the estimated beta risk could change our result.






