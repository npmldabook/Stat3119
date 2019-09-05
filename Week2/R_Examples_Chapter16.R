## Fitting data with R for Example 16.4 
## (The Kenton Food Company wished to test four different package designs for a new breakfast cereal. 


####### Step One : read and check data #######

# Ex16 <- read.table(file.choose(), header=F) # find the file location from file browser 
# Ex16 <- read.table("c:/stat3119/CH16TA01.txt")      # specify where the file is in your local computer

Ex16 <- read.table(url("https://raw.githubusercontent.com/npmldabook/Stat3119/master/Week2/CH16_TA01.txt")) # read directly
dim(Ex16)
head(Ex16, 2) # show 2 lines of data

# rename the variables
names(Ex16)<- c("sales", "package", "stores")

class(Ex16$package)
Ex16$package<- as.factor(Ex16$package) 
class(Ex16$package)

str(Ex16) #19 obs
head(Ex16, 2) 

#Check the factor levels:

levels(Ex16$package)


#Check the sample size ni for each level:
table(Ex16$package)


#Plots of response $Y_{ij}$ by the $r$ factor levels using a stripe-chart or boxplots:

par(mfrow=c(1,2)) 
stripchart(sales ~ package, vertical = TRUE,  pch=1, data = Ex16, xlab="Package Design")
boxplot(sales ~ package, data = Ex16, col=rainbow(4))



######## APPROACH I: use formulas  #############

## Estimate mean responses (R) ##

#-  Method 1: get sample mean for each given factor level (same as Table 16.1) 

# this is to calculate the mean for each level of package
Meani<- with(Ex16,  by( sales, package, mean)) 

Overall.mean   = mean(Ex16$sales)
Total.dev = Ex16$sales- Overall.mean

list(Factor_mean= Meani, Overall_mean= Overall.mean)

Factor.mean   =   rep(as.numeric(Meani), table( Ex16$package))
Tr.dev = Factor.mean - Overall.mean
Error.dev = Ex16$sales -Factor.mean
data.frame(sales=Ex16$sales, Overall.mean, Factor.mean, Total.dev, Tr.dev, Error.dev)
 
# or show rounded value with 2 digit
round(data.frame(sales=Ex16$sales, Overall.mean, Factor.mean, Total.dev, Tr.dev, Error.dev),2)

#########calculate SS and MS and test-stat by formula ############

SSTO = sum(Total.dev^2)
SSTR = sum(Tr.dev^2)
SSE = sum(Error.dev^2)

MSTR= sum(Tr.dev^2)/(4-1)
MSE= sum(Error.dev^2)/(19-4)

Fstat=  MSTR/MSE

round(unlist(list(SSTO=SSTO,SSTR=SSTR, SSE=SSE, MSTR=MSTR, MSE=MSE, Fstat=Fstat)),2)

# Get the critical values
# For alpha=0.05
qf(.95, df1=3, df2=15)

# For alpha=0.01
qf(.99, df1=3, df2=15)

# Finaly, we calculate the P-value= Pr(F> F^*) to verify ANOVA table.

1- pf(18.59,df1=3, df2=15)

# estimate overall unweight mean and factor level deviation for factor effects model

cell.means<- as.numeric(with(Ex16,  by( sales, package, mean)))
Overall.mean <- mean(cell.means)
tau <- cell.means - Overall.mean
list(cell.means=cell.means, Overall.mean=Overall.mean , tau=tau)

######## APPROACH II : easier #############

# Use one-way ANOVA  aov() function to fit the data and get model coefficient.
#-  Use a model: useful later for other parameterization, note: package is a factor variable (using numeric will result in an error message)

fit <- aov(sales ~ package, data = Ex16)

# factor level means
predict(fit, newdata = data.frame(package = factor(1:4)))


# residials
fit$residuals


##  Testing in  One-way ANOVA Model : ANOVA table

summary(fit)

## estimate overall unweight mean and factor level deviation for factor effects model ##

options(contrasts = c("contr.sum", "contr.poly")) 
fit2 <- aov(sales ~ package, data = Ex16)
dummy.coef(fit2)

####################################################### 
####  Use R to estimate power and sample size ########

# A.  First, from sample size to power:  If the company set sample size per group is set $n_i=5$, then what would be the study power?
  
mu     <- c(12.5, 13, 18, 21)
sigma2 <- 3.5^2
power.anova.test(groups = length(mu), n = 5, between.var = var(mu), within.var = sigma2)


# B.  Second,  if the company set power to be 90%,  then what would be the required sample size per group?

# Given assumption of means and sigma 
mu     <- c(12.5, 13, 18, 21)
sigma2 <- 3.5^2

# B1) From planned sample size to calculate power 

? power.anova.test

power.anova.test(groups = length(mu), n = 5, between.var = var(mu), within.var = sigma2)


# B2) From give power to calculate sample size

power.anova.test(groups = length(mu),  between.var = var(mu), within.var = sigma2 , power = 0.9)

#B) change significant level
power.anova.test(groups = length(mu),  between.var = var(mu), within.var = sigma2 , power = 0.9, 
                 sig.level = 0.01)

## Snow tires example (the textbook page 719): 

#- Case 1:  assume delta= 3, sigma=2 --> n=14

Delta = 3
sigma = 2
r = 4 
power.anova.test(groups = r,  between.var =  Delta^2/(2*(r-1)), within.var = sigma^2, power = 0.9)


#- Case 2:  assume the effect size : relative effect= Delta/sigma= k=2 ==> n=9

k=2
r = 4 

## calculate the sample size  
power.anova.test(groups = r,  between.var = k^2/(2*(r-1)), within.var = 1, power = 0.9)


########## Homework data: 9/5/2019 ################################# 

# HW 16.07
HW07 <- read.table(url("https://raw.githubusercontent.com/npmldabook/Stat3119/master/Week2/CH16PR07.txt")) # read directly
head(HW07) 

# rename the variables
names(HW07)<- c("productivity", "expenditures", "firm")

HW07$expenditures<- as.factor(HW07$expenditures) 

str(HW07) 
head(HW07) 
# now you have the data, you can plot it and solve the questions


# HW 16.10
HW10 <- read.table(url("https://raw.githubusercontent.com/npmldabook/Stat3119/master/Week2/CH16PR10.txt")) # read directly
dim(HW10)
head(HW10) 

# rename the variables
names(HW10)<- c("offer", "age", "dealer")

HW10$age<- as.factor(HW10$age) 

str(HW10) 
head(HW10) 

# now you have the data, you can plot it and solve the questions



