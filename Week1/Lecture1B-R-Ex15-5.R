# title: "STAT 3119 ANOVA"
# date: "08/29/2019 @GWU"
# 
# ## A walkthrough for the example using R in Chapter 15.5
#
# -  **Step 1**. We read the data into R. You can go to [**stat3119/Week1
# folder**](https://github.com/npmldabook/Stat3119/tree/master/Week1)  and click
# the data file "CH15TA01.txt" to see its content. Then you click the "raw" Tab
# to open the data file in a browser such as Chrome. From that window, you can
# right-click the mouse to save it to your local computer. If you have the data
# in your local folder, say, "c:/stat3119/CH15TA01.txt", then you can read it
# from you local directory into R in 2 ways: (1) use
# **read.table(file.choose())**, then you choose where the file is (user input).
# (2) use **read.table("c:/stat3119/CH15TA01.txt")**. Note, you have to replace
# the 'c:/stat3119/CH15TA01.txt' with the actual file path in your computer. Or,
# in the third way, you can read it directly from the class website.


#Ex15 <- read.table(file.choose(), header=F) # find the file location from file browser
#Ex15 <- read.table("c:/stat3119/CH15TA01.txt")      # specify where the file is in your local computer


Ex15 <- read.table(url("https://raw.githubusercontent.com/npmldabook/Stat3119/master/Week1/CH15TA01.txt")) # read directly

# check the data. This data has no headers or variable name.
dim(Ex15)
head(Ex15)


# -  **Step 2**. The raw data file has no variable names. We rename the
# variables and check data again. We will csee the same data as in the Table
# 15.1.

names(Ex15) <- c("subject", "Control","Exp", "Dif")
head(Ex15)


# -  **Step 3**.  Run the statistical tests for paried comparison. Here, we run
# paired comparison for control and experimental groups with a paired t-test.
# The results match the textbook (p. 671).
#
# The dermatologists were primarily interested in determining whether the
# experimental allergen formulation led to difference in skin sensitivity, i.e.,
# testing $H_0:  \beta_1 =0 \quad  \text{vs.}  \quad  H_1:  \beta_1 \ne 0$.
#
# We don't have to use MINITAB to run the analysis. This is simply a paired
# t-test that can be done in R with a line of code.


t.test(Ex15$Exp, Ex15$Control, paired=T )

# **Step 4**. In the paired t-test, we are only interested in the treatment
# effect, not the subject difference.  However, if the investigators were not
# primarily interested in determining whether or not subject (block) effects
# were present. Here, blocking was used here to increase the precision of the
# comparisons between the experimental and control treatments and it was fully
# expected that significant subject-to-subject differences would be present. We
# can use ANOVA table to differentiate the source of variability, difference due
# to the treatment or the subjects (study participants). (*Don't worry if this
# is not clear here, we will go over the model and details in later chapters*).
# Basically, we need to transform this data to specify the factors (treatment
# and subject) and outcome $Y$, then we run the ANOVA to get the test reesults.

# Data transformation 
Ex15v2 <- data.frame( Treatment=c(rep(0,20), rep(1,20)), Subject=factor(c(1:20, 1:20)),
                     Y= c( Ex15$Control ,Ex15$Exp))
head(Ex15v2)
summary(aov(Y~  Treatment+Subject, data=Ex15v2 ))



