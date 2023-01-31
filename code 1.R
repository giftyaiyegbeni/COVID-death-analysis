
# set the working directory

setwd(dirname(file.choose()))
getwd()


#import the necessary libraries
library(Hmisc)
library(naniar)
library(dplyr)
library(visdat)
library(corrgram)
library(corrplot)
library(psych)
library(ppcor)
library(nFactors)
library(GPArotation)
library(cluster)
library(fpc)
library(car)
library(relaimpo)
library(RcmdrMisc)
library(BSDA)
library(dplyr)
library(ggplot2)

# read in data from csv file
data <- read.csv("dataset.csv", stringsAsFactors = FALSE)

class(data)

# Confirm the column names of the data
colnames(data)

# View top rows of the data
head(data)   
glimpse(data)
str(data)

# attach the data for easy usage

attach(data)

# measures of central tendency
summary(data)

describe(data)


# check for missing data
sum(is.na(data))

vis_miss(data)
vis_dat(data)


#create a dataframe of relevant variables

df = data.frame(District, Total_deaths, Total_pop, White, Mixed,            
        Asian,  Black,Other_Ethnicity, Good,  Fair,             
        Bad,  Christian,Buddhist, Hindu, Muslim,           
        Other_religion, Noreligion, SocialGrade_Total, Upper, Upper_middle,     
        Middle, Lower  )

str(df)

# Checking for outliers
boxplot(Total_deaths, col = 'blue', Xlab='Total COVID deaths', ylab= 'Count')

#Label Outliers

boxdf <- boxplot(Total_deaths, col = 'blue', Xlab='Total COVID deaths', ylab= 'Count')

#for each outlier in boXdf
for(i in 1:length(Total_deaths)){
  #add teXt to the boXplot
  text(boxdf$group[i], boxdf$out[i],
       which(Total_deaths==boxdf$out[i]),pos=4, ceX=1)}
  
# inspect outliers
df[19,]
df[146,]
df[66,]
  
# Test for normality in the dependent variable

# Kolmogorov-Smirnov Tests of normality
ks.test(Total_deaths,"pnorm", mean(Total_deaths), sd(Total_deaths))

# an alternative is the Shapiro-Wilk's test
shapiro.test(Total_deaths)

# QQ plots to test for normality

qqnorm(Total_deaths, Xlab = "Total COVID death", col= "red" )
qqline(Total_deaths, col= "blue") ## red color


# Calculate percentage of population variables
df <- within (df, pTotal_deaths <- (Total_deaths / Total_pop)*100)
df <- within (df, pWhite <- (White / Total_pop)*100)
df <- within (df, pMixed <- (Mixed / Total_pop)*100)
df <- within (df, pAsian <- (Asian / Total_pop)*100)
df <- within (df, pBlack <- (Black / Total_pop)*100)
df <- within (df, pOther_Ethnicity <- (Other_Ethnicity / Total_pop)*100)
df <- within (df, pGood <- (Good / Total_pop)*100)
df <- within (df, pFair <- (Fair / Total_pop)*100)
df <- within (df, pBad <- (Bad / Total_pop)*100)
df <- within (df, pChristian <- (Christian / Total_pop)*100)
df <- within (df, pBuddhist <- (Buddhist / Total_pop)*100)
df <- within (df, pHindu <- (Hindu / Total_pop)*100)
df <- within (df, pMuslim <- (Muslim / Total_pop)*100)
df <- within (df, pOther_religion <- (Other_religion / Total_pop)*100)
df <- within (df, pNoreligion <- (Noreligion / Total_pop)*100)
df <- within (df, pUpper <- (Upper / SocialGrade_Total)*100)
df <- within (df, pUpper_middle <- (Upper_middle / SocialGrade_Total)*100)
df <- within (df, pMiddle <- (Middle / SocialGrade_Total)*100)
df <- within (df, pLower <- (Lower / SocialGrade_Total)*100)

detach(df)

attach(df)

# Create a boxplot of all independent count variables
boxplot( Total_deaths, White, Mixed,            
        Asian,  Black,Other_Ethnicity, Good,  Fair,             
        Bad,  Christian,Buddhist, Hindu, Muslim,           
        Other_religion, Noreligion, Upper, Upper_middle,     
        Middle, Lower,
        names=c("Total_deaths", "White", "Mixed",            
                "Asian",  "Black","Other_Ethnicity", "Good",  "Fair",             
                "Bad",  "Christian", "Buddhist", "Hindu", "Muslim",           
                "Other_religion", "Noreligion", "Upper", "Upper_middle",     
                "Middle", "Lower" ),
        xlab="Variable", ylab="Count", col = "Bisque")



# Create a boxplot of all independent percentage variables
boxplot(pTotal_deaths, pWhite, pMixed,            
        pAsian,  pBlack, pOther_Ethnicity, pGood,  pFair,             
        pBad,  pChristian, pBuddhist, pHindu, pMuslim,           
        pOther_religion, pNoreligion, pUpper, pUpper_middle,     
        pMiddle, pLower,
        names=c("pTotal_deaths", "pWhite", "pMixed",            
                "pAsian",  "Black","pOther_Ethnicity", "pGood",  "pFair",             
                "pBad",  "pChristian", "pBuddhist", "pHindu", "pMuslim",           
                "pOther_religion", "pNoreligion", "pUpper", "pUpper_middle",     
                "pMiddle", "pLower" ),
                  xlab="Variable", ylab="Percentage", col = "Bisque")


# Test for normality in the percentage of the dependent variable

boxplot(pTotal_deaths, col = 'blue', Xlab='Total COVID deaths', ylab= 'Count')


# Kolmogorov-Smirnov Tests of normality

ks.test(pTotal_deaths,"pnorm", mean(pTotal_deaths), sd(pTotal_deaths))

# an alternative is the Shapiro-Wilk's test
shapiro.test(pTotal_deaths)

# QQ plots to test for normality of dependent variable

qqnorm(pTotal_deaths, Xlab = "Total COVID death", col= "red" )
qqline(pTotal_deaths, col= "blue") 


# QQ plots to test for normality of independent variables

qqnorm(pWhite, Xlab = "White population", col= "red" )
qqline(pWhite, col= "blue") 

qqnorm(pBlack, Xlab = "Black population", col= "red" )
qqline(pTotal_deaths, col= "blue") 

qqnorm(pUpper, Xlab = "Top Social status", col= "red" )
qqline(pUpper, col= "blue") 

qqnorm(pLower, Xlab = "Lower Social status", col= "red" )
qqline(pLower, col= "blue") 


qqnorm(pGood, Xlab = "Good Health Status", col= "red" )
qqline(pGood, col= "blue") 

qqnorm(pBad, Xlab = "Bad Health Status", col= "red" )
qqline(pBad, col= "blue") 

qqnorm(pChristian, Xlab = "Christain Religion", col= "red" )
qqline(pChristian, col= "blue") 

qqnorm(pBuddhist, Xlab = "Buddhists Religion", col= "red" )
qqline(pBuddhist, col= "blue") 



# Multivariate scatterplot matrix
pairs(~ pTotal_deaths+ pWhite+ pMixed+            
      pAsian+  pBlack+ pOther_Ethnicity+ pGood+  pFair+             
      pBad, pch = 19, data = df,
      main = "multivariate scatterplot matrix of dependent and ethnicity and health", 
      lower.panel = panel.smooth,upper.panel = panel.smooth, cex = 0.5)

pairs(~ pTotal_deaths + pChristian+ pBuddhist+ pHindu+ pMuslim+           
        pOther_religion+ pNoreligion+ pUpper+ pUpper_middle+     
        pMiddle+ pLower, pch = 19, data = df,
      main = "multivariate scatterplot matrix of dependent and religion and socail grade", 
      lower.panel = panel.smooth,upper.panel = panel.smooth, cex = 0.5)

# Correlation matrix
df1 <- data.frame(pTotal_deaths, pWhite, pMixed,            
                pAsian,  pBlack, pOther_Ethnicity, pGood,  pFair,             
                pBad,  pChristian, pBuddhist, pHindu, pMuslim,           
                pOther_religion, pNoreligion, pUpper, pUpper_middle,     
                pMiddle, pLower)

cor(df1, method = "spearman")
df1_cor <- cor(df1, method = "spearman")
round(df1_cor, digits = 2)
cor.df1 <- as.data.frame(df1_cor)
View(cor.df1)

#remove variables that have almost no correlation with COVID deaths

myvar <- names(df1) %in% c("pMixed")
df1 <- df1[!myvar]
str(df1)
rm(myvar)


#Corr Plot
corrplot(corr = cor(df1))

#Correlelogram

corrgram(df1, order=FALSE, cor.method = "pearson", lower.panel=panel.conf,
         upper.panel=panel.pie, text.panel=panel.txt, main="COVID Deaths Vs Independent Variables")


# test correlation of dependent variable with all independent variables
cor.test(pTotal_deaths, pWhite, method = "spearman")
cor.test(pTotal_deaths, pAsian, method = "spearman")
cor.test(pTotal_deaths, pGood, method = "spearman")
cor.test(pTotal_deaths, pFair, method = "spearman")
cor.test(pTotal_deaths, pBad, method = "spearman")
cor.test(pTotal_deaths, pChristian, method = "spearman")
cor.test(pTotal_deaths, pBuddhist, method = "spearman")
cor.test(pTotal_deaths, pHindu, method = "spearman")
cor.test(pTotal_deaths, pMuslim, method = "spearman")
cor.test(pTotal_deaths, pUpper, method = "spearman")
cor.test(pTotal_deaths, pMiddle, method = "spearman")
cor.test(pTotal_deaths, pLower, method = "spearman")



# Internal correlations between the variables
cor.test(pOther_Ethnicity, pWhite, method = "spearman")
cor.test(pBlack, pAsian, method = "spearman")
cor.test( pGood, pFair, method = "spearman")
cor.test( pFair,pBad, method = "spearman")
cor.test(pChristian, pHindu, method = "spearman")
cor.test(pMuslim, pBuddhist, method = "spearman")
cor.test(pOther_religion, pNoreligion, method = "spearman")
cor.test(pUpper, pUpper_middle, method = "spearman")
cor.test(pMiddle,pLower, method = "spearman")

#partial correlation

#calculate partial correlation using Spearman and Pearson
pcor.test(pTotal_deaths, pOther_Ethnicity, pWhite, method = "spearman")
pcor.test(pTotal_deaths, pBlack, pAsian, method = "spearman")
pcor.test(pTotal_deaths, pGood, pFair, method = "spearman")
pcor.test(pTotal_deaths, pFair, pBad, method = "spearman")
pcor.test(pTotal_deaths, pChristian, pHindu)
pcor.test(pTotal_deaths, pMuslim, pBuddhist)
pcor.test(pTotal_deaths, pOther_religion, pNoreligion)
pcor.test(pTotal_deaths, pUpper, pUpper_middle)
pcor.test(pTotal_deaths, pMiddle,pLower)

#Hypothesis Testing
#Difference between COVID deaths in 2021 and 2020
colnames(data)

data <- within (data, Q1_2021 <- January_2021 + February_2021 + March_2021 + April_2021)

data <- within (data, Q4_2020 <- September_2020 + October_2020 + November_2020 + December_2020 )

#Checked for the Standard Deviation

sd(data$Q1_2021)
sd(data$Q4_2020)

#Z test to check if there is a difference between Q4 2020 and Q1 2021 deaths

z.test(data$Q1_2021,data$Q4_2020, mu=0, sigma.y=76.84573, sigma.x=100.464, conf.level = 0.95)

#Creating a categorical variable

#Check for the mean of the total covid deaths

summary(df$pTotal_deaths)

#Create a categorical variable for above average mean (High) and Below average mean (Low)

df$Average_death = ifelse(df$pTotal_deaths > 0.2170, "High", "Low")

table = table(df$Average_death)
table

#Chi squared test to check for relationship

chisq.test(table)


# simple chi-squared with additional outputs
output <- chisq.test(table)
output
output$observed
output$expected
output$stdres

# calculate effect size
CramerV(table, conf.level = 0.95)

# Kaiser-Meyer-Olkin statistics: if overall MSA > 0.5, proceed to factor analysis
#Create a new dataframe with the required independent variables

IV <- data.frame(pWhite,pAsian, pBlack, pOther_Ethnicity, pGood,  pFair,            
                  pBad,  pChristian, pBuddhist, pHindu, pMuslim,           
                  pOther_religion,  pNoreligion, pUpper, pUpper_middle,  
                  pMiddle, pLower)
KMO(cor(IV))



# get eigenvalues
ev <- eigen(cor(IV))
ev$values
round(ev$values, 3)

# plot a scree plot of eigenvalues
plot(ev$values, type="b", col="orange", xlab="Independent variables")

# calculate cumulative proportion of eigenvalue and plot
ev.sum<-0
for(i in 1:length(ev$value)){
  ev.sum<-ev.sum+ev$value[i]
}
ev.list1<-1:length(ev$value)
for(i in 1:length(ev$value)){
  ev.list1[i]=ev$value[i]/ev.sum
}
ev.list2<-1:length(ev$value)
ev.list2[1]<-ev.list1[1]
for(i in 2:length(ev$value)){
  ev.list2[i]=ev.list2[i-1]+ev.list1[i]
}
plot (ev.list2, type="b", col="Red", xlab="number of components", ylab ="cumulative proportion")


# Varimax Rotated Principal Components

fit <- principal(IV, nfactors=4, rotate="varimax")
fit

# weed out variables with high communalities

myvars <- names(IV) %in% c("pChristian", "pWhite", "pBuddhist", "pMuslim")
IV <- IV[!myvars]
str(IV)
rm(myvars)


# get eigenvalues
ev <- eigen(cor(IV))
ev$values
round(ev$values, 3)
# plot a scree plot of eigenvalues
plot(ev$values, type="b", col="blue", xlab="variables")

# calculate cumulative proportion of eigenvalue and plot
ev.sum<-0
for(i in 1:length(ev$value)){
  ev.sum<-ev.sum+ev$value[i]
}
ev.list1<-1:length(ev$value)
for(i in 1:length(ev$value)){
  ev.list1[i]=ev$value[i]/ev.sum
}
ev.list2<-1:length(ev$value)
ev.list2[1]<-ev.list1[1]
for(i in 2:length(ev$value)){
  ev.list2[i]=ev.list2[i-1]+ev.list1[i]
}
plot (ev.list2, type="b", col="red", xlab="number of components", ylab ="cumulative proportion")

# Varimax Rotated Principal Components
# retaining 'nFactors' components
fit <- principal(IV, nfactors=4, rotate="varimax")
fit



# Select the variables after factor analysis for use in clustering

myvars <- c("pGood", "pHindu","pOther_Ethnicity", "pNoreligion")
IV <- df1[myvars]
str(IV)
rm(myvars)

# Preparing thee Data
# listwise deletion of missing values
IV <- na.omit(IV)

# visualising the variables
boxplot(IV) 

# scaling between 0-1
IV <- apply(IV, MARGIN = 2, FUN = function(X) (X - min(X))/diff(range(X)))
summary(IV)

# visualising the variables after scaling
boxplot(IV, col = "red") 


#CLUSTERING

# Ward Hierarchical Clustering

dm <- dist(IV, method = "euclidean") 

set.seed(12345)
fit <- hclust(dm, method="ward.D")

# display dendogram

plot (fit, labels = data$District)

# plotting a dendogram with green borders around the 4 clusters

groups <- cutree(fit, k=4) # cut tree into 4 clusters
rect.hclust(fit, k=4, border="green")

# assign cluster number to district name

mydata <- data.frame(data$Code)
names(mydata)[1] <- "Code"
mydata <- within (mydata, Name <- data$District)
mydata <- within (mydata, cluster <- groups)

mydata
write.csv(mydata,file.choose())


# K-Means Cluster Analysis
set.seed(12345)

# 4 cluster solution
fit <- kmeans(IV, 4)

#visualise clusters

# from library(cluster)
clusplot(IV, fit$cluster)   

# from library(fpc)
plotcluster(IV, fit$cluster)    

# get cluster means
aggregate(IV, by=list(fit$cluster),FUN = mean)

# append cluster assignment
mydata1 <- data.frame(data$Code)
names(mydata1)[1] <- "Code"
mydata1 <- within (mydata1, Name <- data$District)
mydata1 <- data.frame(mydata1, fit$cluster)

mydata1
write.csv(mydata1,file.choose())



# LINEAR REGRESSION

# Multiple Regression

# model with all variables

model1 <- lm(pTotal_deaths~ pWhite+ pBlack + 
               Other_Ethnicity +  pAsian+ pGood+  pFair+ pBad+             
               pChristian+ pBuddhist+ pHindu+ pMuslim+  pOther_religion + Noreligion+           
               pUpper+ pMiddle + pUpper_middle + pLower)

summary(model1)

#Remove the perfectly correlating variable

model1 <- lm(pTotal_deaths~ pWhite+ pBlack +  pOther_religion +
             Other_Ethnicity +  pAsian+ pGood+  pFair+             
               pChristian+ pBuddhist+ pHindu+ pMuslim+           
               pUpper+ pMiddle + pUpper_middle)

summary(model1)

# calculate variance inflation factor if > 2 the vif is too high
vif(model1)
sqrt(vif(model1)) > 2  

# model with four variables representing components from factor analysis
cor1 <- cor(df1[, c(1,5,6,11, 14)], method = "spearman")
round(cor1, 2)
corrplot(cor1, type = "upper", tl.col = "black", tl.srt = 45)

model2 <- lm(pTotal_deaths~ pHindu + pGood + pOther_Ethnicity + pNoreligion)

summary(model2)
sqrt(vif(model2)) > 2

# relative importance of variables

calc.relimp(model2, type = c("lmg"), rela = TRUE)

# test whether model1 and model2 are significantly different using F test
anova(model1, model2, test = "F")


# use a stepwise approach to search for a best model
# forward stepwise selection on model 1
model3 <- stepwise(model1, direction = "forward")
summary(model3)


hist(model3$residuals)
rug(model3$residuals)
plot(model3$residuals ~ model3$fitted.values, xlab = "fitted values", ylab = "residuals")
ks.test(model3$residuals, "pnorm", mean(model3$residuals), sd(model3$residuals))

#Internal correlation

sqrt(vif(model3)) > 2
calc.relimp(model3, type = c("lmg"), rela = TRUE)


# calculating the Odds Ratio
exp(coef(model3))


# test if model1 and model3 are significantly different using F test
anova(model1, model3, test = "F")

detach(df)

