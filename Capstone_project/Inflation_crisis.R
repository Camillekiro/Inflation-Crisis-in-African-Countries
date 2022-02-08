library(readr)
library(stats)
library(psych)
library(regclass)
library(fastDummies)
library(caTools)
library(glm2)
library(tidyverse)
library(car)



african_crises <- read_csv("~/Downloads/african_crises.csv")

#looking for missing values
sapply(african_crises, function(x) sum(is.na(x)))

describe(african_crises)

#removing unnecessary variables

african_crises[,1:3]<-list(NULL)
african_crises$independence<-NULL

#removed the inflation_annual_cpi variable because it was causing a perfect prediction warning
african_crises$inflation_annual_cpi<-NULL

#altering the banking_crisis variable to a binary variable with values 0/1, 1 for crisis, 0 for no crisis

african_crises$banking_crisis <- as.factor(ifelse(african_crises$banking_crisis=='crisis',1,0))

#The currency crises variable is a binary variable with values 0/1, there were some observations with 2 as values, I removed these
african_crises<-african_crises[!(african_crises$currency_crises==2),]

#checking for outliers in the data using boxplots
boxplot(african_crises)

#removing the extreme values found in the exch_usd variable
african_crises<-african_crises[!(african_crises$exch_usd > 700),]

#univariate plots
ggplot(african_crises, aes(y=inflation_crises)) + geom_bar()
ggplot(african_crises, aes(x= exch_usd)) + geom_boxplot()
ggplot(african_crises, aes(x= sovereign_external_debt_default)) + geom_bar()
ggplot(african_crises, aes(x= currency_crises)) + geom_bar()
ggplot(african_crises, aes(x= banking_crisis)) + geom_bar()

#bivariate plots
ggplot(data = african_crises, aes(x = exch_usd, y = inflation_crises)) +
  geom_boxplot (stat = "boxplot") + geom_jitter(width = 0.2)
ggplot(african_crises) +
  geom_bar(aes(x = sovereign_external_debt_default, fill = inflation_crises), position = "dodge")
ggplot(african_crises) +
  geom_bar(aes(x = currency_crises, fill = inflation_crises), position = "dodge")
ggplot(african_crises) +
  geom_bar(aes(x = banking_crisis, fill = inflation_crises), position = "dodge")

#summary statistics 
describe(african_crises)

#reviewing data types
sapply(african_crises, class)

#transforming categorical variables to factors
african_crises$inflation_crises <- as.factor(african_crises$inflation_crises)
african_crises$systemic_crisis <- as.factor(african_crises$systemic_crisis)
african_crises$domestic_debt_in_default <- as.factor(african_crises$domestic_debt_in_default)
african_crises$sovereign_external_debt_default <- as.factor(african_crises$sovereign_external_debt_default)
african_crises$currency_crises <- as.factor(african_crises$currency_crises)


#splitting the data into a training/testing set
split = sample.split(african_crises$inflation_crises, SplitRatio = 0.8)

train_set = subset(african_crises, split == TRUE)
test_set = subset(african_crises, split == FALSE)

view(train_set)

Model <- glm(inflation_crises ~.,data = train_set, family = binomial(link = "logit"))
print(summary(Model))

Model2 <- glm(inflation_crises ~ exch_usd + sovereign_external_debt_default + currency_crises
              + banking_crisis,data = train_set, family = binomial())
print(summary(Model2))

#testing the multi-collinearity assumption
VIF(Model2)

#testing the fourth assumption of the logistic regression
#the numeric variable is exch_usd

probs <- predict(Model2, type = "response")
logit = log(probs/(1-probs))

ggplot(train_set, aes(logit, exch_usd))+
  geom_point(size = 0.5, alpha = 0.5) +
  geom_smooth(method = "loess") +
  theme_bw()


#accuracy scores for both models

fitted.results <- predict(Model, newdata = test_set, type = 'response')
fitted.results <- ifelse(fitted.results > 0.5,1,0)
error <- mean(fitted.results != test_set$inflation_crises)
print(paste('Logistic Regression Accuracy score for Model', 1-error))

fitted.resultslg2 <- predict(Model2, newdata = test_set, type = 'response')
fitted.resultslg2 <- ifelse(fitted.resultslg2 > 0.5,1,0)
error <- mean(fitted.resultslg2 != test_set$inflation_crises)
print(paste('Logistic Regression Accuracy score for Model2', 1-error))

#Confusing matrix
print("Confusion matrix_Model"); table(Predicted = fitted.results, Actual = test_set$inflation_crises)
print("Confusion matrix_Model2"); table(Predicted = fitted.resultslg2, Actual = test_set$inflation_crises)

#provides the coefficients of the variables in the final model
coef(Model2)

#final data export, this resulted in an extra column counting the rows, I then did some excel formatting
#to submit the final data used 
write.table(african_crises, file= "african_crises_final.csv", sep = ",")

