

# Load the raw training data and replace missing values with NA
donations<- read.csv('donations.csv',header=T,na.strings=c(""))
donations <- donations[,-c(1,18)]

# Output the number of missing values for each column
sapply(donations,function(x) sum(is.na(x)))

# Quick check for how many different values for each feature
sapply(donations, function(x) length(unique(x)))
  
# R should automatically code Gifts, Gender as a factor(). A factor is R's way of dealing with
# categorical variables
is.factor(donations$Gender)         # Returns TRUE
is.factor(donations$Gifts)    # Returns False
donations$Gifts <- factor(x = donations$Gifts, levels = c(0,1), labels = c("No", "Yes"))
is.factor(donations$Gifts)    # Returns TRUE


# Check categorical variables encoding for better understanding of the fitted model
contrasts(donations$Gender)


for (i in c(3, 15, 16)) {
  donations[[i]] <- gsub(pattern = "\\$", replacement = "", x = donations[[i]])
  donations[[i]] <- as.numeric(donations[[i]]) }
donations$Gifts <- ifelse(donations$Gifts == "Yes", 1,0)
donations$Largest <- as.numeric(donations$Largest)

donations$Last <- as.numeric(donations$Last)
donations$St <- as.character(donations$St)
donations$St <- ifelse(donations$St =="IL",1,0)
donations$Type <- as.character(donations$Type)
donations$Type <- ifelse(donations$Type == "Graduate", 1, 0)
donations$LGDate<-as.numeric(donations$LGDate)
donations$Class <- as.numeric(donations$Class)
donations$Class1 <- 2015- donations$Class
donations$FY1314 <- donations$FY13*donations$FY14
donations$log13 <- log(donations$FY13 + 1)
donations$log14 <- log(donations$FY14 + 1)
donations$logLast <- log(donations$Last + 1)
Lg<-as.numeric(donations$LGDate)
Cu<-as.Date("15Sep2015", "%d%b%Y")
donations$newdate=Cu-Lg
donations$newdate <- as.numeric(donations$newdate)
currentdate <- as.Date("15Sep2015", "%d%b%Y")
donations$TG <- as.character(donations$TestGroup1)
donations$TestGroup1B <- ifelse(donations$TG == "B", 1, 0)
donations$TestGroup1C <- ifelse(donations$TG == "C", 1, 0)
donations$TestGroup1D <- ifelse(donations$TG == "D", 1, 0)


# if you do any data transformation do it before generating train and test

# Spliting the Data (Test & Train)
set.seed(1234)
Partition <- rbinom(n = nrow(donations) ,size = 1, prob = 0.7)
donations <- data.frame(donations, Partition)
train <- donations[donations$Partition==1, ]
test <- donations[donations$Partition==0, ]


# logistic model
fit.logit <- glm(data = train, formula = Gifts ~ logLast+FY1314+log14+log13+newdate, family = binomial(),na.action = na.exclude)
summary(fit.logit)
# test the model
prob <- predict.glm(fit.logit, test, type = "response")
gifts <- as.numeric(test$Gifts)
mean(gifts*log(prob) + (1 - gifts)*log(1-prob))

# regression model
train2 <- train[train$CnGf_1_Amount > 0, ]
test2 <- test[test$CnGf_1_Amount > 0, ]
fit.reg <- lm(data = train2, formula = CnGf_1_Amount ~ Last+log14+log13+Class1+Left+newdate)
summary(fit.reg)
# test the model
pred <- predict.lm(object = fit.reg, newdata = test2)
mean(abs(pred-test2$CnGf_1_Amount))

# test the logit model
#attach(donations)
donations$Gifts <- as.numeric(donations$Gifts)
donations$Gender <- as.numeric(donations$Gender)
prob = .335
donations$Baselinell <- (donations$Gifts*log(prob) + (1 - donations$Gifts)*log(1-prob))



summary(donations)


# # test the regression model
test <- donations[donations$CnGf_1_Amount > 0, ]

test$Baseline <-  (abs( test$CnGf_1_Amount -test$Last))

summary(test)

# model
# Logistic Model
## Gift = 1.049e+01 +(-1.234e-01)logLast+(-1.678e-05)FY1314 + 5.006e-01log14 + 3.414e-01log13 + (-7.925e-04)newdate
# # Regression
# Donation Amount= 129.010335 + 0.906707Last + (-0.334862)log14 + 0.746060log13 + (-0.130387)Class1 + 0.020227Left + (-0.007011)newdate