#to check new csv against old

library(dplyr)

co4_check = read.csv("Cohort4.csv")
co5_check = read.csv("Cohort5.csv")
co7_check = read.csv("Cohort_7.csv")

#Admissions data
co4 = read.csv("Cohort4.csv")
co5 = read.csv("Cohort5.csv")
co7 <- read.csv("Cohort_7.csv")


drops <- c("College.Decision", "College.Decision.Full", "Advisor", "Intent.Summary", "Intent.Response")
co7 <- co7[ , !(names(co7) %in% drops)]
#Match column names
colnames(co7)[1] <- "Status"
colnames(co7)[6] <- "GRE.Verbal"
colnames(co7)[7] <- "GRE.Math"


head(co4)
head(co5)
head(co7)

str(co4)
str(co5)
str(co7)

#Denied = denied by school
#Enrolled - Packet Made = accepted offer
#offer declined = ind. declined school offer
#offer cards made - ind. declined school offer (never formally declined)
#recieved - ind. declined school offer

#add a column indicating the cohort previous to this step for other analyses

co_total <- rbind(co5, co4, co7)
table(co_total$Status)
table(co_total$Scholarship)


co_total$Scholarship <- as.character(co_total$Scholarship)
co_total$Scholarship[co_total$Scholarship=="X"] <- "5"
co_total$Scholarship <- as.numeric(co_total$Scholarship)
co_total$Scholarship[is.na(co_total$Scholarship)] <- 0

glimpse(co_total)

co_total = co_total[ which( ! co_total$Status %in% "Denied") , ]
table(co_total$Status)
table(co_total$Scholarship)
#Group enrolled and non-enrolled

table(co_total$Status)

co_total$Status <- as.character(co_total$Status)
co_total$Status = ifelse(co_total$Status=="Offer Declined", 0, 1)

table(co_total$Status)

#Cohort5 enrolled + registered ==1, others ==0
'''   #####Check with alan to see if this code can be ommited######
co5$Status
str(co5$Status)
co5$Status <- as.character(co5$Status)
co5$Status[co5$Status == "Enrolled Packet Made"] = 1
co5$Status[co5$Status == "Offered - Cards Made"] = 1
co5$Status
co5$Status[co5$Status != 1] = 0

table(co7$Status)
str(co7$Status)
co7$Status <- as.character(co7$Status)
co7$Status[co7$Status == "Enrolled Packet Made"] = 1
co7$Status[co7$Status == "Registered"] = 1
co7$Status[co7$Status != 1] = 0
co7
co7_check

'''

# drop the zip code column
zip = "Local.zip"
co_total = co_total[, !(names(co_total) %in% zip)]
co_total

#change WA to instate

co_total$Local.state = as.character(co_total$Local.state)
str(co_total)
co_total$Local.state[co_total$Local.state == "WA"] = "in_state" 
co_total$Local.state

#change rest to outstate

co_total$Local.state[co_total$Local.state != "in_state"] = "out_state" 
co_total$Local.state

# Where outstate != Unitedstate change to intl

co_total$Local.country = as.character(co_total$Local.country)
co_total$Local.country[co_total$Local.country != "UNITED STATES"] = "intl"
co_total$Local.country

co_total


#first model

#install.packages("tree")
library("tree")
co_total.tree <- tree(Status~., data = co_total)

#NA's were coerced

plot(co_total.tree)
text(co_total.tree, cex = .5)

#Display split criterion, number of obs in a branch, deviance, and overall prediction of branch
co_total.tree

#June 22nd
#install.packages("caret")
library(caret)
set.seed(2)
na_count <-sapply(co_total, function(y) sum(length(which(is.na(y)))))
na_count
complete <- complete.cases(co_total)
base_total <- co_total[complete,]
str(base_total)
str(co_total)
table(co_total$Status)
head(base_total)
head(co_total)
base_total$Status <-as.factor(base_total$Status)

levels(base_total$Status) <- c("N","Y")

control <- trainControl(method = "repeatedcv", 
                        repeats = 5, 
                        classProbs = TRUE)


set.seed(2)
logisticReg <- train(Status~.,
                     data=base_total,
                     method = "glm", 
                     trControl=control)
logisticReg
logisticReg$finalModel$coefficients
varImp(logisticReg)

set.seed(2)
svmFit <- train(Status ~ ., 
                data=base_total,
                method = "svmRadial", 
                preProc = c("center", "scale"), 
                tuneLength = 10, 
                trControl = control)
svmFit
plot(svmFit, scales = list(x = list(log = 2))) 
plot(svmFit)

set.seed(2)

gbmFit <- train(Status ~ .,
                data=base_total,
                method = "gbm",
                trControl = control,
                verbose = FALSE)   
gbmFit

resamp <- resamples(list(SVM = svmFit, Logistic = logisticReg, GBM = gbmFit))
summary(resamp)



modelDifferences <- diff(resamp)
summary(modelDifferences)
dotplot(modelDifferences)

test_0 <- base4_5[,-5]
test_0$Scholarship <- 0
test_5 <- base4_5[,-5]
test_5$Scholarship <- 5
test_10 <- base4_5[,-5]
test_10$Scholarship <- 10

str(test_0)

predict_0 <- predict(logisticReg, newdata = test_0, type = "prob")
predict_0 <- predict_0[,-1]
str(predict_0)

predict_5 <- predict(logisticReg, newdata = test_5, type = "prob")
predict_5 <- predict_5[,-1]
str(predict_5)

predict_10 <- predict(logisticReg, newdata = test_10, type = "prob")
predict_10 <- predict_10[,-1]
str(predict_10)

predict_log <- as.data.frame(cbind(predict_0, predict_5, predict_10))
str(predict_log)
View(predict_log)



#SVM predict
predict_0_svm <- predict(svmFit, newdata = test_0, type = "prob")
predict_0_svm <- predict_0_svm[,-1]


predict_5_svm <- predict(svmFit, newdata = test_5, type = "prob")
predict_5_svm <- predict_5_svm[,-1]


predict_10_svm <- predict(svmFit, newdata = test_10, type = "prob")
predict_10_svm <- predict_10_svm[,-1]

predict_svm <- as.data.frame(cbind(predict_0_svm, predict_5_svm, predict_10_svm))


View(predict_svm)


#combined
log_svm_compare <- as.data.frame(cbind(predict_log, predict_svm))
View(log_svm_compare)

library("dplyr")

log_svm_compare2 <- log_svm_compare %>%
  mutate(diff_0 = predict_0_svm - predict_0) %>%
  mutate(diff_5 = predict_5_svm - predict_5) %>%
  mutate(diff_10 = predict_10_svm - predict_10)


View(log_svm_compare2)

