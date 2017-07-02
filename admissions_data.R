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

#Create a column to identify each cohort, then bind into original dataframe

list_co4 <- rep('4',length(co4_check$Scholarship))
list_co5 <- rep('5',length(co5_check$Scholarship))
list_co7 <- rep('7',length(co7_check$Scholarship))

co4 = cbind(co4, list_co4)
colnames(co4)[8] = "cohort"
co5 = cbind(co5, list_co5)
colnames(co5)[8] = "cohort"
co7 = cbind(co7, list_co7)
colnames(co7)[8] = "cohort"

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
str(co_total)

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

str(co_total)


#first model

#install.packages("tree")
library("tree")

table(co_total$Status)

decision=ifelse(co_total$Status<=0,"No","Yes")
table(decision)
co_total = data.frame(co_total, decision)
co_total.tree <- tree(decision~. -Status, data = co_total)

#NA's were coerced

plot(co_total.tree)
text(co_total.tree, cex = .5, pretty = 0)

#residual mean deviance, misclassification error rate

summary(co_total.tree)

#Display split criterion, 
#number of obs in a branch, deviance, 
#and overall prediction of branch
co_total.tree

set.seed (2)
train=sample(1:nrow(co_total), 100)
co_total.test=co_total[-train ,]
decision.test=decision[-train]

###Find Accuracy from table below = (true negatives + true positives) / total

co_total.tree=tree(decision~. -Status,co_total,subset=train)
tree.pred=predict(co_total.tree,co_total.test,type="class")
table(tree.pred ,decision.test)

#tree trimming and cross validation

set.seed (3)
cv.co_total =cv.tree(co_total.tree ,FUN=prune.misclass )
names(cv.co_total)
cv.co_total

#size = number of terminal nodes considered
#k = value of the cost-complexity parameter
#dev = cross validation error rate

par(mfrow=c(1,2))
plot(cv.co_total$size ,cv.co_total$dev ,type="b")
plot(cv.co_total$k ,cv.co_total$dev ,type="b")

prune.co_total=prune.misclass(co_total.tree,best=4)
plot(prune.co_total)
text(prune.co_total,cex = 0.5, pretty=0)

tree.pred=predict(prune.co_total,co_total.test,type="class")
table(tree.pred ,decision.test)

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

