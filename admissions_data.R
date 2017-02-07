install.packages("reshape2")
library("reshape2")

#Admissions data
co4 = read.csv("Cohort4.csv")
co5 = read.csv("Cohort5.csv")
co7 <- read.csv("Cohort7.csv")

#to check new csv against old

co4_check = read.csv("Cohort4.csv")
co5_check = read.csv("Cohort5.csv")
co7_check = read.csv("Cohort7.csv")

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

summary(co4$Status)
summary(co5$Status)

#Group enrolled and non-enrolled

co4$Status <- as.character(co4$Status)
co4$Status = ifelse(co4$Status=="Enrolled Packet Made", 1, 0)
co4

#Cohort5 enrolled + registered ==1, others ==0

co5$Status
str(co5$Status)
co5$Status <- as.character(co5$Status)
co5$Status[co5$Status == "Enrolled Packet Made"] = 1
co5$Status[co5$Status == "Registered"] = 1
co5$Status[co5$Status != 1] = 0

co5
co5_check

#bind dataframe 4 and 5 together

co4_5 <- rbind(co5, co4)
co4_5

# drop the zip code column
zip = "Local.zip"
co4_5 = co4_5[, !(names(co4_5) %in% zip)]
co4_5

#change WA to instate

co4_5$Local.state = as.character(co4_5$Local.state)
str(co4_5)
co4_5$Local.state[co4_5$Local.state == "WA"] = "in_state" 
co4_5$Local.state

#change rest to outstate

co4_5$Local.state[co4_5$Local.state != "in_state"] = "out_state" 
co4_5$Local.state

# Where outstate != Unitedstate change to intl

co4_5$Local.country = as.character(co4_5$Local.country)
coun = co4_5$Local.state[co4_5$Local.country != "UNITED STATES"] = "intl"


intl <- which(co4_5$Local.country!="UNITED STATES")
co4_5$Local.state[intl,] <- "intl"
head(co4_5, 20)

#really shitty model

install.packages("tree")
library("tree")
co4_5.tree <- tree(Status~.-Local.country, data = co4_5)
plot(co4_5.tree)
text(co4_5.tree, pretty = 0)
