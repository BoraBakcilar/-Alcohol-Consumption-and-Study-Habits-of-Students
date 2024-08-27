library(xlsx)
library(tidyverse)
library(ggplot2)
library(reshape)
data <- read.xlsx("D:\\Kullanıcılar\\365 Final Project\\365 Final project 1.xlsx", 1, sep.names = ".")

###Rate Variables
data[,14:22][data[,14:22] == "Strongly Disagree"] <- "-2"
data[,14:22][data[,14:22] == "Disagree"] <- "-1"
data[,14:22][data[,14:22] == "Neither Agree Nor Disagree"] <- "0"
data[,14:22][data[,14:22] == "Agree"] <- "1"
data[,14:22][data[,14:22] == "Strongly Agree"] <- "2"
data$hard_to_go_lectures <- (as.numeric(data$hard_to_go_lectures))*-1
data$socialize_more <- as.numeric(data$socialize_more)
data$deal_with_stress <- as.numeric(data$deal_with_stress)
data$increases_anxiety_future <- (as.numeric(data$increases_anxiety_future))*-1
data$more_depressed <- (as.numeric(data$more_depressed))*-1
data$control_myself <- as.numeric(data$control_myself)
data$drink_too_much_alcohol <- as.numeric(data$drink_too_much_alcohol)
data$drink_alone <- as.numeric(data$drink_alone)
data$drink_with_friends <- as.numeric(data$drink_with_friends)
data <- data %>% drop_na(c("hard_to_go_lectures" , "socialize_more" , "deal_with_stress" , "increases_anxiety_future" , "more_depressed" , "control_myself"))

###Splitting Data as Male Female
male <- subset(data, gender == "Male (Erkek)")
female <- subset(data, gender == "Female (Kadın)")

###Positivity Score
data$score <- data$hard_to_go_lectures + data$socialize_more + data$deal_with_stress + data$increases_anxiety_future + data$more_depressed + data$control_myself
male$score <- male$hard_to_go_lectures + male$socialize_more + male$deal_with_stress + male$increases_anxiety_future + male$more_depressed + male$control_myself
female$score <- female$hard_to_go_lectures + female$socialize_more + female$deal_with_stress + female$increases_anxiety_future + female$more_depressed + female$control_myself

###Male and Female Score Plots
ggplot(male, aes(score))+
  geom_histogram(binwidth = 1, color = "darkblue", fill = "lightblue", aes(y = ..density..)) +
  geom_vline(aes(xintercept = mean(score)), linetype = "dashed", size = 1, color = "black") +
  geom_density(alpha = .2)

ggplot(female, aes(score))+
  geom_histogram(binwidth = 0.5)
###Spend on Alcohol
data[,10][data[,10] == "0-32 TL"] <- "1"
data[,10][data[,10] == "33-100 TL"] <- "2"
data[,10][data[,10] == "33-100TL"] <- NA
data[,10][data[,10] == "101-200 TL"] <- "3"
data[,10][data[,10] == "201-400 TL"] <- "4"
data[,10][data[,10] == "401-1000 TL"] <- "5"
data[,10][data[,10] == "1001-2000 TL"] <- "6"
data[,10][data[,10] == "1001-2001 TL"] <- NA
data[,10][data[,10] == "2001+ TL"] <- "7"
data$spend_alcohol <- as.numeric(data$spend_alcohol)

###Gender
data[,5][data[,5] == "Male (Erkek)"] <- "1"
data[,5][data[,5] == "Female (Kadın)"] <- "2"
data[,5][data[,5] == "Prefer not to say (Söylememeyi tercih ediyorum)"] <- NA
data[,5][data[,5] == "Others (Diğer)"] <- NA
data$gender <- as.numeric(data$gender)

#Numeric Function
data$cgpa <- as.numeric(data$cgpa)

### Removing NA's
data <- data %>% drop_na(c("spend_alcohol", "where", "first_age", "how_often_drunk", "gender"))

###Spend Alcohol and Where Relation(Not Significant)
lmspendwhere <- lm(spend_alcohol ~ where, data)
summary(lmspendwhere)

###Spend Alcohol and Which Alcohol Relation(Raki, Vodka, Whiskey are Significant)
lmspendwhich <- lm(spend_alcohol ~ which_alcohol, data)
summary(lmspendwhich)

###Frequency of Drinking Alcohol Relation With Others
lmfrequencyothers <- lm(often_alcohol ~ spend_alcohol + first_age, data)
summary(lmgenderexperience)

lmcgpa <- lm(cgpa ~ often_alcohol + feel, data)
summary(lmcgpa)


data1 <- reshape::melt(data, id = c("spend_alcohol"))
ggplot(data, aes(spend_alcohol, how_often_drunk, fill=gender))+
  geom_bar(stat = "identity")+ facet_wrap(~ gender) + scale_x_continuous(breaks=seq(1,12,1))
table(data$how_often_drunk)

ggplot(data, aes(often_alcohol, cgpa)) +
  geom_point(stat = "identity") + geom_smooth(method=lm)










