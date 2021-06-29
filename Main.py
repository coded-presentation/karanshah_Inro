# importing the libraries 

#Fmport the libraries:
library(tidyverse)
library(rpart)
library(rpart.plot)
library(rattle)

#Load the train data:
titanic_train_data <- read.csv("../input/titanic/train.csv")
titanic_test_data <- read.csv("../input/titanic/test.csv")



#Lets get an idea of the data:
glimpse(titanic_train_data)

#Lets also have a summary of the data:
summary(titanic_train_data)



#Now change the data type for some of this columns:
titanic_train_data$Survived <- as.factor(titanic_train_data$Survived)
titanic_train_data$Pclass <- as.factor(titanic_train_data$Pclass)
titanic_train_data$Sex <- as.factor(titanic_train_data$Sex)
titanic_train_data$Embarked <- as.factor(titanic_train_data$Embarked)



ggplot(titanic_train_data, aes(x = Survived, fill = Survived)) + 
  geom_bar() +
  labs(y = "Passenger Count",
       x = "Survived",
       title = "Global survival rate") + 
  scale_x_discrete(labels=c("No", "Yes"))+
  scale_fill_manual(values = c("brown1", "chartreuse3")) +
  theme_minimal()+
  theme(legend.position="none")

options(repr.plot.width=25, repr.plot.height=8)



ggplot(titanic_train_data, aes(x = Survived, fill = Survived)) + 
  geom_bar() +
  labs(y = "Passenger Count",
       x = "Survived",
       title = "Global survival rate") + 
  scale_x_discrete(labels=c("No", "Yes"))+
  scale_fill_manual(values = c("brown1", "chartreuse3")) +
  facet_wrap(~Sex) +
  theme_minimal()+
  theme(legend.position="none")

options(repr.plot.width=25, repr.plot.height=8)



#Lets add the age ranks for both sexes:
ggplot(titanic_train_data, aes(x = Age, fill=Survived)) +
  geom_histogram(binwidth = 5) +
  labs(y = "Passenger Count",
       x = "Survived",
       title = "Global survival rate") + 
  scale_x_discrete(labels=c("No", "Yes"))+
  scale_fill_manual(values = c("brown1", "chartreuse3")) +
  facet_wrap(~Sex) +
  theme_minimal()+
  theme(legend.position="none")

options(repr.plot.width=25, repr.plot.height=8)

#Warning message:
#“Removed 177 rows containing non-finite values (stat_bin).”


titanic_train_data %>% filter(titanic_train_data$Embarked == "C" | titanic_train_data$Embarked == "Q" | titanic_train_data$Embarked == "S") %>%
ggplot(aes(x = Embarked, fill=Survived)) +
  geom_bar(position="dodge") +
  labs(y = "Passenger Count",
       x = "Port",
       title = "Global survival rate") + 
  scale_x_discrete(labels=c("Cherbourg", "Queenstown", "Southampton"))+
  scale_fill_manual(values = c("brown1", "chartreuse3")) +
  facet_wrap(~Pclass) +
  theme_minimal()+
  theme(legend.position="none")

ggplot(titanic_train_data, aes(x = SibSp, fill=Survived)) +
  geom_bar() +
  labs(y = "Passenger Count",
       x = "Port",
       title = "Global survival rate") + 
  scale_fill_manual(values = c("brown1", "chartreuse3")) +
  facet_wrap(~Pclass) +
  theme_minimal()+
  theme(legend.position="none")

options(repr.plot.width=25, repr.plot.height=8)


decision_tree <- rpart(Survived ~ as.numeric(Pclass) + Sex + Age + SibSp + Parch + Fare + Embarked,data=titanic_train_data,method="class")
fancyRpartPlot(decision_tree)

options(repr.plot.width=25, repr.plot.height=25)



prediction <- predict(decision_tree, titanic_test_data, type = "class")
titanic_submission <- data.frame(PassengerId = titanic_test_data$PassengerId, Survived = prediction)
write.csv(titanic_submission, file = "titanic_submission.csv", row.names = FALSE)





