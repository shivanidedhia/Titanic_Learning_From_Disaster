# Continuation from the Understanding the Data 

# Distribution of males and females

table(data.combined$sex)

# Relationship between sex, pclass, and survival
ggplot(data.combined[1:891,], aes(x = sex, fill = survived)) +
  geom_bar() +
  facet_wrap(~pclass) + 
  ggtitle("Pclass") +
  xlab("Sex") +
  ylab("Total Count") +
  labs(fill = "Survived")

# Males were far more likely to survive compared to females, 2:1 ratio for survival
# Third Passenger Class died more than First and Second Class

# Distribution of Age

# 263 NA's in the age col.
summary(data.combined$age)

# 177 NA's if 891 rows are considered from the training. 
# More than half NA's are in the training datasets
summary(data.combined[1:891,"age"])

# Males in 3rd class over the age of 45 have not survived.
# Younger males in 2nd class have survived
ggplot(data.combined[1:891,], aes(x = age, fill = survived)) +
  facet_wrap(~sex + pclass) +
  geom_histogram(binwidth = 10) +
  xlab("Age") +
  ylab("Total Count") +
  ggtitle("Survival by Class and Gender")

# Check for NA's to use as proxy for the missing values in age
boys <- data.combined[which(data.combined$title == "Master."),]
summary(boys$age)

# Most Misses are adult as per the distribution. Age has 50 NA's. 
misses <- data.combined[which(data.combined$title == "Miss."),]
summary(misses$age)

# Younger Miss between 15 to 40 survived compared to older Miss in First Class
ggplot(misses[misses$survived != "None",], aes(x = age, fill = survived)) +
  facet_wrap(~pclass) +
  geom_histogram(binwidth = 5) +
  ggtitle("'Miss' Age by Pclass") + 
  xlab("Age") +
  ylab("Total Count")

misses.alone <- misses[which(misses$sibsp == 0 & misses$parch == 0),]
summary(misses.alone$age)
length(which(misses.alone$age <= 14.5))

# Understanding the Sibsp variable
summary(data.combined$sibsp)

# 7 unique values so it can converted to factor
length(unique(data.combined$sibsp))
data.combined$sibsp <- as.factor(data.combined$sibsp)




