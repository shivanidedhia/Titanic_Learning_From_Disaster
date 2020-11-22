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

# Title has some predictive power
# Mr. is 3rd class who were traveling alone mostly died
ggplot(data.combined[1:891,], aes(x = sibsp, fill = survived)) +
  geom_bar() +
  facet_wrap(~pclass + title) + 
  ggtitle("Survival Rates by Title and Passenger Class") +
  xlab("SibSp") +
  ylab("Total Count") +
  ylim(0,300) +
  labs(fill = "Survived")

# Parent Children Variable with Pclass and Title
# Adult women in third class with a larger 
# family start to have a lower chance of survival
data.combined$parch <- as.factor(data.combined$parch)
ggplot(data.combined[1:891,], aes(x = parch, fill = survived)) +
  geom_bar() +
  facet_wrap(~pclass + title) + 
  ggtitle("Parent and Childeren Survival Rates with Pclass, Title") +
  xlab("ParCh") +
  ylab("Total Count") +
  ylim(0,300) +
  labs(fill = "Survived")

# Create a family size
temp.sibsp <- c(train$sibsp, test$sibsp)
temp.parch <- c(train$parch, test$parch)
data.combined$family.size <- as.factor(temp.sibsp + temp.parch + 1)

# Visualize the family size

ggplot(data.combined[1:891,], aes(x = family.size, fill = survived)) +
  geom_bar() +
  facet_wrap(~pclass + title) + 
  ggtitle("Family Size Survival Rates with Pclass, Title") +
  xlab("family.size") +
  ylab("Total Count") +
  ylim(0,300) +
  labs(fill = "Survived")

