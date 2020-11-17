library(ggplot2)
library(stringr)

# Load raw data

train <- read.csv("train.csv",header = TRUE)
test <- read.csv("test.csv",header = TRUE)

# Adding "survived variable" to the test data 
test.survived <- data.frame(survived = rep("None", nrow(test)), test[,])

# Combine data sets
data.combined <- rbind(train, test.survived)

# Structure of the dataset
str(data.combined)

# p class is the class of ticket for a passagener which is not a int.
# 1 is First Class, 2 is Second Class and 3 is Third
data.combined$pclass <- as.factor(data.combined$pclass)

# 1 means survived and 0 means didn't survive
data.combined$survived <- as.factor(data.combined$survived)

# How many people survived?
table(data.combined$survived) # 549 passed away and 342 survived

# What was the distribution across classes?
table(data.combined$pclass) # More people are in third class. 

# How many people survived by class ?
train$pclass <- as.factor(train$pclass)
ggplot(train, aes(x = pclass, fill = factor(survived))) +
  geom_bar() +
  xlab("Pclass") +
  ylab("Total Count") +
  labs(fill = "Survived") 

# People in 1st class survived more than people in the 3rd class. 
# Possibly since the 1st class passengers are closer to the lifeboats.

# 'names' is not a factor so converted it to character 
head(as.character(train$name))

# How many unique names are there across both train & test?
length(unique(as.character(data.combined$name))) 

## two duplicate names
# How many unique names are there across both train & test?
length(unique(as.character(data.combined$name)))

# First, get the duplicate names and store them as a vector
dup.names <- as.character(data.combined[which(duplicated(as.character(data.combined$name))), "name"])

# Next, take a look at the records in the combined data set
data.combined[which(data.combined$name %in% dup.names),]

# None of the names are duplicated as they are different people

# Pattern with Misses
misses <- data.combined[which(str_detect(data.combined$name, "Miss.")),]
misses[1:5,]

# Pattern with Mrses
mrses <- data.combined[which(str_detect(data.combined$name, "Mrs.")), ]
mrses[1:5,]

# Pattern with Males
males <- data.combined[which(data.combined$sex == "male"), ]
males[1:5,]

# Expand upon the realtionship between `Survived` and `Pclass` by adding the new `Title` variable to the
# data set and then explore a potential 3-dimensional relationship.

# Create a utility function to help with title extraction
# NOTE - Using the grep function here, but could have used the str_detect function as well.
extractTitle <- function(name) {
  name <- as.character(name)
  
  if (length(grep("Miss.", name)) > 0) {
    return ("Miss.")
  } else if (length(grep("Master.", name)) > 0) {
    return ("Master.")
  } else if (length(grep("Mrs.", name)) > 0) {
    return ("Mrs.")
  } else if (length(grep("Mr.", name)) > 0) {
    return ("Mr.")
  } else {
    return ("Other")
  }
}


# NOTE - The code below uses a for loop which is not a very R way of
#        doing things
titles <- NULL
for (i in 1:nrow(data.combined)) {
  titles <- c(titles, extractTitle(data.combined[i,"name"]))
}
data.combined$title <- as.factor(titles)


# Since we only have survived lables for the train set, only use the
# first 891 rows
ggplot(data.combined[1:891,], aes(x = title, fill = survived)) +
  geom_bar() +
  facet_wrap(~pclass) + 
  ggtitle("Pclass") +
  xlab("Title") +
  ylab("Total Count") +
  labs(fill = "Survived")

