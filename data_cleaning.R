# Load dataset
titanic <- read.csv(file.choose())

# View first 6 rows
head(titanic)

# Check structure
str(titanic)

# Check dimensions
dim(titanic)

# Summary
summary(titanic)
# Check missing values
colSums(is.na(titanic))
# Handle missing Age values
titanic$Age[is.na(titanic$Age)] <- median(titanic$Age, na.rm = TRUE)
# Check Age missing values again
sum(is.na(titanic$Age))
# Create CabinKnown variable
titanic$CabinKnown <- ifelse(
  is.na(titanic$Cabin) | trimws(titanic$Cabin) == "",
  "No",
  "Yes"
)

# Check CabinKnown
table(titanic$CabinKnown)
# Convert categorical variables to factors
titanic$Sex <- as.factor(titanic$Sex)
titanic$Embarked <- as.factor(titanic$Embarked)
titanic$CabinKnown <- as.factor(titanic$CabinKnown)

# Check categorical variables
str(titanic[, c("Sex", "Embarked", "CabinKnown")])
# Outlier detection using boxplots

boxplot(titanic$Age,
        main = "Boxplot of Age",
        ylab = "Age")

boxplot(titanic$Fare,
        main = "Boxplot of Fare",
        ylab = "Fare")
# Calculate Age outliers
Q1_Age <- quantile(titanic$Age, 0.25)
Q3_Age <- quantile(titanic$Age, 0.75)

IQR_Age <- Q3_Age - Q1_Age

lower_Age <- Q1_Age - 1.5 * IQR_Age
upper_Age <- Q3_Age + 1.5 * IQR_Age

# Count Age outliers
age_outliers <- titanic$Age[
  titanic$Age < lower_Age | titanic$Age > upper_Age
]

length(age_outliers)
# Check Age outliers
boxplot.stats(titanic$Age)$out
# Count Age outliers
length(boxplot.stats(titanic$Age)$out)
# Check Fare outliers
boxplot.stats(titanic$Fare)$out
# Count Fare outliers
length(boxplot.stats(titanic$Fare)$out)
length(boxplot.stats(titanic$Age)$out)
# Normalize Age
titanic$Age_Normalized <- 
  (titanic$Age - min(titanic$Age)) /
  (max(titanic$Age) - min(titanic$Age))

# Normalize Fare
titanic$Fare_Normalized <- 
  (titanic$Fare - min(titanic$Fare)) /
  (max(titanic$Fare) - min(titanic$Fare))

# Check the new columns
head(titanic[, c("Age_Normalized", "Fare_Normalized")])
# Survival count
table(titanic$Survived)

# Survival visualization
barplot(
  table(titanic$Survived),
  main = "Survival Count",
  xlab = "Survived (0 = No, 1 = Yes)",
  ylab = "Number of Passengers"
)
# Survival by gender
table(titanic$Sex, titanic$Survived)

# Visualization
barplot(
  table(titanic$Sex, titanic$Survived),
  beside = TRUE,
  main = "Survival by Gender",
  xlab = "Gender",
  ylab = "Number of Passengers",
  legend.text = TRUE
)
# Survival by passenger class
table(titanic$Pclass, titanic$Survived)

# Visualization
barplot(
  table(titanic$Pclass, titanic$Survived),
  beside = TRUE,
  main = "Survival by Passenger Class",
  xlab = "Passenger Class",
  ylab = "Number of Passengers",
  legend.text = TRUE
)
# Age distribution
hist(
  titanic$Age,
  main = "Age Distribution of Passengers",
  xlab = "Age",
  ylab = "Number of Passengers"
)
# Relationship between Age and Fare
plot(
  titanic$Age,
  titanic$Fare,
  main = "Age vs Fare",
  xlab = "Age",
  ylab = "Fare",
  pch = 19
)
# Correlation between Age and Fare
cor(titanic$Age, titanic$Fare)
# Correlation matrix
cor(
  titanic[, c("Survived", "Pclass", "Age", "SibSp", "Parch", "Fare")]
)
# Save cleaned Titanic dataset
write.csv(
  titanic,
  "titanic_cleaned.csv",
  row.names = FALSE
)

# Confirm final dataset dimensions
dim(titanic)

# Check remaining missing values
colSums(is.na(titanic))
