# Calculate Cronbach's Alpha for the selected variables

install.packages("psych")  # if not already installed
library(psych)
install.packages("ltm")
library(ltm)

# Load required libraries
library(dplyr)     # For ML and clustering functions
library(ggplot2)   # For plots and charts
library(tidyr)
library(car)       # For ANOVA and MANCOVA
library(lmtest)    # For statistical tests
library(psych)     # For descriptive statistics
install.packages("spdep")
library(spdep)     # For spatial models
install.packages("caret")
library(caret)     # For machine learning models


library(readr)
Provisional_COVID_19_Deaths_by_Sex_and_Age <- read_csv("NCU PhD Program/DIS-9902A - The Dissertation Proposal/Covid-19 Influenza and Pneumonia data/Provisional_COVID-19_Deaths_by_Sex_and_Age.csv")
View(Provisional_COVID_19_Deaths_by_Sex_and_Age)

#remove(population)

population <- Provisional_COVID_19_Deaths_by_Sex_and_Age
population

describe (population)
# Sample1 is for age-group 30-49 years, for male and female for the year 2020 to 2023
remove(sample1)
sample1<-subset (population[ ,c(5:12,14)], population$State== "United States" 
                 &  (population$Age_Group=="30-39 years"  | population$Age_Group=="35-44 years" |population$Age_Group=="40-49 years" )
                 & (population$Sex =="Male" | population$Sex =="Female")
                 & (population$Year %in% c(2020,2021,2022,2023))
                 & (population$Month %in% c(1,2,3,4,5,6,7,8,9,10,11,12))
                 )
sample1
sample1 <- na.omit(sample1)
tail(sample1)        

s1 <- subset(sample1[c(6:9)])
s1
alpha(s1, check.keys = TRUE, na.rm = TRUE)
#cronbach.alpha(s1, CI=TRUE, standardized=TRUE)

# Sample2 is for age-group 18-29 years, for male and female for the year 2020 to 2023
remove(sample2)
sample2<-subset (population[ ,c(5:12,14)], population$State== "United States" 
                 &  (population$Age_Group=="18-29 years" )
                 & (population$Sex =="Male" | population$Sex =="Female")
                 & (population$Year %in% c(2020,2021,2022,2023))
                 & (population$Month %in% c(1,2,3,4,5,6,7,8,9,10,11,12))
)
sample2
sample2 <- na.omit(sample2)
tail(sample2)          

s2 <- subset(sample2[c(6:9)])
s2
alpha(s2, check.keys = TRUE, na.rm = TRUE)
#cronbach.alpha(s2, CI=TRUE, standardized=TRUE)

# Sample3 is for age-group 50-64 years, for male and female for the year 2020 to 2023
remove(sample3)
sample3<-subset (population[ ,c(5:12,14)], population$State== "United States" 
                 &  (population$Age_Group=="50-64 years" )
                 & (population$Sex =="Male" | population$Sex =="Female")
                 & (population$Year %in% c(2020,2021,2022,2023))
                 & (population$Month %in% c(1,2,3,4,5,6,7,8,9,10,11,12)))
sample3
sample3 <- na.omit(sample3)
tail(sample3)          


#--------------------------------
#Normality Testing for various Models
# Install necessary packages if not already installed
install.packages("e1071")     # For skewness and kurtosis
install.packages("ggplot2")   # For Q-Q plots
#install.packages("ggplot")   # For Q-Q plots

# Load the packages
library(e1071)
library(ggplot2)
#library(ggplot)

sample1

# Shapiro-Wilk Test for each variable
shapiro_test_covid <- shapiro.test( sample1$COVID_19_Deaths)
shapiro_test_flu <- shapiro.test(sample1$Influenza_Deaths)
shapiro_test_pneumonia <- shapiro.test(sample1$Pneumonia_Deaths)
shapiro_test_total<- shapiro.test(sample1$Total_Deaths)

# Print results
print(shapiro_test_covid)
print(shapiro_test_flu)
print(shapiro_test_pneumonia)
print(shapiro_test_total)


# Q-Q Plots for each variable-For the Middle-aged group
m_qqplot_covid <- ggplot(sample1, aes( sample = sample1$COVID_19_Deaths)) +
  stat_qq() + stat_qq_line() + ggtitle("Q-Q Plot for COVID-19 Deaths-Middle Aged-Group (30-49)") +
  xlab("Distribution Around Mean") +
  ylab("Death Count")


m_qqplot_flu <- ggplot(sample1, aes(sample = sample1$Influenza_Deaths)) +
  stat_qq() + stat_qq_line() + ggtitle("Q-Q Plot for Influenza Deaths-Middle Aged-Group (30-49)") +
  xlab("Distribution Around Mean") +
  ylab("Death Count")

m_qqplot_pneumonia <- ggplot(sample1, aes(sample = sample1$Pneumonia_Deaths)) +
  stat_qq() + stat_qq_line() + ggtitle("Q-Q Plot for Pneumonia Deaths-Middle Aged-Group (30-49)") +
  xlab("Distribution Around Mean") +
  ylab("Death Count")

m_qqplot_total <- ggplot(sample1, aes(sample = sample1$Total_Deaths)) +
  stat_qq() + stat_qq_line() + ggtitle("Q-Q Plot for Total Deaths-Middle Aged-Group (30-49)") +
  xlab("Distribution Around Mean") +
  ylab("Death Count")

# Display Q-Q plots
print(m_qqplot_covid)
print(m_qqplot_flu)
print(m_qqplot_pneumonia)
print(m_qqplot_total)

# Q-Q Plots for each variable-For the Young-aged group
y_qqplot_covid <- ggplot(sample2, aes(sample = sample2$COVID_19_Deaths)) +
  stat_qq() + stat_qq_line() + ggtitle("Q-Q Plot for COVID-19 Deaths-Young Aged-Group (18-29)")+
  xlab("Distribution Around Mean") +
  ylab("Death Count")

y_qqplot_flu <- ggplot(sample2, aes(sample = sample2$Influenza_Deaths)) +
  stat_qq() + stat_qq_line() + ggtitle("Q-Q Plot for Influenza Deaths-Young Aged-Group (18-29)")+
  xlab("Distribution Around Mean") +
  ylab("Death Count")

y_qqplot_pneumonia <- ggplot(sample2, aes(sample = sample2$Pneumonia_Deaths)) +
  stat_qq() + stat_qq_line() + ggtitle("Q-Q Plot for Pneumonia Deaths-Young Aged-Group (18-29)")+
  xlab("Distribution Around Mean") +
  ylab("Death Count")

y_qqplot_total <- ggplot(sample2, aes(sample = sample2$Total_Deaths)) +
  stat_qq() + stat_qq_line() + ggtitle("Q-Q Plot for Total Deaths-Young Aged-Group (18-29)")+
  xlab("Distribution Around Mean") +
  ylab("Death Count")

# Display Q-Q plots
print(y_qqplot_covid)
print(y_qqplot_flu)
print(y_qqplot_pneumonia)
print(y_qqplot_total)

# Q-Q Plots for each variable-For the Old-aged group
o_qqplot_covid <- ggplot(sample3, aes(sample = sample3$COVID_19_Deaths)) +
  stat_qq() + stat_qq_line() + ggtitle("Q-Q Plot for COVID-19 Deaths-Old Aged-Group (50-64)")+
  xlab("Distribution Around Mean") +
  ylab("Death Count")

o_qqplot_flu <- ggplot(sample3, aes(sample = sample3$Influenza_Deaths)) +
  stat_qq() + stat_qq_line() + ggtitle("Q-Q Plot for Influenza Deaths-Old Aged-Group (50-64)")+
  xlab("Distribution Around Mean") +
  ylab("Death Count")

o_qqplot_pneumonia <- ggplot(sample3, aes(sample = sample3$Pneumonia_Deaths)) +
  stat_qq() + stat_qq_line() + ggtitle("Q-Q Plot for Pneumonia Deaths-Old Aged-Group (50-64)")+
  xlab("Distribution Around Mean") +
  ylab("Death Count")

o_qqplot_total <- ggplot(sample3, aes(sample = sample3$Total_Deaths)) +
  stat_qq() + stat_qq_line() + ggtitle("Q-Q Plot for Total Deaths-Old Aged-Group (50-64)")+
  xlab("Distribution Around Mean") +
  ylab("Death Count")

# Display Q-Q plots
print(o_qqplot_covid)
print(o_qqplot_flu)
print(o_qqplot_pneumonia)
print(o_qqplot_total)

#---- Log transformation to fit the normality of data distribution
#--------------------------------------------------------------------

# Example: Log Transformation and Q-Q Plot for COVID-19 Deaths - Middle aged population (30-49)

# Step 1: Log Transformation
# Adding 1 to avoid log(0) if there are zero values in the data
sample1$log_COVID19_Deaths <- log(sample1$COVID_19_Deaths + 1)

sample1

sample2

# Step 2: Regenerate Q-Q Plot for the Transformed Data
qqnorm(sample1$log_COVID19_Deaths, main = "Q-Q Plot of Log-Transformed COVID-19 Deaths for middle-aged group", 
       xlab = "Distribution Around Mean", ylab = "Death Count")

qqline(sample1$log_COVID19_Deaths, col = "black")


# Repeat for other variables if necessary
# Example: Log Transformation and Q-Q Plot for Influenza Deaths
sample1$log_Influenza_Deaths <- log(sample1$Influenza_Deaths + 1)
qqnorm(sample1$log_Influenza_Deaths, main = "Q-Q Plot of Log-Transformed Influenza Deaths for middle-aged group", 
        xlab = "Distribution Around Mean", ylab = "Death Count")
qqline(sample1$log_Influenza_Deaths, col = "black")

# Example: Log Transformation and Q-Q Plot for Pneumonia Deaths
sample1$log_Pneumonia_Deaths <- log(sample1$Pneumonia_Deaths + 1)
qqnorm(sample1$log_Pneumonia_Deaths, main = "Q-Q Plot of Log-Transformed Pneumonia Deaths for middle-aged group", 
       xlab = "Distribution Around Mean", ylab = "Death Count")
qqline(sample1$log_Pneumonia_Deaths, col = "black")

# Example: Log Transformation and Q-Q Plot for Total Deaths
sample1$log_Total_Deaths <- log(sample1$Total_Deaths + 1)
qqnorm(sample1$log_Total_Deaths, main = "Q-Q Plot of Log-Transformed Total Deaths for middle-aged group", 
       xlab = "Distribution Around Mean", ylab = "Death Count")
qqline(sample1$log_Total_Deaths, col = "black")



# Example: Log Transformation and Q-Q Plot for COVID-19 Deaths - Young aged population (18-29)

# Step 1: Log Transformation
# Adding 1 to avoid log(0) if there are zero values in the data
sample2$log_COVID19_Deaths <- log(sample2$COVID_19_Deaths + 1)

# Step 2: Regenerate Q-Q Plot for the Transformed Data
qqnorm(sample2$log_COVID19_Deaths, main = "Q-Q Plot of Log-Transformed COVID-19 Deaths for young-aged group", 
       xlab = "Distribution Around Mean", ylab = "Death Count")
qqline(sample2$log_COVID19_Deaths, col = "black")

sample2

# Repeat for other variables if necessary
# Example: Log Transformation and Q-Q Plot for Influenza Deaths
sample2$log_Influenza_Deaths <- log(sample2$Influenza_Deaths + 1)
qqnorm(sample2$log_Influenza_Deaths, main = "Q-Q Plot of Log-Transformed Influenza Deaths for young-aged group", 
       xlab = "Distribution Around Mean", ylab = "Death Count")
qqline(sample2$log_Influenza_Deaths, col = "black")

# Example: Log Transformation and Q-Q Plot for Pneumonia Deaths
sample2$log_Pneumonia_Deaths <- log(sample2$Pneumonia_Deaths + 1)
qqnorm(sample2$log_Pneumonia_Deaths, main = "Q-Q Plot of Log-Transformed Pneumonia Deaths for young-aged group", 
       xlab = "Distribution Around Mean", ylab = "Death Count")
qqline(sample2$log_Pneumonia_Deaths, col = "black")

# Example: Log Transformation and Q-Q Plot for Total Deaths
sample2$log_Total_Deaths <- log(sample2$Total_Deaths + 1)
qqnorm(sample2$log_Total_Deaths, main = "Q-Q Plot of Log-Transformed Total Deaths for young-aged group", 
       xlab="Distribution Around Mean", ylab = "Death Count")
qqline(sample2$log_Total_Deaths, col = "black")


# Example: Log Transformation and Q-Q Plot for COVID-19 Deaths - Old aged population (50-64)

# Step 1: Log Transformation
# Adding 1 to avoid log(0) if there are zero values in the data
sample3$log_COVID19_Deaths <- log(sample3$COVID_19_Deaths + 1)

# Step 2: Regenerate Q-Q Plot for the Transformed Data
qqnorm(sample3$log_COVID19_Deaths, main = "Q-Q Plot of Log-Transformed COVID-19 Deaths for old-aged group", 
       xlab = "Distribution Around Mean", ylab = "Death Count")
qqline(sample3$log_COVID19_Deaths, col = "black")

# Repeat for other variables if necessary
# Example: Log Transformation and Q-Q Plot for Influenza Deaths
sample3$log_Influenza_Deaths <- log(sample3$Influenza_Deaths + 1)
qqnorm(sample3$log_Influenza_Deaths, main = "Q-Q Plot of Log-Transformed Influenza Deaths for old-aged group", 
       xlab = "Distribution Around Mean", ylab = "Death Count")
qqline(sample3$log_Influenza_Deaths, col = "black")

# Example: Log Transformation and Q-Q Plot for Pneumonia Deaths
sample3$log_Pneumonia_Deaths <- log(sample3$Pneumonia_Deaths + 1)
qqnorm(sample3$log_Pneumonia_Deaths, main = "Q-Q Plot of Log-Transformed Pneumonia Deaths for old-aged group", 
       xlab = "Distribution Around Mean", ylab = "Death Count")
qqline(sample3$log_Pneumonia_Deaths, col = "black")

# Example: Log Transformation and Q-Q Plot for Total Deaths
sample3$log_Total_Deaths <- log(sample3$Total_Deaths + 1)
qqnorm(sample3$log_Total_Deaths, main = "Q-Q Plot of Log-Transformed Total Deaths for old-aged group", 
       xlab = "Distribution Around Mean", ylab = "Death Count")
qqline(sample3$log_Total_Deaths, col = "black")


#-------------------------------------------------
#------------ HISTOGRMAM for LOG Transformed data
#-------------------------------------------------

# Assuming mortality_data is your data frame

# Histogram for Log-Transformed COVID-19 Deaths
hist_log_covid <- ggplot(sample1, aes(x = log_COVID19_Deaths)) +
  geom_histogram(binwidth = 0.2, fill = "blue", color = "black", alpha = 0.7) +
  labs(title = "Histogram of Log-Transformed COVID-19 Deaths", x = "Log(COVID-19 Deaths)", y = "Frequency") +
  theme_minimal()

# Display histogram for Log-Transformed COVID-19 Deaths
print(hist_log_covid)

# Histogram for Log-Transformed Influenza Deaths
hist_log_flu <- ggplot(sample1, aes(x = log_Influenza_Deaths)) +
  geom_histogram(binwidth = 0.2, fill = "green", color = "black", alpha = 0.7) +
  labs(title = "Histogram of Log-Transformed Influenza Deaths", x = "Log(Influenza Deaths)", y = "Frequency") +
  theme_minimal()

# Display histogram for Log-Transformed Influenza Deaths
print(hist_log_flu)

# Histogram for Log-Transformed Pneumonia Deaths
hist_log_pneumonia <- ggplot(sample1, aes(x = log_Pneumonia_Deaths)) +
  geom_histogram(binwidth = 0.2, fill = "red", color = "black", alpha = 0.7) +
  labs(title = "Histogram of Log-Transformed Pneumonia Deaths", x = "Log(Pneumonia Deaths)", y = "Frequency") +
  theme_minimal()

# Display histogram for Log-Transformed Pneumonia Deaths
print(hist_log_pneumonia)

# Histogram for Log-Transformed Pneumonia Deaths
hist_log_total <- ggplot(sample1, aes(x = log_Total_Deaths)) +
  geom_histogram(binwidth = 0.2, fill = "red", color = "black", alpha = 0.7) +
  labs(title = "Histogram of Log-Transformed Pneumonia Deaths", x = "Log(Pneumonia Deaths)", y = "Frequency") +
  theme_minimal()

# Display histogram for Log-Transformed Pneumonia Deaths
print(hist_log_total)

#-------------------------------------------------------

# MANCOVA Assumption validation
#---------------------------------

#Normality Testing of Dependent Variables
# Assuming 'Age_Group' is the grouping variable in the dataset

# Shapiro-Wilk test for normality of dependent variables
shapiro_test_covid <- by(sample1$COVID_19_Deaths, sample1$Age_Group, shapiro.test)
shapiro_test_influenza <- by(sample1$Influenza_Deaths, sample1$Age_Group, shapiro.test)
shapiro_test_pneumonia <- by(sample1$Pneumonia_Deaths, sample1$Pneumonia_Deaths, shapiro.test)

# Results
shapiro_test_covid
shapiro_test_influenza
shapiro_test_pneumonia

# Homogeneity of Variance-Covariance Matrices (Box's M Test)
install.packages("heplots")
library(heplots)

# Box's M test for homogeneity of variance-covariance matrices
box_m_test_m <- boxM(cbind(COVID_19_Deaths, Influenza_Deaths, Pneumonia_Deaths) ~ Age_Group, data = sample1)
box_m_test_y <- boxM(cbind(COVID_19_Deaths, Influenza_Deaths, Pneumonia_Deaths) ~ Age_Group, data = sample2)
box_m_test_o <- boxM(cbind(COVID_19_Deaths, Influenza_Deaths, Pneumonia_Deaths) ~ Age_Group, data = sample3)

# Results for middle, y0ouong and old age groups
print(box_m_test_m)
print(box_m_test_y)
print(box_m_test_o)

#Linearity Between Covariates and Dependent Variables
#Scatterplots or correlation matrices can be used to examine the linear relationship
#between covariates (e.g., Age_Group, Sex, State) 
#and dependent variables (e.g., COVID_19_Deaths, Influenza_Deaths, Pneumonia_Deaths).

# Scatterplot to check linearity between covariate 'Age_Group' and 'COVID_19_Deaths'

sample1 <- na.omit(sample1)
sample1

# Step 1: Log Transformation
# Adding 1 to avoid log(0) if there are zero values in the data
sample3$log_COVID19_Deaths <- log(sample3$COVID_19_Deaths + 1)

# Step 2: Regenerate Q-Q Plot for the Transformed Data
qqnorm(sample3$log_COVID19_Deaths, main = "Q-Q Plot of Log-Transformed COVID-19 Deaths for old-aged group", 
       xlab = "Distribution Around Mean", ylab = "Death Count")
qqline(sample3$log_COVID19_Deaths, col = "black")

# Log Transformation for  variables if necessary (due to normality assumption)
sample1$log_COVID19_Deaths <- log(sample1$COVID_19_Deaths + 1)
sample1$log_Influenza_Deaths <- log(sample1$Influenza_Deaths + 1)
sample1$log_Pneumonia_Deaths <- log(sample1$Pneumonia_Deaths + 1)
sample1$log_Total_Deaths <- log(sample1$Total_Deaths + 1)

sample1$log_COVID19_Deaths


# Middle age group-Convert Age_Group (30-49) to a factor if it's not already
sample1$Age_Group <- as.factor(sample1$Age_Group)
# Boxplot to show distribution of COVID-19 deaths by age group
boxplot(sample1$log_COVID19_Deaths ~ Age_Group, data = sample1, 
        main = "COVID-19 Deaths by Age Group", 
        xlab = "Age Group", ylab = "COVID-19 Deaths", 
        col = "lightblue")

# Boxplot for Influenza Deaths
boxplot(sample1$log_Influenza_Deaths ~ Age_Group, data = sample1, 
        main = "Influenza Deaths by Age Group", 
        xlab = "Age Group", ylab = "Influenza Deaths", 
        col = "lightgreen")

# Boxplot for Pneumonia Deaths
boxplot(sample1$log_Pneumonia_Deaths ~ Age_Group, data = sample1, 
        main = "Pneumonia Deaths by Age Group", 
        xlab = "Age Group", ylab = "Pneumonia Deaths", 
        col = "lightcoral")

# Boxplot for Pneumonia Deaths
boxplot(sample1$log_Total_Deaths ~ Age_Group, data = sample1, 
        main = "Pneumonia Deaths by Age Group", 
        xlab = "Age Group", ylab = "Total Deaths", 
        col = "lightcoral")

#Young age group-  Log Transformation fo rvariables if necessary (due to normality assumption)
sample2$log_COVID19_Deaths <- log(sample2$COVID_19_Deaths + 1)
sample2$log_Influenza_Deaths <- log(sample2$Influenza_Deaths + 1)
sample2$log_Pneumonia_Deaths <- log(sample2$Pneumonia_Deaths + 1)
sample2$log_Total_Deaths <- log(sample2$Total_Deaths + 1)

sample2$log_COVID19_Deaths


# Convert Age_Group (18-29) to a factor if it's not already
sample2$Age_Group <- as.factor(sample2$Age_Group)
# Boxplot to show distribution of COVID-19 deaths by age group
boxplot(sample2$log_COVID19_Deaths ~ Age_Group, data = sample2, 
        main = "COVID-19 Deaths by Age Group", 
        xlab = "Age Group", ylab = "COVID-19 Deaths", 
        col = "lightblue")

# Boxplot for Influenza Deaths
boxplot(sample2$log_Influenza_Deaths ~ Age_Group, data = sample2, 
        main = "Influenza Deaths by Age Group", 
        xlab = "Age Group", ylab = "Influenza Deaths", 
        col = "lightgreen")

# Boxplot for Pneumonia Deaths
boxplot(sample2$log_Pneumonia_Deaths ~ Age_Group, data = sample2, 
        main = "Pneumonia Deaths by Age Group", 
        xlab = "Age Group", ylab = "Pneumonia Deaths", 
        col = "lightcoral")

# Boxplot for Pneumonia Deaths
boxplot(sample2$log_Total_Deaths ~ Age_Group, data = sample2, 
        main = "Total Deaths by Age Group", 
        xlab = "Age Group", ylab = "Total Deaths", 
        col = "lightcoral")


remove(sample4)

# combining the rows of the two dataframes
sample4 = rbind(sample1,sample2,sample3)
print(sample4)

tail(sample4)

# Boxplot for Covid-19 Deaths against the age-group
boxplot(sample4$log_Total_Deaths ~ Age_Group, data = sample4, 
        main = "Covid-19 Deaths by Age Group", 
        xlab = "Age Group", ylab = "Covid-19 Deaths", 
        col = "grey")

# Boxplot for Pneumonia Deaths against the age-group
boxplot(sample4$log_Pneumonia_Deaths ~ Age_Group, data = sample4, 
        main = "Pneumonia Deaths by Age Group", 
        xlab = "Age Group", ylab = "Pneumonia Deaths", 
        col = "grey")


# Boxplot for Influenza Deaths against the age-group
boxplot(sample4$log_Influenza_Deaths ~ Age_Group, data = sample4, 
        main = "Influenza Deaths by Age Group", 
        xlab = "Age Group", ylab = "Influenza Deaths", 
        col = "grey")


# Boxplot for Total Deaths against the age-group
boxplot(sample4$log_Total_Deaths ~ Age_Group, data = sample4, 
        main = "Total Deaths by Age Group", 
        xlab = "Age Group", ylab = "Total Deaths", 
        col = "grey")


# Boxplot for Covid-19 Deaths against the sex
boxplot(sample4$log_Total_Deaths ~ Sex, data = sample4, 
        main = "Covid-19 Deaths by Age Group", 
        xlab = "Age Group", ylab = "Covid-19 Deaths", 
        col = "grey")

# Boxplot for Pneumonia Deaths against the sex
boxplot(sample4$log_Pneumonia_Deaths ~ Sex, data = sample4, 
        main = "Pneumonia Deaths by Age Group", 
        xlab = "Age Group", ylab = "Pneumonia Deaths", 
        col = "grey")


# Boxplot for Influenza Deaths against the sex
boxplot(sample4$log_Influenza_Deaths ~ Sex, data = sample4, 
        main = "Influenza Deaths by Age Group", 
        xlab = "Age Group", ylab = "Influenza Deaths", 
        col = "grey")


# Boxplot for Total Deaths against the sex
boxplot(sample4$log_Total_Deaths ~ Sex, data = sample4, 
        main = "Total Deaths by Age Group", 
        xlab = "Age Group", ylab = "Total Deaths", 
        col = "grey")
#--------------------------------

#--------------------------
# Checking Homogeneity of Regression Slopes
# Load necessary packages
library(car)  # For the linear hypothesis test
library(ggplot2)

# Ensure 'Sex' is a factor variable (assuming male and female as categories)
sample4$Sex <- as.factor(sample4$Sex)

# Fit a MANCOVA model with interaction terms to test homogeneity of regression slopes
# The interaction term between Sex and the dependent variables is included to test for slope homogeneity

# Fit the model with the interaction term
mancova_model <- lm(cbind(COVID_19_Deaths, Pneumonia_Deaths, Influenza_Deaths, Total_Deaths) ~ Age_Group * Sex, data = sample4)

# Perform a multivariate analysis of variance (MANOVA)
manova_results <- manova(cbind(COVID_19_Deaths, Pneumonia_Deaths, Influenza_Deaths, Total_Deaths) ~ Age_Group * Sex, data = sample4)

# Summarize the results of the MANOVA (which includes testing the interaction term)
summary(manova_results, test = "Wilks")


#Below section of code is to describe your dataset and identify missing values.
#This will help in providing an overview of the dataset and allow to identify any data points that might be missing,
#which is crucial for cleaning and preprocessing the data.

# Load required libraries
library(dplyr)
library(ggplot2)
install.packages("skimr")
library(skimr)   # For dataset summary
install.packages("naniar")
library(naniar)  # For missing data visualization

# Basic summary of the dataset
summary(population)

# Detailed summary using skimr/describe
#skim(population)
describe(population)

# Count missing values in each column
missing_values <- sapply(population, function(x) sum(is.na(x)))

# Display the missing values count
print(missing_values)

# Plot missing values using naniar
gg_miss_var(population) + 
  labs(title = "Missing Data by Variable") +
  theme_minimal()

# Another option: vis_miss to visualize the pattern of missing data
#vis_miss(population)

# Calculate percentage of missing values for each variable
missing_percentage <- colSums(is.na(population)) / nrow(population) * 100

# View the missing percentage for each variable
print(missing_percentage)

population1 <- population
population1

# Remove rows with missing values
data_cleaned <- na.omit(population1)
data_cleaned

# Replace missing values with the median (for numerical variables)
population1$COVID_19_Deaths[is.na(population1$COVID_19_Deaths)] <- median(population1$COVID_19_Deaths, na.rm = TRUE)
population1$Pneumonia_Deaths[is.na(population1$Pneumonia_Deaths)] <- median(population1$Pneumonia_Deaths, na.rm = TRUE)
population1$Influenza_Deaths[is.na(population1$Influenza_Deaths)] <- median(population1$Influenza_Deaths, na.rm = TRUE)
population1$Total_Deaths[is.na(population1$Total_Deaths)] <- median(population1$Total_Deaths, na.rm = TRUE)
population1

# Replace missing values with the mode (for categorical variables)
get_mode <- function(myval) {
  abc <- unique(myval)}
  #abc[which.max(tabulate(match(myval, abc)))]
}
population1$Sex[is.na(population1$Sex)] <- get_mode(population1$Sex)
tail(population1)

#------------------------

#Research Question 1 (RQ1): COVID-19 Mortality Rates Comparison Across Age Groups
#RQ1. To what extent do COVID-19 mortality rates for middle-aged individuals relate or differ from other age groups?


# Check for missing values and handle them
population <- na.omit(population)  # Optionally, use advanced imputation if needed

# Convert Age Group to factor
population$Age_Group <- factor(population$Age_Group)

# Sample4 is combination of sample1 (middle-age) + sample2 (young age) + sample3 (old age)
sample4$Age_Group <- factor(sample4$Age_Group)

head(sample4)
count(sample1)  # 270 rows
count(sample2)  #  90 rows
count(sample3)  #  90 rows 
count(sample4)  # 450 Rows (270+90+90)


# One-way ANOVA to test differences in COVID-19 deaths by age group
anova_covid <- aov(COVID_19_Deaths ~ Age_Group, data = sample4)
summary(anova_covid)

# Post-hoc analysis if ANOVA is significant
TukeyHSD(anova_covid)

#RQ2. To what extent does the influenza mortality rate for middle-aged individuals relate or differ from other age groups?
# One-way ANOVA to test differences in Influenza deaths by age group
anova_influenza <- aov(Influenza_Deaths ~ Age_Group, data = sample4)
summary(anova_influenza)

# Post-hoc analysis if ANOVA is significant
TukeyHSD(anova_influenza)

#RQ3. To what extent does the pneumonia mortality rate for middle-aged individuals relate or differ from other age groups?
# One-way ANOVA to test differences in Pneumonia deaths by age group
anova_pneumonia <- aov(Pneumonia_Deaths ~ Age_Group, data = sample4)
summary(anova_pneumonia)

# Post-hoc analysis if ANOVA is significant
TukeyHSD(anova_pneumonia)

#RQ4. Do COVID-19, influenza, and pneumonia mortality rates differ by gender for the middle-aged population?

# Load necessary libraries
library(MASS)

# Run MANOVA to test the differences by gender
manova_results <- manova(cbind(COVID_19_Deaths, Influenza_Deaths, Pneumonia_Deaths) ~ Sex, data = sample4)

# Summary of the MANOVA results
summary(manova_results)

# Summary with Pillai's test for multivariate test statistics
summary(manova_results, test = "Pillai")

# Summary with Wilks's test for multivariate test statistics
summary(manova_results, test = "Wilks")

# Summary with Hotelling-Lawley's test for multivariate test statistics
summary(manova_results, test = "Hotelling-Lawley")

# Summary with Roy's test for multivariate test statistics
summary(manova_results, test = "Roy")


# To see the ANOVA table for each dependent variable separately
summary.aov(manova_results)


#RQ5. How do age and gender interact to influence the COVID-19 mortality rate among middle-aged individuals compared to other age groups?

# Fit the two-way ANOVA model with interaction
anova_interaction_covid <- aov(COVID_19_Deaths ~ Age_Group * Sex, data = sample4)

# Summarize the ANOVA results
summary(anova_interaction_covid)

# Checking the interaction plot to visualize interaction effects
interaction.plot(sample4$Age_Group, sample4$Sex, sample4$COVID_19_Deaths,
                 main = "Interaction Plot: Age Group vs Sex on COVID-19 Mortality Rates",
                 xlab = "Age Group", ylab = "COVID-19 Deaths", col = c("blue", "red"), legend = TRUE)

#RQ6. How do age and gender interact to influence the influenza mortality rate among middle-aged individuals compared to other age groups?

# Fit the two-way ANOVA model with interaction
anova_interaction_influenza <- aov(Influenza_Deaths ~ Age_Group * Sex, data = sample4)

# Summarize the ANOVA results
summary(anova_interaction_influenza)

# Checking the interaction plot to visualize interaction effects
interaction.plot(sample4$Age_Group, sample4$Sex, sample4$Influenza_Deaths,
                 main = "Interaction Plot: Age Group vs Sex on Influenza Mortality Rates",
                 xlab = "Age Group", ylab = "Influenza Deaths", col = c("blue", "red"), legend = TRUE)

#RQ7. How do age and gender interact to influence the pneumonia mortality rate among middle-aged individuals compared to other age groups?

# Fit the two-way ANOVA model with interaction
anova_interaction_pneumonia <- aov(Pneumonia_Deaths ~ Age_Group * Sex, data = sample4)

# Summarize the ANOVA results
summary(anova_interaction_pneumonia)

# Checking the interaction plot to visualize interaction effects
interaction.plot(sample4$Age_Group, sample4$Sex, sample4$Pneumonia_Deaths,
                 main = "Interaction Plot: Age Group vs Sex on Pneumonia Mortality Rates",
                 xlab = "Age Group", ylab = "Pneumonia Deaths", col = c("blue", "red"), legend = TRUE)

#RQ8. Are there distinct clusters of states based on COVID-19, influenza, and pneumonia mortality rates for the middle-aged population (30-49 years) in the U.S.?
# Usage of K-Means Cluster and DBSCAN algorithms 
# Select relevant columns: State, COVID-19 Deaths, Influenza Deaths, Pneumonia Deaths
# Filter for middle-aged group (30-49 years) data

population1 # Copy of full population

middle_aged_data <- population1 %>%
  filter( (Age_Group=="30-39 years"  | Age_Group=="35-44 years" | Age_Group=="40-49 years" )
         & State != "United States" 
         & (Sex =="Male" | Sex =="Female")
         & Year %in% c(2020,2021,2022,2023)
         & Month %in% c(1,2,3,4,5,6,7,8,9,10,11,12)
         ) %>%
  dplyr::select(State,Sex,Age_Group, Year, Month, COVID_19_Deaths,Pneumonia_Deaths, Influenza_Deaths, Total_Deaths)

head (middle_aged_data)
tail(middle_aged_data)

# K-means clustering code for COVID-19 mortality 
# Normalize the COVID-19 death rates for clustering
normalize <- function(x) { (x - min(x)) / (max(x) - min(x)) }
middle_aged_data_normalized <- middle_aged_data %>%
  mutate(COVID_19_Deaths = normalize(COVID_19_Deaths))

# Convert data into a matrix for K-means (covid-19 mortality)
covid_matrix <- as.matrix(middle_aged_data_normalized[, "COVID_19_Deaths"])

# Perform K-means clustering with 3 clusters for Covid-19 mortality
set.seed(123)  # for reproducibility
kmeans_result <- kmeans(covid_matrix, centers = 3, nstart = 25)

# Add cluster labels to the original data
middle_aged_data$Cluster <- as.factor(kmeans_result$cluster)

# Visualize the clusters using ggplot2
ggplot(middle_aged_data, aes(x = State, y = COVID_19_Deaths, color = Cluster)) +
  geom_point(size = 4) +
  labs(title = "K-means Clustering of States based on COVID-19 Mortality Rates",
       x = "State",
       y = "COVID-19 Deaths (normalized)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))


# Display the number of clusters and their sizes
table(middle_aged_data$Cluster)

# Summarize the clusters
cluster_summary <- middle_aged_data %>%
  group_by(Cluster) %>%
  summarize(
    COVID_19_Deaths_Mean = mean(COVID_19_Deaths),
    States = paste(unique(State), collapse = ", ")
  )
print(cluster_summary)

# Calculate the total within-cluster sum of squares to evaluate the clustering performance
total_withinss <- kmeans_result$tot.withinss
cat("Total within-cluster sum of squares:", total_withinss, "\n")

# Elbow Method to determine optimal number of clusters (optional)
wss <- sapply(1:10, function(k) {
  kmeans(covid_matrix, k, nstart = 25)$tot.withinss
})

# Plot the Elbow graph
plot(1:10, wss, type = "b", pch = 19, frame = FALSE,
     xlab = "Number of clusters (k)",
     ylab = "Total within-cluster sum of squares",
     main = "Elbow Method for Finding Optimal k")

# K-means clustering code for Influenza mortality 
# Normalize Influenza death rates for clustering
normalize <- function(x) { (x - min(x)) / (max(x) - min(x)) }
middle_aged_data_normalized <- middle_aged_data %>%
  mutate(Influenza_Deaths = normalize(Influenza_Deaths))

# Convert data into a matrix for K-means (Influenza  mortality)
influenza_matrix <- as.matrix(middle_aged_data_normalized[, "Influenza_Deaths"])

# Perform K-means clustering with 3 clusters for Influenza mortality
set.seed(123)  # for reproducibility
kmeans_result <- kmeans(influenza_matrix, centers = 3, nstart = 25)

# Add cluster labels to the original data
middle_aged_data$Cluster <- as.factor(kmeans_result$cluster)

# Visualize the clusters using ggplot2
ggplot(middle_aged_data, aes(x = State, y = Influenza_Deaths, color = Cluster)) +
  geom_point(size = 4) +
  labs(title = "K-means Clustering of States based on Influenza Mortality Rates",
       x = "State",
       y = "Influenza Deaths (normalized)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))


# Display the number of clusters and their sizes
table(middle_aged_data$Cluster)

# Summarize the clusters
cluster_summary <- middle_aged_data %>%
  group_by(Cluster) %>%
  summarize(
    Influenza_Deaths_Mean = mean(Influenza_Deaths),
    States = paste(unique(State), collapse = ", ")
  )
print(cluster_summary)

# Calculate the total within-cluster sum of squares to evaluate the clustering performance
total_withinss <- kmeans_result$tot.withinss
cat("Total within-cluster sum of squares:", total_withinss, "\n")

# Elbow Method to determine optimal number of clusters (optional)
wss <- sapply(1:10, function(k) {
  kmeans(influenza_matrix, k, nstart = 25)$tot.withinss
})

# Plot the Elbow graph
plot(1:10, wss, type = "b", pch = 19, frame = FALSE,
     xlab = "Number of clusters (k)",
     ylab = "Total within-cluster sum of squares",
     main = "Elbow Method for Finding Optimal k")

# K-means clustering code for Pneumonia mortality 
# Normalize the Pneumonia death rates for clustering
normalize <- function(x) { (x - min(x)) / (max(x) - min(x)) }
middle_aged_data_normalized <- middle_aged_data %>%
  mutate(Pneumonia_Deaths = normalize(Pneumonia_Deaths))

# Convert data into a matrix for K-means (Pneumonia mortality)
pneumonia_matrix <- as.matrix(middle_aged_data_normalized[, "Pneumonia_Deaths"])

# Perform K-means clustering with 3 clusters for Pneumonia mortality
set.seed(123)  # for reproducibility
kmeans_result <- kmeans(pneumonia_matrix, centers = 3, nstart = 25)

# Add cluster labels to the original data
middle_aged_data$Cluster <- as.factor(kmeans_result$cluster)

# Visualize the clusters using ggplot2
ggplot(middle_aged_data, aes(x = State, y = Pneumonia_Deaths, color = Cluster)) +
  geom_point(size = 4) +
  labs(title = "K-means Clustering of States based on Pneumonia Mortality Rates",
       x = "State",
       y = "Pneumonia Deaths (normalized)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))


# Display the number of clusters and their sizes
table(middle_aged_data$Cluster)

# Summarize the clusters
cluster_summary <- middle_aged_data %>%
  group_by(Cluster) %>%
  summarize(
    Pneumonia_Deaths_Mean = mean(Pneumonia_Deaths),
    States = paste(unique(State), collapse = ", ")
  )
print(cluster_summary)

# Calculate the total within-cluster sum of squares to evaluate the clustering performance
total_withinss <- kmeans_result$tot.withinss
cat("Total within-cluster sum of squares:", total_withinss, "\n")

# Elbow Method to determine optimal number of clusters (optional)
wss <- sapply(1:10, function(k) {
  kmeans(pneumonia_matrix, k, nstart = 25)$tot.withinss
})

# Plot the Elbow graph
plot(1:10, wss, type = "b", pch = 19, frame = FALSE,
     xlab = "Number of clusters (k)",
     ylab = "Total within-cluster sum of squares",
     main = "Elbow Method for Finding Optimal k")

