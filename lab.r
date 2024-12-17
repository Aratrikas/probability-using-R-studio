# Initialize the data
x <- c(22, 26, 29, 30, 31, 31, 34, 35)
y <- c(20, 20, 21, 29, 27, 24, 27, 31)
data <- data.frame(x, y)

# Variance, Covariance, and Standard Deviation
variance_x <- var(data$x)
variance_y <- var(data$y)
covariance_xy <- cov(data$x, data$y)
sd_x <- sd(data$x)
sd_y <- sd(data$y)

cat("Variance of x:", variance_x, "\n")
cat("Variance of y:", variance_y, "\n")
cat("Covariance of x and y:", covariance_xy, "\n")
cat("Standard deviation of x:", sd_x, "\n")
cat("Standard deviation of y:", sd_y, "\n")

# Linear Regression
model <- lm(y ~ x, data = data)
summary(model)




# Define the x and y values
x <- c(22, 26, 29, 30, 31, 31, 34, 35)
y <- c(20, 20, 21, 29, 27, 24, 27, 31)

# Create a data frame
data <- data.frame(x, y)

# Plot the data
plot(data)
plot(x, y, main = "Scatter Plot of x vs y", xlab = "x", ylab = "y", pch = 19)

# Fit the linear model
a1 <- lm(y ~ x)

# Add the regression line to the plot
abline(a1, col = "blue", lwd = 2)

# Calculate covariance between x and y
b1 <- cov(x, y)

# Print the covariance
b1

# Display the linear model summary
summary(a1)

# Predict y for a new x value
x1 <- data.frame(x = c(38))
predicted_value <- predict(a1, x1)

# Print the predicted value
predicted_value



# Create your data frame
x <- c(22, 26, 29, 30, 31, 31, 34, 35)
y <- c(20, 20, 21, 29, 27, 24, 27, 31)
data <- data.frame(x, y)

# Perform linear regression
model <- lm(y ~ x, data = data)

# Print the summary of the model
summary(model)

















# Binomial Distribution
n <- 10
p <- 0.5
x_binom <- 0:n
binom_prob <- dbinom(x_binom, size = n, prob = p)
plot(x_binom, binom_prob, type = "h", lwd = 2, col = "blue",
     main = "Binomial Distribution",
     xlab = "Number of Successes", ylab = "Probability")

# Poisson Distribution
lambda <- 3
x_pois <- 0:10
poisson_prob <- dpois(x_pois, lambda)
plot(x_pois, poisson_prob, type = "h", lwd = 2, col = "red",
     main = "Poisson Distribution",
     xlab = "Number of Events", ylab = "Probability")

# Normal Distribution
mu <- 0
sigma <- 1
x_norm <- seq(-10, 10, length = 100)
normal_prob <- dnorm(x_norm, mean = mu, sd = sigma)
plot(x_norm, normal_prob, type = "l", lwd = 2, col = "green",
     main = "Normal Distribution",
     xlab = "Value", ylab = "Density")

# Exponential Distribution
lambda_exp <- 1
x_exp <- seq(0, 10, length = 100)
exp_prob <- dexp(x_exp, rate = lambda_exp)
plot(x_exp, exp_prob, type = "l", lwd = 2, col = "purple",
     main = "Exponential Distribution",
     xlab = "Value", ylab = "Density")





# Load the iris dataset
data(iris)

# Extract Sepal.Length for setosa and versicolor
setosa_sepal_length <- iris$Sepal.Length[iris$Species == "setosa"]
versicolor_sepal_length <- iris$Sepal.Length[iris$Species == "versicolor"]

# Perform t-test
t_test_result <- t.test(setosa_sepal_length, versicolor_sepal_length)

# Print the result
print(t_test_result)






# Create the vector
d <- c(2, 10, 8, 4)

# Create the matrix with the correct parameter
m <- matrix(d, ncol = 2, byrow = TRUE)

# Perform the Chi-Squared test
chi_squared_test_result <- chisq.test(m)

# Print the result
print(chi_squared_test_result)

# Table 1
table1 <- matrix(c(10, 20, 30, 40), ncol = 2)

# Table 2
table2 <- matrix(c(15, 25, 35, 45), ncol = 2)

# Combine the tables into a larger contingency table
combined_table <- rbind(table1, table2)

# Perform the Chi-Squared test
chi_squared_test_result <- chisq.test(combined_table)

# Print the result
print(chi_squared_test_result)

# Load the iris dataset
data(iris)

# Discretize Sepal.Length into categorical variables (e.g., "short", "medium", "long")
iris$Sepal.Length.Category <- cut(iris$Sepal.Length,
                                  breaks = quantile(iris$Sepal.Length, probs = seq(0, 1, 0.33)),
                                  labels = c("short", "medium", "long"),
                                  include.lowest = TRUE)

# Create a contingency table for Sepal.Length.Category
sepal_length_table <- table(iris$Sepal.Length.Category)

# Create a contingency table for Species
species_table <- table(iris$Species)

# Combine the tables into a larger contingency table
combined_table <- rbind(sepal_length_table, species_table)

# Perform the Chi-Squared test
chi_squared_test_result <- chisq.test(combined_table)

# Print the result
print(chi_squared_test_result)


# First example
x <- c(5, 4, 3, 7, 5, 1, 3, 4, 1, 7)
y <- c('A', 'B', 'C', 'A', 'C', 'A', 'B', 'A', 'B', 'B')
df <- data.frame(x, y)
summary(aov(x ~ y, data = df)) # Perform ANOVA
# Output indicates p-value > LOS (Accept Ho)

# Second example
x <- c(5, 7, 6, 10, 3, 6, 11, 5, 12, 4, 7, 4, 1)
y <- c('A', 'B', 'C', 'A', 'B', 'C', 'A', 'B', 'A', 'B', 'C', 'A', 'B')
df <- data.frame(x, y)
summary(aov(x ~ y, data = df)) # Perform ANOVA
# Output indicates p-value < LOS (Reject Ho)

# Third example
x <- c(1, 7, 2, 2, 6, 1, 6, 2, 1, ...)
y1 <- c('B1', 'B2', 'B3', 'B4', 'B5', ...)
y2 <- c('R1', 'R2', 'R3', 'R1', 'R3', ...)
df <- data.frame(x, y1, y2)
summary(aov(x ~ y1 + y2, data = df)) # Perform ANOVA with multiple factors
# Output indicates p-values for y1 and y2

# Fourth example
x <- c(16, 17, 20, 16, 21, 15, 15, 12, 13)
y1 <- c('E1', 'E2', 'E3', ...)
y2 <- c('L1', 'L2', 'L3', ...)
y3 <- c('B1', 'B2', 'B3', ...)
df <- data.frame(x, y1, y2, y3)
summary(aov(x ~ y1 + y2 + y3, data = df)) # Perform ANOVA
# Output indicates p-values for y1, y2, y3



# Load the iris dataset
data(iris)

# Discretize Sepal.Width into categorical variables (e.g., "narrow", "medium", "wide")
iris$Sepal.Width.Category <- cut(iris$Sepal.Width,
                                 breaks = quantile(iris$Sepal.Width, probs = seq(0, 1, 0.33)),
                                 labels = c("narrow", "medium", "wide"),
                                 include.lowest = TRUE)

# Perform two-way ANOVA
anova_result <- aov(Sepal.Length ~ Species + Sepal.Width.Category, data = iris)

# Print the summary of the ANOVA result
summary(anova_result)


# Define the x values and corresponding probabilities
x <- c(1, 2, 3, 4)
p <- c(0.4, 0.3, 0.2, 0.1)

# A is (x > 1/2 and x < 7/2) which means x can be 2, 3, 4
A <- (x > 1/2 & x < 7/2)

# B is (x > 1) which means x can be 2, 3, 4
B <- (x > 1)

# Calculate P(A), P(B), and P(A and B)
P_A <- sum(p[A])
P_B <- sum(p[B])
P_A_and_B <- sum(p[A & B])

# Calculate P(A|B)
P_A_given_B <- P_A_and_B / P_B

# Output the result
P_A_given_B

# Define the x values and corresponding probabilities
x <- c(1, 2, 3, 4)
p <- c(0.4, 0.3, 0.2, 0.1)

# Sum of all probabilities
total_sum <- sum(p)

# Output the result
total_sum

# Define the x values and corresponding probabilities
x <- c(1, 2, 3, 4)
p <- c(0.4, 0.3, 0.2, 0.1)

# Find the sum of probabilities where x >= 1 and x <= 8
valid_x <- (x >= 1 & x <= 8)

# Sum the probabilities for valid x values
sum_probabilities <- sum(p[valid_x])

# Output the result
sum_probabilities

# Define the x values and corresponding probabilities
x <- c(1, 2, 3, 4)
p <- c(0.4, 0.3, 0.2, 0.1)

# A is probability where x = -3 (not in the given set, so P(A) = 0)
A <- (x == -3)
P_A <- sum(p[A])  # This will be 0

# B is probability where x < 0 (no such values in the given set, so P(B) = 0)
B <- (x < 0)
P_B <- sum(p[B])  # This will also be 0

# Conditional probability P(A|B) is undefined since P(B) = 0
if (P_B == 0) {
  P_A_given_B <- NA  # Undefined
} else {
  P_A_given_B <- P_A / P_B
}

# Output the result
P_A_given_B




# Install necessary packages
install.packages("dplyr")
install.packages("ggplot2")

# Load the libraries
library(dplyr)
library(ggplot2)

# Creating the dataset
emp_data <- data.frame(
  empid = 1:10, 
  age = c(25, 30, 28, 40, 35, 50, 29, 27, 42, 34),
  gender = c("Male", "Female", "Female", "Male", "Female", 
             "Male", "Female", "Male", "Female", "Male"),
  status = c("Single", "Married", "Single", "Married", "Single", 
             "Divorced", "Single", "Married", "Single", "Divorced")
)

print(emp_data)
# Convert 'gender' and 'status' to factors
emp_data$gender <- as.factor(emp_data$gender)
emp_data$status <- as.factor(emp_data$status)

str(emp_data)

male_emp_data <- emp_data %>% filter(gender == "Male")


print(male_emp_data)
# One-way table for gender
gender_table <- table(emp_data$gender)
print(gender_table)

# One-way table for status
status_table <- table(emp_data$status)
print(status_table)

# Two-way table for gender and status
gender_status_table <- table(emp_data$gender, emp_data$status)
print(gender_status_table)

# Bar plot for gender distribution
ggplot(emp_data, aes(x = gender)) +
  geom_bar(fill = "skyblue") +
  ggtitle("Gender Distribution of Employees") +
  xlab("Gender") +
  ylab("Count")

# Grouped bar plot for gender and status
ggplot(emp_data, aes(x = gender, fill = status)) +
  geom_bar(position = "dodge") +
  ggtitle("Gender vs Status of Employees") +
  xlab("Gender") +
  ylab("Count") +
  scale_fill_brewer(palette = "Set2")


# Basic plot: Scatter plot of empid vs age
plot(emp_data$empid, emp_data$age, 
     main = "Scatter Plot of Employee ID vs Age", 
     xlab = "Employee ID", 
     ylab = "Age", 
     col = "blue", 
     pch = 19)

# Bar plot of gender
barplot(table(emp_data$gender), 
        main = "Bar Plot of Gender Distribution", 
        col = "lightblue", 
        xlab = "Gender", 
        ylab = "Count")

# Pie chart for marital status
status_counts <- table(emp_data$status)
pie(status_counts, 
    main = "Pie Chart of Marital Status", 
    col = rainbow(length(status_counts)))

# Box plot of age distribution by gender
boxplot(age ~ gender, data = emp_data, 
        main = "Box Plot of Age by Gender", 
        xlab = "Gender", 
        ylab = "Age", 
        col = "lightgreen")



# Calculate the mean of the Sepal.Length column
mean_sepal_length <- median(iris$Sepal.Length)

# Print the result
print(mean_sepal_length)



# Load the ggplot2 package
library(ggplot2)

# Create data frame from the vectors
data <- data.frame(x = c(22, 26, 29, 30, 31, 31, 34, 35),
                   y = c(20, 20, 21, 29, 27, 24, 27, 31),
                   z = c(22, 26, 29, 30, 31, 31, 34, 35))

# Convert data to long format for ggplot2
data_long <- gather(data, key = "variable", value = "value", -x)

# Scatter plot using ggplot2
ggplot(data_long, aes(x = x, y = value, color = variable)) +
  geom_point(size = 3) +
  labs(title = "Scatter Plot of x vs y and x vs z",
       x = "x-axis",
       y = "y/z-axis",
       color = "Variable") +
  theme_minimal()











# Load the ToothGrowth dataset
data("ToothGrowth")

# Convert the 'supp' column to factor
ToothGrowth$supp <- as.factor(ToothGrowth$supp)

# View the structure of the dataset to confirm 'supp' is a factor
str(ToothGrowth)

# Plot a pie chart for the 'supp' factor
supp_counts <- table(ToothGrowth$supp)
pie(supp_counts, main = "Pie Chart of Supplement Types", labels = names(supp_counts))

# Create a table between 'supp' and 'len'
table_supp_len <- table(ToothGrowth$supp, ToothGrowth$len)

# Create a table between 'supp' and 'dose'
table_supp_dose <- table(ToothGrowth$supp, ToothGrowth$dose)

# Print the tables
table_supp_len
table_supp_dose
pie(supp_counts)
