# we can do an example to simulation in R
# Simulating a simple linear regression model in R
# Load necessary library
library(ggplot2)
# Set seed for reproducibility
set.seed(123)
# Define parameters
n <- 100  # number of observations
beta_0 <- 2  # intercept
beta_1 <- 3  # slope
sigma <- 1  # standard deviation of the error term
# Simulate predictor variable (X) 
X <- rnorm(n, mean = 5, sd = 2)
# Simulate error term (epsilon)
epsilon <- rnorm(n, mean = 0, sd = sigma)
# Generate response variable (Y) using the linear model
Y <- beta_0 + beta_1 * X + epsilon
# Create a data frame to hold the simulated data
data <- data.frame(X = X, Y = Y)
# Fit a linear regression model to the simulated data   
model <- lm(Y ~ X, data = data)
# Display the summary of the model
summary(model)
# Plot the simulated data and the fitted regression line
ggplot(data, aes(x = X, y = Y)) +
  geom_point() +
  geom_smooth(method = "lm", col = "blue") +
  labs(title = "Simulated Linear Regression Model",
       x = "Predictor Variable (X)",
       y = "Response Variable (Y)") +
  theme_minimal() 
# End of the simulation code
  
