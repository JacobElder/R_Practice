OF <- c(0,20,40,60,80,100)
class(OF)
OF <- ordered(c(0,20,40,60,80,100))
ordered(OF)

OF <- c(0,20,40,60,80,100)

# Set seed for reproducibility
set.seed(150)

# Number of observations
n <- 300
e <- rnorm(n)
x0 <- rnorm(n)
OF <- sample(OF, 300, replace=T)

# Simulate outcome variable
outcome <- rnorm(n,0) + .02*OF + rnorm(n)

## AS NUMERIC

# Fit linear regression model
model_Numeric <- lm(outcome ~ OF)

# Print model summary
summary(model_Numeric)

## AS FACTOR

# Fit linear regression model
model_Factor <- lm(outcome ~ ordered(OF))

# Print model summary
summary(model_Factor)
