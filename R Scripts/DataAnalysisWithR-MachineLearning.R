# Machine Learning in R

# Required packages for the module
library(tidyverse)
library(MASS)
library(Metrics)


# Load the Boston data set from the MASS package
# and separate the data in testing and training 
# data sets
data("Boston")

set.seed(1)
train_rownumbers <- sample(nrow(Boston), size = trunc(0.8*nrow(Boston)))

train_dataset <- 
  Boston %>%
  filter(row_number() %in% train_rownumbers) %>%
  dplyr::select(medv, crim, rm, tax, lstat)

test_dataset <- 
  Boston %>% 
  filter(!row_number() %in% train_rownumbers) %>%
  dplyr::select(medv, crim,rm,tax,lstat) 


# Create a model that predicts median house prices using the
# crim, rm, tax, and lstat variables from the training data set
# created in the previous step
model <- lm(medv ~ crim+rm+tax+lstat, data = train_dataset)
summary(model)


# Score the data held back in the test_dataset to see how well
# the model generalizes to new data
data_to_score <- dplyr::select(test_dataset, crim, rm, tax, lstat)
pred_medv <- predict(model, data_to_score)
pred_medv[0:5]


# Add the predictions to the test_dataset data frame
final_output <- cbind(test_dataset, pred_medv)
final_output[0:5,]


# Calculate the RMSE (root mean square error)
rmse_stat <- rmse(final_output$medv, final_output$pred_medv)
rmse_stat