rm(list=ls())
gc()
library(keras)
library(tidyverse)
library(dplyr)
library(rsample)
library(tensor)
load("~/smote_final.RData")
data_onehot <- model.matrix(~.,smoteif_data %>% as.data.frame())
data_onehot <- as.data.frame(data_onehot)

set.seed(123)
split <- rsample::initial_split(data_onehot, prop = .7, strata = "KSC01_onset")
train <- rsample::training(split)
test  <- rsample::testing(split)

train_x <- train %>% dplyr::select(-KSC01_onset)
mean    <- colMeans(train_x)
std     <- apply(train_x, 2, sd)
train_x <- scale(train_x, center = mean, scale = std)

test_x <- test %>% dplyr::select(-KSC01_onset)
test_x <- scale(test_x, center = mean, scale = std)
train_y <- (train$KSC01_onset)
test_y  <- (test$KSC01_onset)

zv <- which(colSums(is.na(train_x)) > 0, useNames = FALSE)
train_x <- train_x[, -zv]
test_x  <- test_x[, -zv]


model <- keras_model_sequential() %>%
  
  # network architecture
  layer_dense(units = 10, activation = "relu", input_shape = ncol(train_x)) %>%
  layer_dense(units = 5, activation = "relu") %>%
  layer_dense(units = 1) %>%
  
  # backpropagation
  compile(
    optimizer = "rmsprop",
    loss = "mse",
    metrics = c("mae")
  )
summary(model)
# train our model
learn <- model %>% fit(
  x = train_x,
  y = train_y,
  epochs = 100,
  batch_size = 32,
  validation_split = .2
)
