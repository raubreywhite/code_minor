library(tensorflow)

# Define the exponential learning rate decay function dependent on the current step
optimizer_exp_decay <- function() {
  global_step <- tf$contrib$framework$get_or_create_global_step()
  learning_rate <- tf$train$exponential_decay(
    learning_rate = 0.1,
    global_step = global_step,
    decay_steps = 100,
    decay_rate = 0.001)
  return(tf$train$AdagradOptimizer(learning_rate=learning_rate))
}

data("iris")

set.seed(4)
train_inds <- sample(1:nrow(iris),100)
test_inds <- 1:nrow(iris)
test_inds <- test_inds[!test_inds %in% train_inds]

dataX <- as.matrix(iris[,1:4])
dataY <- as.numeric(iris$Species)-1
dataY <- as.integer(dataY)

temp_model_dir <- tempfile()
dir.create(temp_model_dir)

# Infer the real-valued feature columns
feature_columns <- tf$contrib$learn$infer_real_valued_columns_from_input(dataX)

# Initialize a DNN classifier with hidden units 10, 15, 10 in each layer
classifier <- tf$contrib$learn$DNNClassifier(
  feature_columns = feature_columns,
  hidden_units = c(10L, 15L, 10L),
  n_classes = 3L,
  model_dir = temp_model_dir,
  optimizer = optimizer_exp_decay)

# Train a DNN Classifier
classifier$fit(dataX[train_inds, ], dataY[train_inds], steps = 100)

# Generate predictiosn on new data
predictions <- classifier$predict(dataX[test_inds, ])
# The predictions are iterators by default in Python API so we call iterate() to collect them
predictions <- iterate(predictions)
accuracy <- sum(predictions == dataY[test_inds]) / length(predictions)
print(paste0("The accuracy is ", accuracy))

