library(keras)

model <- keras_model_sequential() %>% 
  layer_embedding(input_dim = num_diagnoses + 1, output_dim = 32) %>% 
  layer_lstm(units = 32) %>% 
  layer_dense(units = 1, activation = "sigmoid")

model %>% compile(
  optimizer = "rmsprop",
  loss = "binary_crossentropy",
  metrics = c("acc")
)

history <- model %>% fit(
  input_train, y_train,
  epochs = 10,
  batch_size = 20,
  validation_split = 0.2
)
