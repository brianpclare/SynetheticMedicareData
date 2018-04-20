library(keras)

model <- keras_model_sequential() %>% 
  layer_embedding(input_dim = num_diagnoses + 1, output_dim = 60) %>% 
  layer_lstm(units = 32, dropout = 0.2, recurrent_dropout = 0.2, return_sequences = TRUE) %>% 
  layer_lstm(units = 64, dropout = 0.2, recurrent_dropout = 0.3) %>% 
  layer_dense(units = 1, activation = "sigmoid")

model %>% compile(
  optimizer = optimizer_rmsprop(lr = 0.005),
  loss = "binary_crossentropy",
  metrics = c("acc")
)

history <- model %>% fit(
  input_train, y_train,
  epochs = 10,
  batch_size = 16,
  validation_split = 0.2
)

summary(model)
