library(keras)

model <- keras_model_sequential() %>% 
  layer_embedding(input_dim = num_diagnoses + 1, output_dim = 40) %>% 
  bidirectional(
    layer_lstm(units = 32, dropout = 0.2, recurrent_dropout = 0.2)
  ) %>% 
  layer_dense(units = 1, activation = "sigmoid")

model %>% compile(
  optimizer = "rmsprop",
  loss = "binary_crossentropy",
  metrics = c("acc")
)

history <- model %>% fit(
  input_train, y_train,
  epochs = 12,
  batch_size = 24,
  validation_split = 0.2
)
