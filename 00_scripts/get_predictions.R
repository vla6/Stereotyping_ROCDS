# Function to get predictions for arbitrary data
get_predictions <- function(data, model) {
  data_pred <-h2o.predict(model, newdata = data %>% as.h2o()) %>%
    as.data.frame() %>%
    bind_cols(data %>% dplyr::select(ID, bad_loan, 
                                     any_of(c('female', 'female_pq'))))
  return(data_pred)
}
