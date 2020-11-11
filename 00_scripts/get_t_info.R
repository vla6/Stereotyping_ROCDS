#
# Function to take in a data table with sex fields and compare
# means using a t test
#

get_t_info <- function(data, 
                       metric_field = phi,
                       gender_var = female,
                       alternative = 'greater',
                       gender_levels = NULL) {
  
  gender_quo = enquo(gender_var)
  
  if (is.null(gender_levels)) {
    gender_levels = c('1', '0')
  }
  
  this_x = data  %>%
    dplyr::filter(!!gender_quo == gender_levels[1]) %>%
    pull(phi)
  
  this_y = data %>%
    dplyr::filter(!!gender_quo == gender_levels[2]) %>%
    pull(phi)
  
  t_ret = tryCatch(t.test(this_x, this_y, alternative = alternative),
                   error = function(e) {NULL})
  
  if(is.null(t_ret) | length(t_ret) == 0) {
    out_df <- data.frame(t_statistic = NA,
                         t_parameter = NA,
                         t_p_value = NA)
  } else{
    out_df <- data.frame(t_statistic = t_ret$statistic,
                         t_parameter = t_ret$parameter,
                         t_p_value = t_ret$p.value)
  }
  
  return(out_df)
}

get_t_info_one_sided <- function(data, 
                       metric_field = phi,
                       alternative = 'greater') {
  this_x = data  %>%
    pull(phi)
  
  
  t_ret = tryCatch(t.test(this_x, NULL, alternative = alternative),
                   error = function(e) {NULL})
  
  if(is.null(t_ret) | length(t_ret) == 0) {
    out_df <- data.frame(t_statistic = NA,
                         t_parameter = NA,
                         t_p_value = NA)
  } else{
    out_df <- data.frame(t_statistic = t_ret$statistic,
                         t_parameter = t_ret$parameter,
                         t_p_value = t_ret$p.value)
  }
  
  return(out_df)
}
