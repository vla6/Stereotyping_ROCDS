
require(fastDummies)

get_pairwise_cor <- function(data, method = 'pearson') {
  return(data.frame(cor = cor(data[1], data[2],
                              method  = method,
                              use='pairwise.complete.obs')))
}

get_all_cor <- function(data, com_cor_feature = female,
                        method='pearson',
                        keep_cols = NULL) {
  
  cor_quo <- enquo(com_cor_feature)
  cor_quo_nm <- quo_name(cor_quo)
  
  if (!is.null(keep_cols)) {
    data <- data %>%
      dplyr::select(!!cor_quo, one_of(keep_cols))
  }
  
  # Convert input feature if needed
  data_fact_com_cor_feature <- data %>%
    dplyr::select(!!cor_quo) %>%
    mutate_if(negate(is.numeric), 
              function(x) as.numeric(as.character(x)))
  
  # Convert factors to dummy features
  data_fact_dum_1 <- data %>%
    dplyr::select(-!!cor_quo) %>%
    dplyr::select_if(is.factor) 
  
  if (ncol(data_fact_dum_1) > 0) {
    data_fact_dum <-data_fact_dum_1 %>%
      fastDummies::dummy_cols(remove_first_dummy = T,
               remove_selected_columns =T)
  } else {
    data_fact_dum <- data.frame(remove_temp_column = seq(1, nrow(data_fact_dum_1)))
  }
  
  # Recombine data set
  data_num <- data %>%
    dplyr::select(-!!cor_quo) %>%
    dplyr::select_if(is.numeric) %>%
    bind_cols(data_fact_com_cor_feature) %>%
    bind_cols(data_fact_dum)
  
  data_num_long <- data_num %>%
    pivot_longer(cols = names(data_num)[!names(data_num)  %in% c(cor_quo_nm, 'remove_temp_column')]) 
  
  num_cors <- data_num_long %>%
    dplyr::select(!!cor_quo, value, name) %>%
    group_by(name) %>%
    do(get_pairwise_cor(., method=method)) %>%
    ungroup() %>%
    arrange(desc(abs(value)))
  
  return(num_cors)
  
}
