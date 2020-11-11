#
# Function to plot Shapley data for a
# subset of models and sexes
#

plot_model_phi_side_by_side <- function(data, models,
                                    female_val,
                                    female_var = female,
                                    outfile =NULL) {
  
  female_quo = enquo(female_var)
  sel_data <- data %>%
    dplyr::filter(model %in% models &
                    !!female_quo == female_val) %>%
    arrange(phi_rank) %>%
    mutate(feature = as_factor(feature))
  
  # Shap plot
  shap_plot_df <- sel_data %>%
    ggplot(aes(x=fct_rev(feature), y=phi_mean)) +
    geom_col(position=position_dodge(), fill='lightblue') +
    coord_flip() +
    theme_minimal(base_size = 14) +
    facet_wrap(~model) +
    labs(x=NULL, y = 'mean phi')
  
  print(shap_plot_df)
  
  if (!is.null(outfile)) {
    
    ggsave(outfile, shap_plot_df, 
           type='cairo', width = 9, height=4)
  }
  
  return(shap_plot_df)
  
}
