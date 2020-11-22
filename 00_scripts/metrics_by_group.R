#
# Functions used to calculate metrics by group,
# including AUC, ROC curves, DIFR
#

#
# (PR)-AUC ----
# Get the ROC- and PR-AUCs per group from a data set with actual outcomes
# and predicted probabilities.
#
get_aucs <- function(data) {
  
  
  aucpr <- PRROC::pr.curve(scores.class0 =data$p1,
                           weights.class0 = 
                             as.numeric(as.character(data$bad_loan)))
  
  auc <- PRROC::roc.curve(scores.class0 =data$p1,
                          weights.class0 = 
                            as.numeric(as.character(data$bad_loan)))
  
  return(data.frame(auc = auc$auc,
                    aucpr = aucpr$auc.integral))
}

#
# ROC and PR curves ----
# Get curve data set, no plotting
#
get_roc_curves <- function(data) {
  
  aucpr <- PRROC::pr.curve(scores.class0 =data$p1,
                           weights.class0 = 
                             as.numeric(as.character(data$bad_loan)),
                           curve = T)
  
  auc <- PRROC::roc.curve(scores.class0 =data$p1,
                          weights.class0 = 
                            as.numeric(as.character(data$bad_loan)),
                          curve = T)
  
  curve_df <- aucpr$curve %>%
    as.data.frame() %>%
    setNames(c('x', 'y', 'threshold')) %>%
    mutate(type = 'aucpr') %>%
    bind_rows(auc$curve %>%
                as.data.frame() %>%
                setNames(c('x', 'y', 'threshold')) %>%
                mutate(type = 'auc'))
  
  return(curve_df)
}

#
# Plot ROC and PR curves ----
# Expects data from get_roc_curves, with a 
# gender feature to display
#

plot_roc_curves <- function(data,  gender_var = gender_str,
                            outfile_prefix = NULL,
                            title_str = NULL) {
  
  gender_quo = enquo(gender_var)
  
  gp_roc_auc_sex <- roc_curve_df %>%
    dplyr::filter(type == 'auc') %>%
    ggplot(aes(x = x, y = y, color = !!gender_quo)) +
    geom_line() +
    theme_minimal(base_size = 16) +
    scale_color_manual(values=c('coral3', 'turquoise')) +
    labs(x = 'false positive rate', y='sensitivity', color=NULL,
         title = title_str) +
    theme(legend.position = c(0.8, 0.3),
          legend.background = element_rect(fill='white', color='gray'))
  
  print(gp_roc_auc_sex)
  

  # PR-AUC
  gp_roc_aucpr_sex <- roc_curve_df %>%
    dplyr::filter(model == this_model &
                    type == 'aucpr') %>%
    ggplot(aes(x = x, y = y, color = !!gender_quo)) +
    geom_line() +
    theme_minimal(base_size = 16) +
    scale_color_manual(values=c('coral3', 'turquoise')) +
    labs(x = 'recall', y='precision', color=NULL,
         title = title_str) +
    theme(legend.position = c(0.8, 0.8),
          legend.background = element_rect(fill='white', color='gray'))
  
  print(gp_roc_aucpr_sex)
  
  if (!is.null(outfile_prefix)) {
    ggsave(paste0(outfile_prefix, '_ROC.png'),
           gp_roc_auc_sex, type='cairo', width=4, height=4)
    ggsave(paste0(outfile_prefix, '_PR.png'),
           gp_roc_aucpr_sex, type='cairo', width=4, height=4)
  }
}

#
# DIFR ----
# Dispersion Index for Risk 
# Function to cacluate DIFR for data with low, med, high risk buckets
# 
get_difr <- function(data, risk_grp_var = risk_grp) {
  
  risk_grp_quo = enquo(risk_grp_var)
  
  # Get overall log odds
  lo_ov <- data %>%
    dplyr::summarize(base_rate_overall = mean(bad_loan == '1'),
                     n_overall = n()) %>%
    mutate(log_odds_overall = log(base_rate_overall / (1-base_rate_overall)))
  
  lo_grp <- data %>%
    group_by(!!risk_grp_quo) %>%
    dplyr::summarize(base_rate_grp = mean(bad_loan == '1'),
                     n_grp = n()) %>%
    ungroup() %>%
    mutate(log_odds_grp = log(base_rate_grp / (1-base_rate_grp)))
  
  # difr calclation
  difr_grp <- lo_grp %>%
    crossing(lo_ov) %>%
    mutate(difr_term = ((log_odds_overall - log_odds_grp)**2) * (n_grp / n_overall)) %>%
    dplyr::summarize(difr_term = sum(difr_term)) %>%
    mutate(difr = sqrt(difr_term))
  
  
  return(data.frame(difr = difr_grp$difr))
  
}


#
# Calibration ----
# Plots actual defaults vs. risk quantiles
# Can limit models using the "models" argument
#
plot_calibration_curves <- function(data, 
                                    quantile_var = risk_decile,
                                    models = NULL,
                                    outfile = NULL) {
  
  quantile_quo = enquo(quantile_var)
  
  if (is.null(models)) {
    models = unique(data$model)
  }
  
  levels_plot_orig = levels(data %>% pull(!!quantile_quo))  
  levels_plot = seq(1, length(levels_plot_orig))
  
  gp_cal <- data %>%
    dplyr::filter(model %in% models) %>%
    group_by(model, female, !!quantile_quo) %>%
    dplyr::summarize(bad_loan_rate = mean(bad_loan == '1')) %>%
    ungroup() %>%
    ggplot(aes(x=!!quantile_quo, y= bad_loan_rate, group=female,
               color=female)) +
    geom_line() +
    theme_minimal(base_size = 14) +
    facet_wrap(~model, labeller = label_both) +
    labs(y= 'actual bad loan rate',
         x = 'predicted risk quantile') +
    scale_y_continuous(labels=scales::percent) +
    scale_x_discrete(breaks=levels_plot_orig,
                     labels = levels_plot) +
    theme(legend.position = 'right') +
    scale_color_manual(values=c('turquoise', 'coral3'))
  
  print(gp_cal)
  
  if (!is.null(outfile)) {
    ggsave(outfile,
           gp_cal, type='cairo', width=9, height=4)
  }
}
