#
# Shapley explanations for stereotype assessments.
# Analyzes models P-S only.
#
# Look at the Shapley values for females
# vs. a foil consisting of the males. Use
# validation data.
#
# THIS SCRIPT IS VERY SLOW!  Adjust sample
# count (kNumSamps) downwards to run faster,
# with less reliability.
#

rm(list=ls())
source('00_setup.R')

library(h2o)
library(iml)

# Number of cases to sample (exact Shapley is slow)
kNumSamps <- 300
kNumSampsFoil <- 2500

# Max features to show on plots (simpify plots)
kMaxFeat <- 15 # Higher as female_pq not very important overall

# Exempt model with different female definitions
kModelInclude <- c('p', 'q', 'r', 's', 't')

#
# Import data  ----
#

h2o.init()
h2o.removeAll()

model_id_df <- fread(file.path(kOutputDir, '/02_REPORT_model_ids.csv'))
model_list = model_id_df$model[model_id_df$model %in%kModelInclude]
model_list_h2o <- sapply(model_list, 
                         function(x) model_load(x, model_id_df), USE.NAMES = T)

split_id_val <- readRDS(file.path(kOutputDir, '/01_DATA_split_id_val.rds'))
data_val <- readRDS(file.path(kOutputDir, '/01_DATA_base_gender_inf.rds')) %>%
  semi_join(split_id_val, by='ID')

predictors_df <- readRDS(file.path(kOutputDir, '02_DATA_predictors.rds')) %>%
  dplyr::filter(model %in% kModelInclude)

predictions_validation <- readRDS(file.path(kOutputDir, '02_DATA_predictions.rds'))  %>%
  dplyr::filter(model %in% kModelInclude)


#
# Sample data ----
# Get a foil consisting of randomly selected males,
# and sample data stratified by gender.  We are 
# intersted in females but look at makes also
# to verify overall low values.
#

# Sample data, split by "gender"
set.seed(333)
samp_data <- data_val %>%
  group_by(female_pq) %>%
  slice_sample(n=kNumSamps) %>%
  ungroup() 

# Foil data - random males. Same for each model
set.seed(333)
foil_data <- data_val %>%
  dplyr::filter(female_pq == 0) %>%
  sample_n(kNumSampsFoil)


#
# IML predictor function  ----
# Removes created objects from h2o 
# to avoid running out of memory
#

pred_fun <- function(model, newdata) {
  
  # Get initial h2o objects
  init_ls_char <- h2o.ls() %>% mutate_all(as.character) %>% pull(key)
  
  results <- as.data.frame(h2o.predict(model, as.h2o(newdata)))
  
  # Clean up new h2o objects
  fin_ls_char <- h2o.ls() %>% mutate_all(as.character) %>% pull(key)
  diff_ls_char <- fin_ls_char[!fin_ls_char %in% init_ls_char]
  h2o.rm(diff_ls_char)
  
  return(results[[3L]])
}

#
# Shapley loop ----
# This section of code is slow.
# May take hours or overnight to run.
#

shap_df = data.frame()

for (i in seq(1, length(model_list))) { 
  
  this_model = model_list[i]
  this_model_h2o = model_list_h2o[[this_model]] 
  
  this_predictors = predictors_df %>%
    dplyr::filter(model == this_model) %>%
    pull(feature) %>%
    as.character()
  
  this_foil <- foil_data %>%
    dplyr::select(all_of(this_predictors), 'bad_loan') %>%
    as.data.frame()

    # Create predictor object, using test data as the base
    this_predictor <- Predictor$new(
      model = this_model_h2o, 
      y = 'bad_loan', 
      data =  this_foil,
      predict.fun = pred_fun,
      class = 2
    )
  
    # Shapley on each
    for (case_id in samp_data$ID) {
      
      this_x = samp_data %>% 
        dplyr::filter(ID == case_id) %>%
        dplyr::select(one_of(this_predictors)) %>%
        as.data.frame()
      
      this_shap  <- (Shapley$new(this_predictor, this_x))$results %>%
        as.data.frame() %>%
        mutate(ID = case_id,
               model = this_model) %>%
        left_join(samp_data, by='ID')
      
      shap_df <- shap_df %>%
        bind_rows(this_shap)

    }
}

saveRDS(shap_df, file.path(kOutputDir, '06_DATA_shap_results.rds'))

#
# Aggregate Shapley values by sex ----
# Combine low-value features into an
# "other' group to simplify plots and tables.
#

shap_df <- readRDS(file.path(kOutputDir, '06_DATA_shap_results.rds')) %>%
  dplyr::filter(model %in% kModelInclude)

# Get overall most influential features by model
shap_model_inf <- shap_df %>%
  group_by(model, feature) %>%
  dplyr::summarize(phi_mean_overall = mean(abs(phi))) %>%
  ungroup() %>%
  arrange(model, desc(phi_mean_overall)) %>%
  group_by(model) %>%
  mutate(phi_rank = seq_len(n())) %>%
  ungroup() %>%
  dplyr::select(model, feature, phi_rank)

# Combine non-influential features into "other" group
# Shapley values are additive, so sum low-importances features per individual
shap_df_other_grp <- shap_df %>%
  left_join(shap_model_inf, by=c('model', 'feature')) %>%
  mutate(phi_rank = case_when(phi_rank >= kMaxFeat ~ as.integer(999), TRUE ~ phi_rank),
         feature = case_when(phi_rank == 999 ~ 'other',
                             TRUE ~ feature)) %>%
  group_by(model, ID, feature) %>%
  dplyr::summarize(female_pq = first(female_pq),
                   phi = sum(phi, na.rm=T),
                   phi_rank = first(phi_rank)) %>%
  ungroup() 

# Shapley value means by model and sex, with "other" group
shap_df_agg_sex <- shap_df_other_grp %>%
  group_by(model, female_pq, feature) %>%
  dplyr::summarize(phi_mean = mean(phi),
                   phi_sd = sd(phi),
                   phi_rank = first(phi_rank),
                   tot_n = n()) %>%
  ungroup() %>%
  arrange(model, phi_rank)


#
# Plot mean Shapley values by sex ----
#

for (this_model in model_list) {
  
  # Overall Shapley importances
  # Shap plot
  this_shap_agg <- shap_df_agg_sex %>%
    dplyr::filter(model == this_model) %>%
    arrange(phi_rank) %>%
    mutate(feature = as_factor(feature))
  
  # Put plots on the same scale
  scale_min_phi <- 0.01*floor(min(this_shap_agg$phi_mean)/0.01)
  scale_max_phi <- 0.01*ceiling(max(this_shap_agg$phi_mean)/0.01)
  
  for(this_female_pq in unique(shap_df_agg_sex$female_pq)) {
    
    this_shap_agg_sex <- this_shap_agg %>%
      dplyr::filter(female_pq == this_female_pq) 
      
    # Shap plot
    shap_plot_df <- this_shap_agg_sex %>%
      ggplot(aes(x=fct_rev(feature), y=phi_mean)) +
      geom_col(position=position_dodge(), fill='lightblue') +
      coord_flip() +
      theme_minimal(base_size = 14) +
      scale_y_continuous(limits=c(scale_min_phi, scale_max_phi))+
      labs(x=NULL, y = 'mean phi', 
           title = paste0('model:', this_model, '; female_pq:', this_female_pq))
      
    print(shap_plot_df)
    
    ggsave(file.path(kOutputDir, paste0('06_PLOT_shap_raw_female_pq', this_female_pq,
                                        '_', this_model, '.png')),
           shap_plot_df, type='cairo', width = 4, height=4)
     
  }
  
}


#
# Sex phi means ----
# Check that the mean Shapley value by model/sex is consistent
# with what is expected for population probabilities.
# The mean Shapley values for the population should equal
# (population mean probability- foil mean probability).
# Small rounding errors may occur but we expect overall agreement
#

# Get mean predictions in reference

foil_stats_overall <- foil_data %>%
  left_join(predictions_validation %>%
              dplyr::select(p1,model,  ID),
            by='ID') %>%
  group_by(model) %>%
  dplyr::summarize(mean_p1_foil = mean(p1),
                   tot_cases_foil = n())

# Get Shapley test population means
samp_stats_sex <- samp_data %>%
  left_join(predictions_validation %>%
              dplyr::select(ID, model, p1),
            by=c('ID')) %>%
  group_by(model, female_pq) %>%
  dplyr::summarize(mean_p1_samp = mean(p1),
                   tot_cases_samp = n()) %>%
  ungroup()


# Get test population info
phi_sums <- shap_df_agg_sex %>%
  group_by(model, female_pq) %>%
  dplyr::summarize(tot_phi_mean = sum(phi_mean)) %>%
  ungroup() %>%
  left_join(foil_stats_overall %>% dplyr::select(model, mean_p1_foil),
            by='model') %>%
  left_join(samp_stats_sex %>% dplyr::select(model, female_pq, mean_p1_samp,
                                             tot_cases_samp),
            by=c('model', 'female_pq')) %>%
  mutate(pop_mean_minus_foil = mean_p1_samp - mean_p1_foil,
         diff_check = pop_mean_minus_foil - tot_phi_mean)

print(phi_sums)

phi_sums %>%
  fwrite(file.path(kOutputDir, '06_DATA_phi_sums_grp.csv'))

#
# Histograms ----
#

shap_hist_data <- shap_df %>%
  left_join(shap_model_inf, by=c('feature', 'model')) %>%
  mutate(phi_rank = case_when(phi_rank >= kMaxFeat ~ as.integer(999), TRUE ~ phi_rank),
         feature = case_when(phi_rank == 999 ~ 'other',
                             TRUE ~ feature)) 

# Overall and within CM by model

for (model_nm in model_list) { 
  
  this_hist_data <- shap_hist_data %>%
    dplyr::filter(model == model_nm) %>%
    arrange(desc(phi_rank)) %>%
    mutate(feature = as_factor(feature)) 
  
  if(nrow(this_hist_data) == 0) {
    next
  }
  
  # Overall
  this_hist_overall  <- this_hist_data %>%
    ggplot(aes(x=phi, fill=female_pq, color=female_pq)) +
    geom_density(alpha=0.3) +
    facet_wrap(~fct_rev(feature), scales='free_y') +
    theme_minimal(base_size=14) +
    scale_fill_manual(values = c('turquoise', 'pink')) +
    scale_color_manual(values = c('turquoise', 'pink'))+
    labs(title=paste0('Model: ', model_nm),
         y=NULL) +
    theme(axis.text.y = element_blank())
  print(this_hist_overall)
  
  ggsave(file.path(kOutputDir, 
                   paste0('06_PLOT_shapley_hists_model_', model_nm, '.png')),
         this_hist_overall, type='cairo', width=9, height=5)
  
}

#
# T -tests ----
#

shap_df_other_grp %>%
  dplyr::filter(grepl('_pq$', feature)) %>%
  group_by(model, feature) %>%
  dplyr::summarize(mean(phi),
                 min(phi),
                 max(phi))

# overall F v M
t_info_overall_fm <- shap_df_other_grp %>%
  group_by(model, feature) %>%
  do(get_t_info(., gender_var = female_pq)) %>%
  ungroup() 

t_info_overall_fm %>%
  dplyr::filter(t_p_value <= 0.05)

t_info_overall_fm %>%
  dplyr::filter(feature == 'female_pq')

saveRDS(t_info_overall_fm,
        file.path(kOutputDir, '06_DATA_t_tests_overall_f_v_m.rds'))
fwrite(t_info_overall_fm,
        file.path(kOutputDir, '06_DATA_t_tests_overall_f_v_m.csv'))

# One-sided, F
t_info_overall_1 <- shap_df_other_grp %>%
  dplyr::filter(female_pq == '1') %>%
  group_by(model, feature) %>%
  do(get_t_info_one_sided(.)) %>%
  ungroup() 

# Include mean, std dev
t_info_overall <- shap_df_agg_sex %>%
  dplyr::filter(female_pq == '1') %>%
  dplyr::select(model, feature, phi_mean, phi_sd) %>%
  left_join(t_info_overall_1, by=c('model', 'feature')) %>%
  arrange(model, t_p_value)

saveRDS(t_info_overall,
        file.path(kOutputDir, '06_DATA_t_tests_overall_f.rds'))
fwrite(t_info_overall,
       file.path(kOutputDir, '06_DATA_t_tests_overall_f.csv'))

#
# Phis t-test, correlations ----
#

# T test.  Which phis are different for models p,q?
p_q_comp <- shap_df_other_grp %>%
  dplyr::filter(female_pq == '1')

p_q_comp_t_test <- p_q_comp %>%
  group_by(feature) %>%
  do(get_t_info(., gender_var = model,
                gender_levels = c('q', 'p'))) %>%
  ungroup() 

fwrite(p_q_comp_t_test, file.path(kOutputDir, '06_REPORT_t_test_pq.csv'))

# Correlations. 
data = shap_df_other_grp %>%
  dplyr::filter(model == 'q') %>%
  dplyr::select(ID, feature, phi) %>%
  pivot_wider(names_from = 'feature', values_from = 'phi')
  
wide_data = shap_df_other_grp %>%
  dplyr::select(ID, model, feature, phi) %>%
  pivot_wider(id_cols = c('ID', 'model'),
              names_from = 'feature', values_from = 'phi')%>%
  left_join(samp_data %>% 
              dplyr::select(female_pq, ID) %>%
              rename(female_pq_grp = female_pq), by='ID') 

cor_cols <- names(wide_data)[!names(wide_data) == 'female_pq_grp']
wide_data_cor <- wide_data %>%
  dplyr::filter(model == 'q')%>%
  group_by(model, female_pq_grp) %>%
  do(get_all_cor(., female_pq,
              keep_cols = cor_cols)) %>%
  ungroup() %>%
  arrange(model, desc(abs(value)))

fwrite(wide_data_cor, file.path(kOutputDir, '06_REPORT_phi_cors_female_pq.csv'))


#
# Basic interaction Plots ----
#

predictions_with_income <-   predictions_validation %>%
  left_join(data_val %>% dplyr::select(ID, annual_inc_pq), by='ID') %>%
  mutate(inc_grp = pmin(20000*floor(annual_inc_pq/20000) + 10000, 120000)) 

# Model probabilities by income group
gp_int_p1 <- predictions_with_income %>%
  group_by(model, inc_grp, female_pq) %>%
  dplyr::summarize(mean_p1 = mean(p1),
                   sd_p1 = sd(p1)) %>%
  ungroup() %>%
  ggplot(aes(x=inc_grp, y=mean_p1, color=female_pq)) +
  geom_point()+
  facet_wrap(~model) +
  theme_minimal(base_size = 14) +
  scale_color_manual(values=c('turquoise', 'coral3')) +
  scale_x_continuous(labels=scales::dollar_format(scale=1e-3)) +
  labs(title='Model Prediction by Income Level',
       y = 'Mean model p1',
       x = 'Income (grouped), in thousands')

print(gp_int_p1)
ggsave(file.path(kOutputDir, '06_PLOT_interaction_p1_by_income.png'),
       gp_int_p1, type='cairo', width=5, height=4)

# actual outcomes by income group (model-independent)
gp_int_out <- data_val %>%
  mutate(inc_grp = pmin(20000*floor(annual_inc_pq/20000) + 10000, 120000)) %>%
  group_by(inc_grp, female_pq) %>%
  dplyr::summarize(mean_outcome = mean(bad_loan == '1')) %>%
  ungroup() %>%
  ggplot(aes(x=inc_grp, y=mean_outcome, color=female_pq)) +
  geom_point()+
  theme_minimal(base_size = 14) +
  scale_color_manual(values=c('turquoise', 'coral3')) +
  scale_x_continuous(labels=scales::dollar_format(scale=1e-3)) +
  labs(title='Actual Outcomes by Income Level',
       y = 'Bad Loan Rate',
       x = 'Income (grouped), in thousands')
print(gp_int_out)
ggsave(file.path(kOutputDir, '06_PLOT_interaction_outcome_by_income.png'),
       gp_int_out, type='cairo', width=5, height=4)

# 
# Phi interaction plots ----
#

shap_data_inc_grp <- shap_df %>%
  left_join(predictions_with_income %>%
              dplyr::select(ID, model, p1, predict, inc_grp), 
            by=c('ID', 'model'))


# Plot annual income phi value by female status for both models

gp_inc_phi_by_inc <- shap_data_inc_grp %>%
  dplyr::filter(feature == 'annual_inc_pq') %>%
  group_by(model, inc_grp, female_pq) %>%
  dplyr::summarize(mean_phi = mean(phi),
                   sd_phi = sd(phi)) %>%
  ungroup() %>%
  ggplot(aes(x=inc_grp, y=mean_phi, color=female_pq)) +
  geom_point() +
  theme_minimal(base_size = 16) +
  scale_color_manual(values=c('turquoise', 'coral3')) +
  scale_x_continuous(labels=scales::dollar_format(scale=1e-3)) +
  facet_wrap(~model) +
  geom_hline(yintercept=0) +
  labs(title='Income effect by income level',
       y = 'Phi for income feature',
       x = 'Income (grouped), in thousands')

print(gp_inc_phi_by_inc)

ggsave(file.path(kOutputDir, '/06_PLOT_phi_income_by_income.png'),
       gp_inc_phi_by_inc, type='cairo', width=8, height=5)

# Plot female phi value by female status for both models

gp_female_phi_by_inc <- shap_data_inc_grp %>%
  dplyr::filter(feature == 'female_pq') %>%
  group_by(model, inc_grp, female_pq) %>%
  dplyr::summarize(mean_phi = mean(phi),
                   sd_phi = sd(phi)) %>%
  ungroup() %>%
  ggplot(aes(x=inc_grp, y=mean_phi, color=female_pq)) +
  geom_point() +
  theme_minimal(base_size = 16) +
  scale_color_manual(values=c('turquoise', 'coral3')) +
  scale_x_continuous(labels=scales::dollar_format(scale=1e-3)) +
  facet_wrap(~model) +
  geom_hline(yintercept=0) +
  labs(title='Female feature effect by income level',
       y = 'Phi for female feature',
       x = 'Income (grouped), in thousands')

print(gp_female_phi_by_inc)

ggsave(file.path(kOutputDir, '/06_PLOT_phi_female_by_income.png'),
       gp_female_phi_by_inc, type='cairo', width=8, height=5)

#
# Phi scatter ----
# phi for income vs. phi for female status
# phi for income vs. phi for income/female interaction
#

plot_pairwise_phi_scatter <- function(data, this_model, features,
                                      include_males = F,
                                      outfile_prefix = NULL) {
  
  if (length(features) != 2) {
    return(NULL)
  }
  name_vec <- sapply(features, function(x) paste0('phi_', x))
  names(name_vec) <- c('v1', 'v2')
  
  inc_scatter_df <- data %>%
    dplyr::select(ID, model, feature, phi, female_pq) %>%
    dplyr::filter(model == this_model &
                    feature %in% features) %>%
    pivot_wider(id_cols = c('ID', 'model', 'female_pq'),
                names_from = 'feature',
                values_from = 'phi',
                names_prefix = 'phi_') %>%
    rename_at(vars(all_of(name_vec)), ~names(name_vec) )
  
  if (include_males == F) {
    gp_inc_scatter <- inc_scatter_df %>%
      dplyr::filter(female_pq == 1) %>%
      ggplot(aes(x=v1, y=v2)) 
    
  } else {
    gp_inc_scatter <- inc_scatter_df %>%
      ggplot(aes(x=v1, y=v2, color=female_pq)) 
  }

  gp_inc_scatter <- gp_inc_scatter +
    geom_point() +
    theme_minimal(base_size = 14) +
    labs(x = paste0('phi for ', features[1]),
         y = paste0('phi for ', features[2]),
         title=paste0('Model: ', this_model))
  
  print(gp_inc_scatter)
  
  if (!is.null(outfile_prefix)) {
    ggsave(paste0(outfile_prefix, '_', this_model,
                  '_', features[1], '_', features[2], '.png'),
           gp_inc_scatter, type='cairo', width=5, height=4)
  }
  
  # Correlation
  this_cor <- (inc_scatter_df %>%
    dplyr::filter(female_pq == 1) %>%
    dplyr::select(v1, v2) %>%
    cor(method ='spearman'))[1,2]
  
  return(data.frame(model = this_model,
                    v1 = features[1],
                    v2 = features[2],
                    cor = this_cor))
  
}


# phi value plots, also save correlations

shap_data_inc_grp %>% glimpse()

feature_pairs <- list(c('annual_inc_pq', 'female_pq'),
                c('annual_inc_pq', 'inc_female_pq'),
                c('inc_female_pq', 'female_pq'))

phi_cor_df = data.frame()

for (this_model in model_list) { 
  for (this_features in feature_pairs) {
    
    this_model_features = predictors_df %>%
      dplyr::filter(model == this_model) %>%
      pull(feature) %>%
      as.character()
    
    if (length(setdiff(this_features, this_model_features)) > 0) {
      next
    }

    this_cor <- shap_data_inc_grp %>%
      plot_pairwise_phi_scatter(this_model, this_features,
                                outfile_prefix= file.path(kOutputDir,
                                                          '06_PLOT_scatter_phi'))
    
    phi_cor_df <- phi_cor_df %>%
      bind_rows(this_cor)
    
  }
}

fwrite(phi_cor_df,
       file.path(kOutputDir, '06_REPORT_phi_correlations.csv'))
  
#
# Income scatter ----
# Income dollars vs. phi value scatter plots
# Look at female_pq, annual_inc_pq, inc_female_pq
#

plot_income_scatter <- function(data, this_model, this_feature,
                                outfile_prefix = NULL) {
  inc_scatter_df <- data %>%
    dplyr::filter(female_pq == 1 &
                    feature == this_feature &
                    model == this_model) %>%
    dplyr::select(ID, annual_inc_pq, phi)
  
  gp_inc_scatter <- inc_scatter_df %>%
    ggplot(aes(x=annual_inc_pq, y=phi)) +
    geom_point() +
    theme_minimal(base_size = 14) +
    labs(y = paste0('phi for ', this_feature),
         title=paste0('Model: ', this_model)) +
    scale_x_log10(limits =c(1e3, 1e6)) 
  
  print(gp_inc_scatter)
  
  if (!is.null(outfile_prefix)) {
    ggsave(paste0(outfile_prefix, '_', this_model,
                  '_', this_feature, '.png'),
           gp_inc_scatter, type='cairo', width=5, height=4)
  }
  
  # Correlation
  this_cor <- (inc_scatter_df %>%
                 dplyr::select(annual_inc_pq, phi) %>%
                 cor(method ='spearman'))[1,2]
  
  return(data.frame(model = this_model,
                    feature = this_feature,
                    cor = this_cor))
  
}


# phi vs income plots, also save correlations

features <- c('annual_inc_pq', 'female_pq', 'inc_female_pq')

inc_cor_df = data.frame()

for (this_model in model_list) { 
  for (this_feature in features) {
    
    this_model_features = predictors_df %>%
      dplyr::filter(model == this_model) %>%
      pull(feature) %>%
      as.character()
    
    if (!this_feature %in% this_model_features) {
      next
    }
    
    this_cor <- shap_data_inc_grp %>%
      plot_income_scatter(this_model, this_feature,
                                outfile_prefix= file.path(kOutputDir,
                                                          '06_PLOT_scatter_inc_phi'))
    
    inc_cor_df <- inc_cor_df %>%
      bind_rows(this_cor)
    
  }
}

fwrite(inc_cor_df,
       file.path(kOutputDir, '06_REPORT_inc_phi_correlations.csv'))

#
# Feature interaction test ----
# Use sammple data, which is split
# between "males" and "females"
#


shap_int_df = data.frame()

for (i in seq(1, length(model_list))) { 
  
  this_model = model_list[i]
  this_model_h2o = model_list_h2o[[this_model]] 
  
  this_features = predictors_df %>%
    dplyr::filter(model == this_model) %>%
    pull(feature) %>%
    as.character()
  
    this_samp <- samp_data %>%
      dplyr::select(all_of(this_features), 'bad_loan') %>%
      as.data.frame()
    
    # Create predictor object, using test data as the base
    this_predictor <- Predictor$new(
      model = this_model_h2o, 
      y = 'bad_loan', 
      data =  this_samp,
      predict.fun = pred_fun,
      class = 2
    )
    
    # Interaction with female_pq
    if ('female_pq' %in% this_features) {
      this_int  <- (Interaction$new(this_predictor, feature='female_pq'))$results 
    } else {
      this_int = data.frame()
    }
    
    # Interaction with income
    if ('annual_inc_pq' %in% this_features) {
      this_int_inc  <- (Interaction$new(this_predictor, feature='annual_inc_pq'))$results 
    } else {
      this_int_inc = data.frame()
    }
    
    # Interaction with interaction term
    if ('inc_female_pq' %in% this_features) {
      this_int_int  <- (Interaction$new(this_predictor, feature='inc_female_pq'))$results 
    } else {
      this_int_int = data.frame()
    }
    
    this_int_df <- this_int %>%
      as.data.frame() %>%
      mutate(model = this_model,
             feature = 'female_pq') %>%
      bind_rows(this_int_inc %>%
                  as.data.frame() %>%
                  mutate(model = this_model,
                         feature = 'annual_inc_pq'))  %>%
      bind_rows(this_int_int %>%
                  as.data.frame() %>%
                  mutate(model = this_model,
                         feature = 'inc_female_pq'))
    
    shap_int_df <- shap_int_df %>%
      bind_rows(this_int_df)
  
}

saveRDS(shap_int_df, file.path(kOutputDir, '06_DATA_shap_inter.rds'))
fwrite(shap_int_df,  file.path(kOutputDir, '06_DATA_shap_inter.csv'))

shap_int_df %>%
  arrange(model,  desc(.interaction)) %>%
  dplyr::filter(feature == 'female_pq')

shap_int_df %>%
  arrange(model, desc(.interaction)) %>%
  dplyr::filter(feature == 'annual_inc_pq')

shap_int_df %>%
  arrange(model, desc(.interaction)) %>%
  dplyr::filter(feature == 'inc_female_pq')

#
# ALE/PDP ----
# One and two way ALE plots which separate out
# main effects from interactions.  Also show the PDPs.
#

female_ale_pdp = data.frame()

for (i in seq(1, length(model_list))) { 
  
  this_model = model_list[i]
  this_model_h2o = model_list_h2o[[this_model]] 
  
  this_features = predictors_df %>%
    dplyr::filter(model == this_model) %>%
    pull(feature) %>%
    as.character()
  
  this_samp <- samp_data %>%
    dplyr::select(all_of(this_features), 'bad_loan') %>%
    as.data.frame()
  
  this_plot_prefix = file.path(kOutputDir, paste0('06_PLOT_dep_',
                                                  this_model))
  
  # Create predictor object, using test data as the base
  this_predictor <- Predictor$new(
    model = this_model_h2o, 
    y = 'bad_loan', 
    data =  this_samp,
    predict.fun = pred_fun,
    class = 2
  )
  
  # Income feature.  ALE and PDP
  if ('annual_inc_pq' %in% this_features) {
    ale_inc <- FeatureEffect$new(this_predictor,  c("annual_inc_pq"), method='ale') 
    gp_ale_inc <-ale_inc$results %>%
      ggplot(aes(x=annual_inc_pq, y=.value))+
      geom_line() +
      scale_x_continuous(labels=scales::dollar) +
        coord_cartesian(xlim =c(0,150000), expand=T) +
      theme_minimal(base_size = 14) +
      labs(x='Annual Income', y='ALE Value') +
      geom_hline(yintercept=0, color='gray')
    print(gp_ale_inc)
    ggsave(paste0(this_plot_prefix, '_ale_annual_inc_pq.png'),
           gp_ale_inc, type='cairo', width=5, height=4)
    
    pdp_inc <- FeatureEffect$new(this_predictor,  c("annual_inc_pq"), method='pdp') 
    gp_pdp_inc <-pdp_inc$results %>%
      ggplot(aes(x=annual_inc_pq, y=.value))+
      geom_line() +
      scale_x_continuous(labels=scales::dollar) +
      coord_cartesian(xlim =c(0,150000), expand=T) +
      theme_minimal(base_size = 14) +
      labs(x='Annual Income', y='PDP Value')
    print(gp_pdp_inc)
    ggsave(paste0(this_plot_prefix, '_pdp_annual_inc_pq.png'),
           gp_pdp_inc, type='cairo', width=5, height=4)
  }
  
  # Female feature.  ALE and PDP
  if ('female_pq' %in% this_features) {
    ale_female <- FeatureEffect$new(this_predictor,  c("female_pq"), method='ale')
    gp_ale_female <- ale_female$results %>%
      ggplot(aes(x=female_pq, y=.value)) +
      geom_col() +
      geom_hline(yintercept=0) +
      theme_minimal(base_size = 14) +
      labs(x='Female Status', y='ALE Value') 
    print(gp_ale_female)
    ggsave(paste0(this_plot_prefix, '_ale_female_pq.png'),
           gp_ale_female, type='cairo', width=5, height=4)
    
    pdp_female <- FeatureEffect$new(this_predictor,  c("female_pq"), method='pdp')
    gp_pdp_female <- pdp_female$results %>%
      ggplot(aes(x=female_pq, y=.value)) +
      geom_col() +
      geom_hline(yintercept=0) +
      theme_minimal(base_size = 14) +
      labs(x='Female Status', y='PDP Value')
    print(gp_pdp_female)
    ggsave(paste0(this_plot_prefix, '_pdp_female_pq.png'),
           gp_pdp_female, type='cairo', width=5, height=4)
    
    # Save the values
    female_ale_pdp <- female_ale_pdp %>%
      bind_rows(ale_female$results %>%
                  as.data.frame() %>%
                  mutate(model=this_model)) %>%
      bind_rows(pdp_female$results %>%
                  as.data.frame() %>%
                  mutate(model=this_model))
  }
  
  # Income/female interaction feature.  ALE and PDP
  if ('inc_female_pq' %in% this_features) {
    ale_female_int <- FeatureEffect$new(this_predictor,  c("inc_female_pq"), method='ale')
    gp_ale_female_int <- ale_female_int$results %>%
      ggplot(aes(x=inc_female_pq, y=.value))+
      geom_line() +
      scale_x_continuous(labels=scales::dollar) +
      coord_cartesian(xlim =c(0,150000), expand=T) +
      theme_minimal(base_size = 14) +
      labs(x='Female/Income Interaction', y='ALE Value') +
      geom_hline(yintercept=0, color='gray')
    print(gp_ale_female_int)
    ggsave(paste0(this_plot_prefix, '_ale_inc_female_pq.png'),
           gp_ale_female_int, type='cairo', width=5, height=4)
    
    pdp_female_int <- FeatureEffect$new(this_predictor,  c("inc_female_pq"), method='pdp')
    gp_pdp_female_int <- pdp_female_int$results %>%
      ggplot(aes(x=inc_female_pq, y=.value)) +
      geom_line() +
      scale_x_continuous(labels=scales::dollar) +
      coord_cartesian(xlim =c(0,150000), expand=T) +
      theme_minimal(base_size = 14) +
      labs(x='Female/Income Interaction', y='PDP Value') 
    print(gp_pdp_female_int)
    ggsave(paste0(this_plot_prefix, '_pdp_inc_female_pq.png'),
           gp_pdp_female_int, type='cairo', width=5, height=4)
  }
  
  # Interaction - female and income
  if ('female_pq' %in% this_features & 
      'annual_inc_pq' %in% this_features) {
    ale_female_inc <- FeatureEffect$new(this_predictor,  
                                        c('annual_inc_pq', "female_pq"), method='ale')
    gp_ale_female_inc <- ale_female_inc$results %>%
      ggplot(aes(x=annual_inc_pq, y=.ale, color=female_pq)) +
      geom_line() +
      scale_x_continuous(labels=scales::dollar) +
      coord_cartesian(xlim =c(0,150000), expand=T) +
      theme_minimal(base_size = 14) +
      labs(x='Annual Income', y='ALE Value', color='Female')+
      geom_hline(yintercept=0, color='gray') +
      scale_color_manual(values = c('turquoise', 'coral')) +
      theme(legend.position=c(0.8, 0.6),
            legend.background = element_rect(color='gray', fill='white'))
    print(gp_ale_female_inc)
    ggsave(paste0(this_plot_prefix, '_ale_2way_inc_female.png'),
           gp_ale_female_inc, type='cairo', width=5, height=4)
    
    pdp_female_inc <- FeatureEffect$new(this_predictor,  
                                        c('annual_inc_pq', "female_pq"), method='pdp')
    gp_pdp_female_inc <- pdp_female_inc$results %>%
      ggplot(aes(x=annual_inc_pq, y=.value, color=female_pq)) +
      geom_line() +
      scale_x_continuous(labels=scales::dollar) +
      coord_cartesian(xlim =c(0,150000), expand=T) +
      theme_minimal(base_size = 14) +
      labs(x='Annual Income', y='PDP Value', color='Female')+
      scale_color_manual(values = c('turquoise', 'coral')) +
      theme(legend.position=c(0.8, 0.6),
            legend.background = element_rect(color='gray', fill='white'))
    print(gp_pdp_female_inc)
    ggsave(paste0(this_plot_prefix, '_pdp_2way_inc_female.png'),
           gp_pdp_female_inc, type='cairo', width=5, height=4)
  }
  
  # Interaction - female/inc and income
  if ('inc_female_pq' %in% this_features & 
      'annual_inc_pq' %in% this_features) {
    ale_female_int_inc <- FeatureEffect$new(this_predictor,  
                                        c('annual_inc_pq', "inc_female_pq"), method='ale')
    gp_ale_female_int_inc <- ale_female_int_inc$results %>%
      dplyr::filter(.bottom <= 150000 &
                      .right<= 150000) %>%
      ggplot(aes(xmin = .left, xmax = .right, ymin = .bottom,
                 ymax = .top, fill=.ale)) +
      geom_rect() +
      theme_minimal(base_size = 14) +
      labs(x='Annual Income', y='Income/Female Interaction', fill='ALE')  +
      theme_minimal(base_size = 14) +
      scale_x_continuous(labels=scales::dollar) +
      scale_y_continuous(labels=scales::dollar) +
      scale_fill_gradient2()
    print(gp_ale_female_int_inc)
    ggsave(paste0(this_plot_prefix, '_ale_2way_inc_femaleint.png'),
           gp_ale_female_int_inc, type='cairo', width=5, height=4)
    
    pdp_female_int_inc <- FeatureEffect$new(this_predictor,  
                                        c('annual_inc_pq', "inc_female_pq"), method='pdp')
    gp_pdp_female_int_inc <- pdp_female_int_inc$results %>%
      dplyr::filter(annual_inc_pq<= 150000 &
                      inc_female_pq <= 150000) %>%
      ggplot(aes(x = annual_inc_pq, y= inc_female_pq,fill=.value)) +
      geom_tile() +
      theme_minimal(base_size = 14) +
      labs(x='Annual Income', y='Income/Female Interaction', fill='PDP')  +
      theme_minimal(base_size = 14) +
      scale_x_continuous(labels=scales::dollar) +
      scale_y_continuous(labels=scales::dollar)
    print(gp_pdp_female_int_inc)
    ggsave(paste0(this_plot_prefix, '_pdp_2way_inc_femaleint.png'),
           gp_pdp_female_int_inc, type='cairo', width=5, height=4)
  }
  
}

fwrite(female_ale_pdp,
       file.path(kOutputDir, '/06_ALE_PDP_female_results.csv'))


#
# Models P and Q Comparison ----
# Show most influential features in P,
# plus the female feature.
#

# Get top 5 features for model P, plus income row from model Q
# Rank for major differences
model_p_ranks <- shap_model_inf %>%
  dplyr::filter(model == 'p' & phi_rank <= 5) %>%
  dplyr::select(-model) %>%
  bind_rows(shap_model_inf %>%
              dplyr::filter(model == 'q' & feature == 'female_pq') %>%
              dplyr::select(-model)) %>%
  arrange(phi_rank)

# Group unselected features into "other" for p and q, females only
shap_df_other_grp_pq <- shap_df %>%
  dplyr::filter(model %in% c('p', 'q')) %>%
  left_join(model_p_ranks, by=c('feature')) %>%
  mutate(phi_rank = case_when(is.na(phi_rank)  ~ as.integer(999), TRUE ~ phi_rank),
         feature = case_when(phi_rank == 999 ~ 'other',
                             TRUE ~ feature)) %>%
  group_by(model, female_pq, ID, feature) %>%
  dplyr::summarize(female_pq = first(female_pq),
                   phi = sum(phi, na.rm=T),
                   phi_rank = first(phi_rank)) %>%
  ungroup() 

# Aggregate model p,q values
shap_df_agg_pq <- shap_df_other_grp_pq %>%
  group_by(model, female_pq, feature) %>%
  dplyr::summarize(phi_mean = mean(phi),
                   phi_sd = sd(phi),
                   phi_rank = first(phi_rank),
                   tot_n = n()) %>%
  ungroup() %>%
  arrange(model, female_pq, phi_rank)

shap_df_agg_pq_features = unique((shap_df_agg_pq %>%
                                   dplyr::filter(model == 'q' &
                                                   female_pq == 1))$feature) 
shap_df_agg_pq_labels= gsub('*_pq$', '',
                                shap_df_agg_pq_features)

ylim = 0.01 *c(floor(min(shap_df_agg_pq$phi_mean)/0.01),
         ceiling(max(shap_df_agg_pq$phi_mean)/0.01))

  
# Plot females
gp_shap_pq <- shap_df_agg_pq %>%
  dplyr::filter(female_pq == 1) %>%
  mutate(feature = factor(feature, levels=shap_df_agg_pq_features,
                          labels=shap_df_agg_pq_labels),
         model = factor(model, levels=c('p', 'q'),
                        labels=c('Model', 'Model + female'))) %>%
  ggplot(aes(x=fct_rev(feature), y=phi_mean, fill=model)) +
  geom_col(position=position_dodge2(preserve = "single", reverse=T)) +
  coord_flip() +
  theme_minimal(base_size=14) +
  labs(x=NULL, y='Mean Shapley value', fill=NULL) +
  theme(legend.position=c(0.8, 0.15),
        legend.background = element_rect(fill='white', color='gray')) +
  scale_fill_manual(values = c('darkgray', 'darkorange')) +
  scale_y_continuous(limits=ylim, expand = expansion(0.03))

print(gp_shap_pq)
ggsave(file.path(kOutputDir, '/06_PLOT_shap_pq_compare.png'),
       gp_shap_pq, type='cairo', width=5.5, height=4.5)

