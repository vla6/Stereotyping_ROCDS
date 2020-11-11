#
# Create models to enable assessment of
# stereotyping and feature bias effects.
#
# Two types of models are created:
#  A-E: Models to assess stereotyping effects
#  P-S: Models to assess feature bias effects
# 
# Models to assess stereotyping effects:
#  A.  original feature set 
#  B.  original feature set - income + female  
#  C.  original feature set - income
#  D.  original feature set + female
#  E.  original feature set - income + female + female2
#
# Models to assess feature bias effects:
#  P: original feature set with modified (biased) income
#  Q: model P + female_pq
#  R: model Q - income_pq
#  S: model P + female_pq + income/female_pq interaction
#
# By default we create XGBoost models.  However, this
# code can be modified to create random forest models.
#
#

rm(list=ls())
source('00_setup.R')

library(h2o)
library(zeallot) # multiple func return

# Model type to use ('xgboost' or 'randomForest')
kModelType <- 'xgboost'

#
# Import data ----
#

data_filt <- readRDS(file.path(kOutputDir, '/01_DATA_base_gender_inf.rds'))

split_id_train <- readRDS(file.path(kOutputDir, '/01_DATA_split_id_train.rds'))
split_id_val <- readRDS(file.path(kOutputDir, '/01_DATA_split_id_val.rds'))

# original predictors and target feature
y <- "bad_loan"
x_base <- readRDS(file.path(kOutputDir, '01_DATA_original_feature_set.rds'))

# train data
data_train <- data_filt %>%
  semi_join(split_id_train, by='ID') 

# validation data 
data_val <- data_filt %>%
  semi_join(split_id_val, by='ID') 
                     
#
# h2o init and transform ----
#

h2o.init()

h2o.removeAll()

data_h2o_train <- data_filt %>% semi_join(split_id_train, by='ID') %>%
  as.h2o()

dim(data_h2o_train)

data_h2o_val <- data_filt %>% semi_join(split_id_val, by='ID') %>%
  as.h2o()

#
# Model train function ----
# Parameters pre-selected to speed things up; see
# '00_scripts/parameter_tuning.R' to repeat/modify
# parameter tuning
# 

fit_save_model <- function(model_str_id,
                          x_vars, 
                          train_data, test_data,
                          outdir = kOutputDir,
                          model_type = 'xgboost') {
  
  if (model_type == 'xgboost') {
    this_model <- h2o.xgboost(x = x_vars,
                              y = 'bad_loan', 
                              training_frame = train_data,
                              validation_frame = test_data,
                              seed=123,
                              ntrees = 425,
                              max_depth =  3,
                              learn_rate = 0.1099,
                              sample_rate = 0.8478,
                              col_sample_rate = 0.69,
                              min_split_improvement= 0.06) }
  else if (model_type == 'randomForest') {
    this_model <- h2o.randomForest(x = x_vars,
                                   y = 'bad_loan', 
                                   training_frame = train_data,
                                   validation_frame = test_data,
                                   seed=123,
                                   histogram_type = 'QuantilesGlobal',
                                   categorical_encoding = 'Enum',
                                   mtries = 3,
                                   nbins = 50,
                                   max_depth = 10,
                                   ntrees = 150)
  } else {
    return(NULL)
  }
    
    
  # Save the model 
  if (!is.null(outdir) & dir.exists(outdir)) {
    data.frame(model_id = this_model@model_id) %>% 
      fwrite(paste0(outdir, '/02_MDL_', model_str_id, '.csv'))
    h2o.saveModel(this_model, outdir, force=T)
  }

  return(list(model_id = this_model@model_id,
              model = this_model))  
}



#
# Models Fits ----
#

# Base models
x_a <- x_base
x_b <- c(x_base[x_base != 'annual_inc'], 'female')
x_c <- x_base[x_base != 'annual_inc']
x_d <- c(x_base, 'female')
x_e <- c(x_base[x_base != 'annual_inc'], 'female', 'female2')

# Feature bias experiment models
x_p <- c(x_base[x_base != 'annual_inc'], 'annual_inc_pq')
x_q <- c(x_base[x_base != 'annual_inc'], 'annual_inc_pq',
         'female_pq')
x_r <- c(x_base[x_base != 'annual_inc'], 'female_pq')
x_s <- c(x_base[x_base != 'annual_inc'], 'annual_inc_pq',
         'female_pq', 'inc_female_pq')

# Save features
predictors_df <- data.frame(feature = x_a) %>%
  mutate(model = 'a') %>%
  bind_rows(data.frame(feature = x_b) %>%
              mutate(model = 'b')) %>%
  bind_rows(data.frame(feature = x_c) %>%
              mutate(model = 'c')) %>%
  bind_rows(data.frame(feature = x_d) %>%
              mutate(model = 'd'))%>%
  bind_rows(data.frame(feature = x_e) %>%
              mutate(model = 'e')) %>%
  bind_rows(data.frame(feature = x_p) %>%
              mutate(model = 'p')) %>%
  bind_rows(data.frame(feature = x_q) %>%
              mutate(model = 'q'))  %>%
  bind_rows(data.frame(feature = x_r) %>%
              mutate(model = 'r')) %>%
bind_rows(data.frame(feature = x_s) %>%
            mutate(model = 's')) 

saveRDS(predictors_df, file.path(kOutputDir, '02_DATA_predictors.rds'))

# Train all the models

c(grid_a_top_model_id, grid_a_top_model)  %<-%  
  fit_save_model('a', x_a, 
                 data_h2o_train, data_h2o_val,
                 model_type = kModelType)
c(grid_b_top_model_id, grid_b_top_model)  %<-%  
  fit_save_model('b', x_b, 
                 data_h2o_train, data_h2o_val,
                 model_type = kModelType)
c(grid_c_top_model_id, grid_c_top_model)  %<-%  
  fit_save_model('c',  x_c, 
                 data_h2o_train, data_h2o_val,
                 model_type = kModelType)
c(grid_d_top_model_id, grid_d_top_model)  %<-%  
  fit_save_model('d', x_d, 
                data_h2o_train, data_h2o_val,
                model_type = kModelType)
c(grid_e_top_model_id, grid_e_top_model)  %<-%  
  fit_save_model('e', x_e, 
                 data_h2o_train, data_h2o_val,
                model_type = kModelType)
c(grid_p_top_model_id, grid_p_top_model)  %<-%  
  fit_save_model('p', x_p, 
                 data_h2o_train, data_h2o_val,
               model_type = kModelType)
c(grid_q_top_model_id, grid_q_top_model)  %<-%  
  fit_save_model('q', x_q, 
                 data_h2o_train, data_h2o_val,
               model_type = kModelType)
c(grid_r_top_model_id, grid_r_top_model)  %<-%  
  fit_save_model('r', x_r, 
                 data_h2o_train, data_h2o_val,
                 model_type = kModelType)
c(grid_s_top_model_id, grid_s_top_model)  %<-%  
  fit_save_model('s', x_s, 
                 data_h2o_train, data_h2o_val,
               model_type = kModelType)

# Save a file with all the model IDs
model_id_df <- data.frame(model=c('a', 'b', 'c', 'd', 'e', 'p', 'q','r', 's'),
                          id = c(grid_a_top_model_id,
                                 grid_b_top_model_id,
                                 grid_c_top_model_id,
                                 grid_d_top_model_id,
                                 grid_e_top_model_id,
                                 grid_p_top_model_id,
                                 grid_q_top_model_id,
                                 grid_r_top_model_id,
                                 grid_s_top_model_id))

model_id_df %>%
  fwrite(file.path(kOutputDir, '/02_REPORT_model_ids.csv'))

#
# Metrics ----
#

# Reload models
model_id_df <- fread(file.path(kOutputDir, '/02_REPORT_model_ids.csv'))
model_list = unique(model_id_df$model)
model_list_h2o <- sapply(model_list, 
                         function(x) model_load(x, model_id_df), USE.NAMES = T)

# Get max f1 thresholds
get_train_thresh <- function(model) {
  return( h2o.performance(model)@metrics$max_criteria_and_metric_scores %>%
    dplyr::filter(metric == 'max f1') %>%
    pull(threshold))
}

train_thresh_list <- sapply(model_list_h2o, get_train_thresh)

data.frame(model=names(train_thresh_list),
           thresh = train_thresh_list) %>%
  saveRDS(file.path(kOutputDir, '/02_DATA_thresholds.rds'))

# Get accuracy thresholds
get_train_thresh_acc <- function(model) {
  return( h2o.performance(model)@metrics$max_criteria_and_metric_scores %>%
            as.data.frame() %>%
            dplyr::filter(metric == 'max accuracy') %>%
            pull(threshold))
}

train_thresh_list_acc <- sapply(model_list_h2o, get_train_thresh_acc)

data.frame(model=names(train_thresh_list_acc),
           thresh = train_thresh_list_acc) %>%
  saveRDS(file.path(kOutputDir, '/02_DATA_thresholds_acc.rds'))

# Get metrics at the thresholds
get_met_thresh <- function(model, newdata, thresh) {
  perf = h2o.performance(model, newdata = newdata)
  met = h2o.metric(perf) %>%
    dplyr::filter(threshold >= thresh) %>%
    arrange(threshold) %>%
    top_n(1) %>%
    as.data.frame()
  
  met <- met %>%
    bind_cols(data.frame(auc = h2o.auc(perf, valid=T), 
              prauc = h2o.aucpr(perf, valid=T)))
  return(met)
}

metrics_comb = data.frame()
for (this_model in model_list) {
  this_metrics <- model_list_h2o[[this_model]] %>%
    get_met_thresh(data_h2o_val, train_thresh_list[[this_model]]) %>%
    mutate(model = this_model)
  
  metrics_comb <- metrics_comb %>%
    bind_rows(this_metrics)
}

fwrite(metrics_comb,
       file.path(kOutputDir, '02_REPORT_model_metrics.csv'))

#
# Feature importances ----
#

plot_imp <- function(model, n_vars = 12,
                     plot_outfile = NULL) {
  
  varimp <- h2o.varimp(model) %>%
    arrange(desc(relative_importance))
  
  gp_varmp <- varimp %>%
    top_n(n_vars, wt = relative_importance) %>%
    mutate(variable = as_factor(variable)) %>%
    ggplot(aes(x=fct_rev(variable), y=relative_importance)) +
    geom_col() +
    coord_flip() +
    theme_minimal(base_size=16) +
    theme(axis.text.x = element_blank(),
          axis.text.y=element_text(size=14)) +
    labs(y= 'relative importance', x=NULL)
  
  print(gp_varmp)
  
  if (!is.null(plot_outfile)) {
    ggsave(plot_outfile,
           gp_varmp, type='cairo', height=5, width=5)
  }
  
  return(varimp)
}

varimp_df <- data.frame()
for (this_model in model_list) {
  
  # Get top 10 plot
  this_varimp <- model_list_h2o[[this_model]] %>%
    plot_imp(plot_outfile = file.path(kOutputDir, 
                                      paste0('02_PLOT_glob_imp_top_10_', this_model, '.png'))) %>%
    as.data.frame() %>%
    mutate(model = this_model)
  
  # Do top 20 plot
  model_list_h2o[[this_model]] %>%
    plot_imp(20, plot_outfile = file.path(kOutputDir, 
                                      paste0('02_PLOT_glob_imp_top_20_', this_model, '.png')))
  
  varimp_df <- varimp_df %>%
    bind_rows(this_varimp)
}

varimp_df %>%
  fwrite(file.path(kOutputDir, '02_REPORT_feature_importances.csv'))
varimp_df %>%
  saveRDS(file.path(kOutputDir, '02_REPORT_feature_importances.rds'))

#
# Predict ----
# Train and validation data for all models
#

# Function to get predictions for arbitrary data
get_predictions <- function(data, model) {
  data_pred <-h2o.predict(model, newdata = data %>% as.h2o()) %>%
    as.data.frame() %>%
    bind_cols(data %>% dplyr::select(ID, bad_loan, 
                                     any_of(c('female', 'female_pq'))))
  return(data_pred)
}


# Get predictions for train and validation for all models
predictions_validation = data.frame()
predictions_train = data.frame()

for (this_model in model_list) {
  this_model_h2o = model_list_h2o[[this_model]]
  
  this_predict = data_val %>% 
    get_predictions(., this_model_h2o) %>%
    mutate(model = this_model)
  
  this_predict_train = data_train %>% 
    get_predictions(., this_model_h2o) %>%
    mutate(model = this_model)
  
  predictions_validation <- predictions_validation %>%
    bind_rows(this_predict) 
  
  predictions_train <- predictions_train %>%
    bind_rows(this_predict_train)
}

saveRDS(predictions_validation,
        file.path(kOutputDir, '02_DATA_predictions.rds'))

saveRDS(predictions_train,
        file.path(kOutputDir, '02_DATA_predictions_train.rds'))
