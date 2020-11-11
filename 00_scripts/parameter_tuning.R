#
# Perform parameter tuning using the
# original loans data set.  I will use these
# parameters for all models.  This is
# the only place the test data is used.
#
# Run after the first script, '01_data_process.R'
#
# By default fits an XGBoost model but can fit
# random forest as well
#

rm(list=ls())

source('00_setup.R')

library(h2o)
library(zeallot) # multiple function return


# Model type to use ('xgboost' or 'randomForest')
kModelType <- 'xgboost'


#
# Import data ----
#

data_filt <- readRDS(file.path(kOutputDir, '/01_DATA_base_gender_inf.rds'))

split_id_train <- readRDS(file.path(kOutputDir, '/01_DATA_split_id_train.rds'))
split_id_test <- readRDS(file.path(kOutputDir, '/01_DATA_split_id_test.rds'))
split_id_val <- readRDS(file.path(kOutputDir, '/01_DATA_split_id_val.rds'))

# predictors
y <- "bad_loan"
x_base <- setdiff(names(data_filt), c("bad_loan", "int_rate", "ID", "addr_state",
                            "female_prob", "unif_v", "Region"))

       
#
# h2o init and data transform ----
#

h2o.init()

h2o.removeAll()

data_h2o_train <- data_filt %>% semi_join(split_id_train, by='ID') %>%
  as.h2o()

dim(data_h2o_train)

data_h2o_test <- data_filt %>% semi_join(split_id_test, by='ID') %>%
  as.h2o()


#
# Fit the model  ----
#

# Get the standard parameters
x_base <- setdiff(names(data_filt), c("bad_loan", "int_rate", "ID", "addr_state",
                                      "female_prob", "unif_v", "Region"))
x_a <- setdiff(x_base, c('female','female2'))


# Grid search parameters
if (kModelType == 'xgboost') {
  hyper_params <- list(ntrees = seq(10, 1000, 1),
                       learn_rate = seq(0.0001, 0.2, 0.0001),
                       max_depth = seq(1, 20, 1),
                       sample_rate = seq(0.5, 1.0, 0.0001),
                       col_sample_rate = seq(0.2, 1.0, 0.001),
                       min_split_improvement = seq(0, 0.12, 0.03))
} else if (kModelType == 'randomForest') {
  hyper_params <- list(mtries = c(3, 4, 5, 7),
                       nbins = seq(10, 60, 10),
                       ntrees = seq(50, 150, 25),
                       max_depth = c(0, 10, 20),
                       histogram_type = c('QuantilesGlobal'))
}


search_criteria <- list(strategy = "RandomDiscrete",
                        stopping_metric='AUCPR',
                        max_models = 30, 
                        seed = 222)

rf_grid <- h2o.grid(algorithm = kModelType,
                      x = x_a,
                      y = 'bad_loan', 
                      training_frame = data_h2o_train,
                      validation_frame = data_h2o_test,
                      seed = 123,
                      hyper_params = hyper_params,
                      search_criteria = search_criteria)

# Sort the grid by PR-AUC
grid_sort <- h2o.getGrid(grid_id = rf_grid@grid_id, sort_by = "AUCPR", decreasing = TRUE)
print(grid_sort)

# Get the parameters for the top model

top_model_id <- grid_sort@summary_table[1, "model_ids"]
top_model <- h2o.getModel(top_model_id)

print(top_model)
print(top_model@parameters)

# Save the results

saveRDS(grid_sort@summary_table %>% as.data.frame(),
        file.path(kOutputDir, '/TMP_parameter_tuning_grid.rds'))
saveRDS(top_model@parameters,
        file.path(kOutputDir, '/TMP_parameter_tuning_params.rds'))
saveRDS(top_model@model$validation_metrics@metrics$cm$table,
        file.path(kOutputDir, '/TMP_parameter_tuning_confusion_matrix.rds'))
data.frame(auc = top_model@model$validation_metrics@metrics$AUC,
           prauc = top_model@model$validation_metrics@metrics$pr_auc,
           mean_per_class_error = top_model@model$validation_metrics@metrics$mean_per_class_error) %>%
  saveRDS(file.path(kOutputDir, '/TMP_parameter_tuning_metrics.rds'))

