#
# Fairness metrics for models A-E (stereotype
# assessment).  This script calculates  common 
# "fairness metrics", as well as plotting 
# ROC curves by "gender", and other comparisons
#
# For models A-E only.  P-S are treated 
# separately due to different female defnitions
#
# Models analyzed:
#  A.  original feature set 
#  B.  original feature set - income + female  
#  C.  original feature set - income
#  D.  original feature set + female
#  E.  original feature set - income + female + female2
#
# Comparing Model A and Model B compares use of a possibly
# reasonable feature for this decision vs. a stereotyped
# assumption for this feature.
#
# Models C-E help us better understand A-B.  Model C is used to
# determing whether a sensistive feature is helpful in increasing
# overall accuracy.  Models D and E are used to assess Shapley
# result changes when adding the sensitive feature to the model. 
#

rm(list=ls())
source('00_setup.R')

library(PRROC)

# Exempt model with different female definitions
kModelOmit <- c('p', 'q', 'r', 's')

#
# Import data  ----
#

# Reload models
model_id_df <- fread(file.path(kOutputDir, '/02_REPORT_model_ids.csv'))
model_list = model_id_df$model [!model_id_df$model %in%kModelOmit]

# f1 threshold
threshold_df <- readRDS(file.path(kOutputDir, '/02_DATA_thresholds.rds'))  %>%
  dplyr::filter(!model %in% kModelOmit)

# Validation data predictions
predictions_validation <- readRDS(file.path(kOutputDir, '02_DATA_predictions.rds'))  %>%
  dplyr::filter(!model %in% kModelOmit)

#
# Actual, predicted ----
# rates by group, model
#

actual_overall <-predictions_validation %>%
  distinct(ID, .keep_all= T) %>%
  dplyr::summarize(mean_actual = mean(bad_loan == '1')) %>%
  mutate(gender_str = 'Z_ALL')

actual_by_sex <- predictions_validation %>%
  distinct(ID, .keep_all= T) %>%
  group_by(female) %>%
  dplyr::summarize(mean_actual = mean(bad_loan == '1')) %>%
  ungroup()  %>%
  mutate(gender_str = case_when(female == 0 ~ 'M',
                                TRUE ~ 'F')) %>%
  bind_rows(actual_overall) %>%
  dplyr::select(gender_str, mean_actual)

predicted_overall <- predictions_validation %>%
  group_by(model) %>%
  dplyr::summarize(mean_model = mean(p1)) %>%
  ungroup() %>%
  mutate(gender_str = 'Z_ALL')

predicted_by_sex <- predictions_validation %>%
  group_by(model, female) %>%
  dplyr::summarize(mean_model = mean(p1)) %>%
  ungroup() %>%
  mutate(gender_str = case_when(female == 0 ~ 'M',
                                TRUE ~ 'F')) %>% 
  bind_rows(predicted_overall) %>%
  arrange(model, gender_str) %>%
  dplyr::select(model, gender_str, mean_model)

predicted_by_sex %>%
  pivot_wider(names_from='model',
              values_from=c('mean_model')) %>%
  left_join(actual_by_sex, by='gender_str') %>%
  fwrite(file.path(kOutputDir, '/03_REPORT_ratios_by_sex.csv'))

#
# AUC by group ----
# ROC-AUC and PR-AUC
# See 00_scripts/metrics_by_group.R for the
# get_aucs function definition
#

# AUC by "gender"  ---
auc_df <- predictions_validation %>%
  group_by(model, female) %>%
  do(get_aucs(.)) %>%
  ungroup() %>%
  mutate(gender_str = case_when(female == 0 ~ 'M',
                                TRUE ~ 'F'))
# Overall auc
auc_all <- predictions_validation %>%
  group_by(model) %>%
  do(get_aucs(.)) %>%
  ungroup() %>%
  mutate(gender_str = 'Z_ALL') %>%
  bind_rows(auc_df) %>%
  arrange(model, gender_str)
  
auc_all %>%
  pivot_wider(names_from='model',
              values_from=c('auc', 'aucpr')) %>%
  fwrite(file.path(kOutputDir, '/03_REPORT_auc_by_sex.csv'))

#
# ROC curves ----
# See 00_scripts/metrics_by_group.R for
# function definitions
#

# curve data by "gender"
roc_curve_df <- predictions_validation %>%
  group_by(model, female) %>%
  do(get_roc_curves(.)) %>%
  ungroup() %>%
  mutate(gender_str = case_when(female == 0 ~ 'M',
                                TRUE ~ 'F'))

# Plot - separte by model
for (this_model in model_list) {
  roc_curve_df %>%
    dplyr::filter(model == this_model) %>%
    plot_roc_curves(outfile_prefix = paste0(kOutputDir, '/03_PLOT_roc_curve_', this_model),
                    title_str = paste0('Model: ', this_model)) 
}

#
# DIFR ----
# Diffusion Index For Risk
# Divide into high, med, low risk groups within models
# See 00_scripts/metrics_by_group.R for
# get_difr function definition
#

predictions_validation_grp <- predictions_validation %>%
  group_by(model) %>%
  mutate(risk_grp= cut(p1, 
                       breaks=c(quantile(p1,probs=c(0, 0.33, 0.67), 
                                         na.rm=T), 1), 
                       include.lowest = T, ordered_result=T,
                       labels=c('low', 'med', 'high'))) %>%
  ungroup()

difr_grp <- predictions_validation_grp %>%
  group_by(model, female) %>%
  do(get_difr(.)) %>%
  ungroup() %>%
  mutate(gender_str = case_when(female == 0 ~ 'M',
                                TRUE ~ 'F'))

difr_all <- predictions_validation_grp %>%
  group_by(model) %>%
  do(get_difr(.)) %>%
  ungroup() %>%
  mutate(gender_str = 'Z_ALL') %>%
  bind_rows(difr_grp) %>%
  arrange(model, gender_str)

difr_all %>%
  pivot_wider(names_from='model',
              values_from=c('difr')) %>%
  fwrite(file.path(kOutputDir, '/03_REPORT_difr_by_sex.csv'))

#
# Calibration ----
# Plot actual terminations by risk decile (overall)
# for both males and females.
# See 00_scripts/metrics_by_group.R for function
# definitions.
#

# Get risk deciles for the models
predictions_validation_decile <- predictions_validation %>%
  group_by(model) %>%
  mutate(risk_decile= cut(p1, 
                       breaks=quantile(.$p1,probs=seq(0, 1, 0.1), na.rm=T), 
                       include.lowest = T, ordered_result=T)) %>%
  ungroup()


# All models
predictions_validation_decile %>%
  plot_calibration_curves(outfile = file.path(kOutputDir, 
                               '/03_PLOT_calibration.png'))

# A,B only
predictions_validation_decile %>%
  plot_calibration_curves(models=c('a', 'b'),
           outfile = file.path(kOutputDir, 
                               '/03_PLOT_calibration_a_b.png'))

# C only
predictions_validation_decile %>%
  plot_calibration_curves(models=c('c'),
           outfile = file.path(kOutputDir, 
                               '/03_PLOT_calibration_c.png'))

#
# CP accuracy ----
#

# f1 accuracy
acc_grp_1 <- predictions_validation %>%
  mutate(accurate = case_when(bad_loan == '1' &
                                predict == '1' ~ 1,
                              bad_loan == '0' &
                                predict == '0' ~ 1,
                              TRUE ~ 0)) %>%
  mutate(gender_str = case_when(female == 0 ~ 'M',
                                TRUE ~ 'F'))

acc_grp_1 %>% group_by(model) %>% dplyr::summarize(mean(accurate))

acc_grp <- acc_grp_1 %>%
  mutate(gender_str = 'Z_ALL') %>%
  bind_rows(acc_grp_1) %>%
  group_by(model, gender_str) %>%
  dplyr::summarize(accuracy = mean(accurate)) %>%
  ungroup() 

acc_grp %>%
  pivot_wider(names_from = 'model',
              values_from = 'accuracy') %>%
  fwrite(file.path(kOutputDir, '/03_REPORT_accuracy.csv'))

#
# CP f1 metric ----
#

f1_grp_1 <- predictions_validation %>%
  mutate(tp = ifelse(bad_loan == '1' &
                          predict == '1', 1, 0),
         fp = ifelse(bad_loan == '0' &
                       predict == '1', 1, 0),
         fn = ifelse(bad_loan == '1' &
                       predict == '0', 1, 0),
         tn = ifelse(bad_loan == '0' &
                       predict == '0', 1, 0))  %>%
  mutate(gender_str = case_when(female == 0 ~ 'M',
                                TRUE ~ 'F'))

f1_grp <- f1_grp_1 %>%
  mutate(gender_str = 'Z_ALL') %>%
  bind_rows(f1_grp_1) %>%
  group_by(model, gender_str) %>%
  dplyr::summarize(tp = sum(tp),
                   fn = sum(fn),
                   fp = sum(fp)) %>%
  ungroup() %>%
  mutate(f1 = tp / (tp + 0.5*(fp+fn))) 

f1_grp %>%
  dplyr::select(model,  gender_str, f1) %>%
  pivot_wider(names_from = 'model',
              values_from = 'f1') %>%
  fwrite(file.path(kOutputDir, '/03_REPORT_f1.csv'))

#
# CP precision  ----
#

cp_prec <- f1_grp_1 %>%
  mutate(gender_str = 'Z_ALL') %>%
  bind_rows(f1_grp_1) %>%
  group_by(model, gender_str) %>%
  dplyr::summarize(tp = sum(tp),
                   fp = sum(fp)) %>%
  ungroup() %>%
  mutate(precision = tp / (tp +fp)) %>%
  dplyr::select(-tp, -fp) 

cp_prec %>%
  pivot_wider(names_from = 'model',
              values_from = 'precision') %>%
  fwrite(file.path(kOutputDir, '/03_REPORT_precision.csv'))

#
# CP recall   ----
#

cp_recall <- f1_grp_1 %>%
  mutate(gender_str = 'Z_ALL') %>%
  bind_rows(f1_grp_1) %>%
  group_by(model, gender_str) %>%
  dplyr::summarize(tp = sum(tp),
                   fn = sum(fn)) %>%
  ungroup() %>%
  mutate(recall = tp / (tp +fn)) %>%
  dplyr::select(-tp, -fn)

cp_recall %>%
  pivot_wider(names_from = 'model',
              values_from = 'recall') %>%
  fwrite(file.path(kOutputDir, '/03_REPORT_recall.csv'))


#
# CP false positive rate  ----
#

fp_df <- f1_grp_1 %>%
  mutate(gender_str = 'Z_ALL') %>%
  bind_rows(f1_grp_1) %>%
  dplyr::filter(bad_loan == '0')
fp_grp <- fp_df %>%
  group_by(model, gender_str) %>%
  dplyr::summarize(fp_rate = mean(fp)) %>%
  ungroup() 

fp_grp %>%
  pivot_wider(names_from = 'model',
              values_from = 'fp_rate') %>%
  fwrite(file.path(kOutputDir, '/03_REPORT_fp.csv'))


#
# CP equal opportunity ----
# Equal opportunity is the true negative rate for this model
#

tn_grp <- f1_grp_1 %>%
  mutate(gender_str = 'Z_ALL') %>%
  bind_rows(f1_grp_1) %>%
  dplyr::filter(bad_loan == '0') %>%
  group_by(model, gender_str) %>%
  dplyr::summarize(tn_rate = mean(tn)) %>%
  ungroup() 

tn_grp %>%
  pivot_wider(names_from = 'model',
              values_from = 'tn_rate') %>%
  fwrite(file.path(kOutputDir, '/03_REPORT_equal_opportunity.csv'))

#
# False positive plot ----
#

for (this_model in model_list) {
  
  this_thresh = threshold_df %>%
    dplyr::filter(model == this_model) %>%
    slice(1) %>%
    pull(thresh) 
  
  gp_fp_model <- fp_df %>%
    dplyr::filter(model == this_model &
                    gender_str %in% c('F', 'M')) %>%
    ggplot(aes(x=p1, fill = female)) +
    geom_density(alpha=0.15) +
    geom_vline(xintercept = this_thresh) +
    theme_minimal(base_size = 16) +
    theme(axis.text.y = element_blank(),
          legend.position = c(0.75,0.75),
          legend.background = element_rect(fill='white', color='gray')) +
    labs(x = 'Model Loan Default Probability') +
    scale_fill_manual(values=c('cyan4', 'coral3')) +
    labs(title=paste0('Model: ', this_model))
  
  print(gp_fp_model)
  
  ggsave(file.path(kOutputDir,
         paste0('03_PLOT_false_positives_', this_model, '.png')),
         gp_fp_model, type='cairo', width=7, height=4)
}

#
# CP false negative rate  ----
#

fn_df <- f1_grp_1 %>%
  mutate(gender_str = 'Z_ALL') %>%
  bind_rows(f1_grp_1) %>%
  dplyr::filter(bad_loan == '1')
fn_grp <- fn_df %>%
  group_by(model, gender_str) %>%
  dplyr::summarize(fn_rate = mean(fn)) %>%
  ungroup() 

fn_grp %>%
  pivot_wider(names_from = 'model',
              values_from = 'fn_rate') %>%
  fwrite(file.path(kOutputDir, '/03_REPORT_fn.csv'))
