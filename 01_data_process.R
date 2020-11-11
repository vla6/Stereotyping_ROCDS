#
# Input loans data set, and do some data cleaning
# and recoding of state into geographical region.
#
# Create features for assessing stereotyping and
# feature bias.
#
# Stereotyping: Create an artificial "gender" field, 
# which is correlated with income, to demonstrate 
# methods for distinguishing stereotyping from
# decisions made based on potentially reasonable 
# factors
#
# Feature bias:  Assign genders randomly then create
# an income feature which is artifically depressed for
# females only.

rm(list=ls())
source('00_setup.R')

#
# Import census regions ----
# Raw location https://raw.githubusercontent.com/cphalpert/census-regions/master/us%20census%20bureau%20regions%20and%20divisions.csv
# See also https://github.com/cphalpert/census-regions/blob/master/us%20census%20bureau%20regions%20and%20divisions.csv
#

census_data <- fread("https://raw.githubusercontent.com/cphalpert/census-regions/master/us%20census%20bureau%20regions%20and%20divisions.csv")

census_data  %>% glimpse()

# 
# Import credit data ----
# Note original location 
# "https://raw.githubusercontent.com/h2oai/app-consumer-loan/master/data/loan.csv"
# https://github.com/h2oai/app-consumer-loan
# Map census regions
# 

credit_data <- fread("https://raw.githubusercontent.com/h2oai/app-consumer-loan/master/data/loan.csv", 
              colClasses = c('loan_amnt' = 'integer',
                             'term' = 'factor',
                             'int_rate' = 'numeric',
                             'emp_length' = 'integer',
                             'home_ownership' = 'character',
                             'annual_inc' = 'numeric',
                             'purpose' = 'factor',
                             'addr_state' = 'character',
                             'dti' = 'numeric',
                             'delinq_2yrs' = 'numeric',
                             'revol_util' = 'numeric',
                             'total_acc' = 'numeric',
                             'bad_loan' = 'integer',
                             'longest_credit_length' = 'integer',
                             'verification_status' = 'factor')) %>%
  left_join(census_data  %>%
              rename(State2 = State,
                     State = `State Code`) %>%
              dplyr::select(State, Region) %>%
              mutate(region_name = as_factor(Region)) %>%
              dplyr::select(-Region),
            by=c('addr_state' = 'State')) %>%
  mutate(ID = seq_len(n()),
         bad_loan = factor(bad_loan, levels=c(0,1)),
         home_ownership = factor(home_ownership,
                                 levels=c('MORTGAGE', 'OWN', 'RENT', 'NONE',
                                          'OTHER', 'ANY'),
                                 labels=c('MORTGAGE', 'OWN', 'RENT', 'OTHER',
                                          'OTHER', 'OTHER'))) %>%
  as.data.frame()

nrow(credit_data) # 163,987
credit_data %>% filter(complete.cases(.)) %>% nrow() # 157996 

# Save the initial feature set for later use
credit_data_predictors_init <- names(credit_data)[!names(credit_data) %in%
                                                    c("bad_loan", "int_rate", "ID", "addr_state")]
saveRDS(credit_data_predictors_init,
        file.path(kOutputDir, '01_DATA_original_feature_set.rds'))
#
# NA audit ----
#

na_info <- credit_data %>%
  dplyr::filter_all(any_vars(is.na(.))) %>%
  mutate_at(vars(-ID), as.character) %>%
  as.data.table() %>%
  melt(id.var = 'ID') %>%
  dplyr::filter(is.na(value)) %>%
  group_by(variable) %>%
  dplyr::summarize(tot_na = n()) %>%
  ungroup()
print(na_info)
na_info %>% 
  fwrite(paste0(kOutputDir, '/SM_AUDIT_na_list.csv'))


#
# Stereotyping analysis features ----
# Assign "female" status based on income
#

# Assign females more often at lower income levels.
female_prob_sigmoid <- function(income) {
  return(1/2- (((income/20000)-2.25)/
                 (sqrt(1+((income/10000)-4.5)**2))))
}

# Assign female status, and create a copy of the feature for later
# assessments.
set.seed(123)
credit_data_filt_1 <- credit_data %>% filter(complete.cases(.)) %>%
  mutate(female_prob = female_prob_sigmoid(annual_inc),
         unif_v = runif(n()),
         female = factor(case_when(female_prob >= unif_v ~ 1,
                               TRUE ~ 0), levels=c(0,1)),
         female2 = female) %>%
  dplyr::select(-starts_with('female_prob'),
                -starts_with('unif_v'))


# Plot overall income histogram
gp_hist_inc_orig <- credit_data_filt_1 %>%
  dplyr::filter(annual_inc<=150000) %>%
  ggplot(aes(x=annual_inc)) +
  geom_density(bw=5000) +
  theme_minimal(base_size = 12) +
  theme(axis.text.y  =element_blank()) +
  scale_x_continuous(labels=scales::comma)

print(gp_hist_inc_orig)
ggsave(file.path(kOutputDir, '/01_PLOT_hist_income.png'),
       gp_hist_inc_orig, type='cairo', width=5, height=4)

# Plot income by our inferred "gender"
gp_hist_inc_sex <- credit_data_filt_1 %>%
  dplyr::filter(annual_inc<=150000) %>%
  ggplot(aes(x=annual_inc, fill=female)) +
  geom_density(bw=5000, alpha=0.15) +
  theme_minimal(base_size = 12) +
  theme(axis.text.y  =element_blank(),
        legend.position=c(0.8,0.8)) +
  scale_x_continuous(labels=scales::comma) +
  scale_fill_manual(values=c('turquoise', 'pink'))

print(gp_hist_inc_sex)
ggsave(file.path(kOutputDir, '01_PLOT_hist_income_sex.png'),
       gp_hist_inc_sex, type='cairo', width=5, height=4)

#
# Feature bias analysis features ----
# Create features to support feature bias
# testing. These models are evaluated separately
# and use a different female definition
#

credit_data_filt <-credit_data_filt_1 %>%
  mutate(female_pq = factor(case_when(runif(n()) <= 0.4 ~ 1,
                                       TRUE ~ 0),
                             levels=c(0,1)),
         annual_inc_pq = case_when(female_pq == '1' ~ 
                                     pmax(0, annual_inc*(1- rnorm(n(), mean=0.4,
                                                                  sd=0.3))),
                                   TRUE ~ annual_inc),
        inc_female_pq = as.numeric(as.character(female_pq)) * annual_inc_pq)

credit_data_filt %>%
  group_by(female_pq) %>%
  dplyr::summarize(mean(annual_inc_pq))

# By sex feature bias income
gp_hist_inc_sex_fb <- credit_data_filt %>%
  dplyr::filter(annual_inc_pq<=150000) %>%
  ggplot(aes(x=annual_inc_pq, fill=female_pq)) +
  geom_density(bw=5000, alpha=0.15) +
  theme_minimal(base_size = 12) +
  theme(axis.text.y  =element_blank(),
        legend.position=c(0.8,0.8)) +
  scale_x_continuous(labels=scales::comma) +
  scale_fill_manual(values=c('turquoise', 'pink')) +
  labs(title='feature bias income')

print(gp_hist_inc_sex_fb)
ggsave(file.path(kOutputDir, '01_PLOT_hist_feature_bias_feat.png'),
       gp_hist_inc_sex, type='cairo', width=5, height=4)

saveRDS(credit_data_filt,
        file.path(kOutputDir, '/01_DATA_base_gender_inf.rds'))


#
# Split train-test-validation data sets ----
# Use 60/15/25 split
# Train is used for model building.
# Test is used for hyperparameter tuning only
# Validation is used for fairness metrics and Shapley values
#

set.seed(123)

split_id_train <- credit_data_filt %>%
  dplyr::select(ID) %>%
  sample_frac(0.6)

split_id_test <- credit_data_filt %>%
  dplyr::select(ID) %>%
  anti_join(split_id_train, by='ID') %>%
  sample_frac(0.375)

split_id_val <- credit_data_filt %>%
  dplyr::select(ID) %>%
  anti_join(split_id_train, by='ID') %>%
  anti_join(split_id_test, by='ID') 

saveRDS(split_id_train,
        file.path(kOutputDir, '/01_DATA_split_id_train.rds'))
saveRDS(split_id_test,
        file.path(kOutputDir, '/01_DATA_split_id_test.rds'))
saveRDS(split_id_val,
        file.path(kOutputDir, '/01_DATA_split_id_val.rds'))

#
# Correlations ----
#

cor_sex <- cor(as.numeric(as.character(credit_data_filt$female)),
    as.numeric(as.character(credit_data_filt$bad_loan)))
cor_raw <- cor(as.numeric(credit_data_filt$annual_inc),
    as.numeric(as.character(credit_data_filt$bad_loan)))

cor_df <- data.frame(sex = cor_sex, raw = cor_raw)

fwrite(cor_df, file.path(kOutputDir, '/01_REPORT_correlations.csv'))

#
# Summary stats ----
# Percent female, median income, etc.
#

# Stats relevant for models A-E

desc_df <- credit_data_filt %>%
  group_by(female) %>%
  dplyr::summarize(cnt= n(),
                   median_income = median(annual_inc),
                   default_rate = mean(bad_loan == '1')) %>%
  ungroup() %>%
  mutate(fract = cnt/sum(cnt)) %>%
  dplyr::select(female, cnt, fract, median_income, default_rate)

print(desc_df)

fwrite(desc_df, file.path(kOutputDir, '01_REPORT_sex_stats_abcde.csv'))

# Stats relevant for models P-Q

desc_df_pq <- credit_data_filt %>%
  group_by(female_pq) %>%
  dplyr::summarize(cnt= n(),
                   median_income = median(annual_inc_pq),
                   default_rate = mean(bad_loan == '1')) %>%
  ungroup() %>%
  mutate(fract = cnt/sum(cnt)) %>%
  dplyr::select(female_pq, cnt, fract, median_income, default_rate)

print(desc_df_pq)

fwrite(desc_df_pq, file.path(kOutputDir, '01_REPORT_sex_stats_pqrs.csv'))
