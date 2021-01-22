#
# Examines tree structure for selected
# feature bias models.  Examines female-income interactions.
# For now, examines only models Q and S
#


rm(list=ls())
source('00_setup.R')

library(h2o)

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


#
# Functions ----
#

# Get decision paths ending at leaf nodes only
get_decision_path = function(node, prior_values) {
  
  if(class(node)[1] == 'H2OLeafNode') {
    return(paste(prior_values, as.character(node@prediction), sep=','))
  } 

  if (!is.null(prior_values)) {
    this_values= paste(prior_values, node@split_feature, sep=',')
  } else {
    this_values = node@split_feature
  }
  
  left_info <- get_decision_path(node@left_child, this_values)
  right_info <- get_decision_path(node@right_child, this_values)
  
  return(c(left_info, right_info))

}


# Count decision paths with income, female status, both, or neither
dec_path_type = function(decision_path, oth_feat = 'female_pq') {
  
  female_match = paste0('^', oth_feat, '|,', oth_feat)
  
  has_income = grepl('^annual_inc_pq|,annual_inc_pq', decision_path)
  has_female = grepl(female_match, decision_path)
  
  if (!(has_income | has_female)) {
    return('neither')
  } else if (has_income & has_female) {
    return('both')
  } else if (has_income ) {
    return('income')
  } else {
    return('female')
  }
  
}

# Count depth of path
dec_path_depth = function(decision_path) {
  return(lengths(regmatches(decision_path, gregexpr(",", decision_path))))
}
  
# For model S, include thresholds in values
get_decision_path_w_thresh = function(node, prior_values) {
  
  if(class(node)[1] == 'H2OLeafNode') {
    return(paste(prior_values, as.character(node@prediction), sep=','))
  } 
  
  this_item = paste0(node@split_feature, '#', as.character(node@threshold))
  
  if (!is.null(prior_values)) {
    this_values= paste(prior_values, this_item, sep=',')
  } else {
    this_values = this_item
  }
  
  left_info <- get_decision_path_w_thresh(node@left_child, this_values)
  right_info <- get_decision_path_w_thresh(node@right_child, this_values)
  
  return(c(left_info, right_info))
  
}

# Get thresholds for a feature from decision path.
# Return all
dec_path_thresh <- function(data, filt_feature) {
  
  items_df = data.frame(item = strsplit(as.character(data$path[1]), ',')[[1]]) %>%
    mutate(path_level = seq_len(n())) %>%
    separate(item, into=c('feature', 'threshold'), sep='#') %>%
    dplyr::filter(feature == filt_feature) %>%
    mutate(threshold= as.numeric(threshold)) %>%
    dplyr::select(path_level, threshold)
  
  return(items_df)
}


#
# Model Q -----
# Look at all trees, and get statistics
#


this_model = 'q'
this_model_h2o = model_list_h2o[[this_model]] 

num_trees = this_model_h2o@parameters$ntrees

# Loop through trees and get key statistics
tree_statistics = data.frame()

for (this_tree_id in seq(1, num_trees)) {
  
  htree = h2o.getModelTree(this_model_h2o, tree_number=this_tree_id)
  
  # Get the initial stats
  this_stats = data.frame(tree_number = htree@tree_number,
                          num_nodes = length(htree),
                          max_depth_model = this_model_h2o@parameters$max_depth)
  
  
  # Count total nodes with income, female features
  this_stats <- this_stats %>%
    mutate(num_nodes_female_pq  = sum(htree@features == 'female_pq', na.rm=T),
           num_nodes_annual_inc_pq= sum(htree@features == 'annual_inc_pq', na.rm=T))
  
  # Get terminal decision paths
  term_dec_path <- get_decision_path(htree@root_node, NULL)
  
  # Get the types of each path, count
  term_dec_path_types <- sapply(term_dec_path, dec_path_type, simplify = "array", USE.NAMES = F)
  term_dec_path_types_table <- table(term_dec_path_types)
  term_dec_path_df <- data.frame(tname = names(term_dec_path_types_table),
                                 tval = as.vector(term_dec_path_types_table)) %>%
    pivot_wider(names_from='tname', values_from='tval') %>%
    rename_all(function(x) paste0('dec_path_type_', x))
  
  this_stats <- this_stats %>%
    mutate(dec_path_count = length(term_dec_path_types)) %>%
    bind_cols(term_dec_path_df)
  
  # Get decision path depths
  dec_path_depths <- sapply(term_dec_path, dec_path_depth, simplify = "array", USE.NAMES = F)
  this_stats <- this_stats %>%
    mutate(dec_path_depth_min = min(dec_path_depths),
           dec_path_depth_max = max(dec_path_depths),
           dec_path_depth_median = median(dec_path_depths),
           dec_path_depth_mean = mean(dec_path_depths)) 
  

  tree_statistics <- tree_statistics %>%
    bind_rows(this_stats)
  
}  

saveRDS(tree_statistics, file.path(kOutputDir, '/07_DATA_model_q_tree_stats.rds'))

summary(tree_statistics$dec_path_count)
summary(tree_statistics$dec_path_type_female)
summary(tree_statistics$dec_path_type_income)
summary(tree_statistics$dec_path_type_both)

# Replace zeros


# Aggregate tree statistics
tree_statistics_overall <- tree_statistics %>%
  dplyr::select(-tree_number) %>%
  replace(is.na(.), 0) %>%
  summarize_all(mean) %>%
  bind_cols(trees_with_no_fem_only = sum(is.na(tree_statistics$dec_path_type_female)))
fwrite(tree_statistics_overall,  file.path(kOutputDir, '/07_DATA_model_q_tree_stats_mean.csv'))

# Get some sample trees
tree_statistics %>%
  sample_n(3) %>%
  fwrite(file.path(kOutputDir, '/07_DATA_model_q_tree_stats_sample.csv'))

# Plot histogram of number of trees with female only
gp_fem_hist <- tree_statistics %>%
  replace(is.na(.), 0) %>%
  ggplot(aes(x=dec_path_type_female)) +
  geom_histogram(binwidth=3) +
  theme_minimal(base_size = 14) +
  labs(x = '# paths with female status but not income',
       y= '# trees')
print(gp_fem_hist)
ggsave(file.path(kOutputDir, '/07_PLOT_model_q_hist_female_only.png'),
       gp_fem_hist, type='cairo', width=5, height = 4)

# Scatter of female only vs both type
gp_tree_scatter <- tree_statistics %>%
  dplyr::select(tree_number, dec_path_type_both, dec_path_type_female) %>%
  replace(is.na(.), 0) %>%
  ggplot(aes(x=dec_path_type_both, y=dec_path_type_female)) +
  geom_point() +
  theme_minimal(base_size = 14) +
  labs(y = '# paths with female status but not income',
       x = '# paths with both female status and income')
print(gp_tree_scatter)
ggsave(file.path(kOutputDir, '/07_PLOT_model_q_scatter_dec_path_fem_both.png'),
       gp_tree_scatter, type='cairo', width=5, height = 4)  


#
# Model S ----
# Examine split points for the income feature, and in particular
# paths with low cut points and no income interactions
#

this_model = 's'
this_model_h2o = model_list_h2o[[this_model]] 

num_trees_s = this_model_h2o@parameters$ntrees

tree_statistics_s = data.frame()
decision_path_summary = data.frame()
thresholds_nonincome = data.frame()

# Loop through trees and get key statistics

for (this_tree_id in seq(1, num_trees_s)) {
  htree_s = h2o.getModelTree(this_model_h2o, tree_number=this_tree_id)
  
  # Get the initial stats
  this_stats = data.frame(tree_number = htree_s@tree_number,
                          num_nodes = length(htree_s),
                          max_depth_model = this_model_h2o@parameters$max_depth)
  

  
  # Get decision paths with thresholds
  term_dec_path_s <- get_decision_path_w_thresh(htree_s@root_node, NULL)
  
  # Finalize basic statistics
  this_stats <- this_stats %>%
    mutate(num_nodes_inc_female_pq  = sum(htree_s@features == 'inc_female_pq', na.rm=T),
           num_nodes_annual_inc_pq= sum(htree_s@features == 'annual_inc_pq', na.rm=T),
           num_dec_paths = term_dec_path_s %>% length())
  
  tree_statistics_s <- tree_statistics_s %>%
    bind_rows(this_stats)
  
  
  # Summarize decision paths
  term_dec_path_types_s <- sapply(term_dec_path_s, 
                                  function(x)  dec_path_type(x, 'inc_female_pq'), 
                                  simplify = "array", USE.NAMES = F)
  term_dec_path_types_table_s <- table(term_dec_path_types_s)
  term_dec_path_df_s <- data.frame(tname = names(term_dec_path_types_table_s),
                                 tval = as.vector(term_dec_path_types_table_s)) %>%
    pivot_wider(names_from='tname', values_from='tval') %>%
    rename_all(function(x) paste0('dec_path_type_', x)) %>%
    mutate(tree_number = this_tree_id,
           num_dec_paths = term_dec_path_s %>% length())
  

  # Get split points for the inc_female_pq features
  this_thresholds <- data.frame(path = term_dec_path_s) %>%
    mutate(path_id = seq_len(n())) %>%
    group_by(path_id) %>%
    do(dec_path_thresh(., 'inc_female_pq')) %>%
    ungroup()
  
  # Find paths with income feature
  this_inc_feat <- data.frame(path = term_dec_path_s) %>%
    mutate(path_id = seq_len(n())) %>%
    dplyr::filter(grepl('annual_inc_pq', path)) %>%
    distinct(path_id)
  
  # Get paths without income feature, and with thresholds near 0
  this_thresholds_nonincome <- this_thresholds %>%
    anti_join(this_inc_feat, by='path_id') %>%
    mutate(tree_number = this_tree_id)
  
  thresholds_nonincome <- thresholds_nonincome %>%
    bind_rows(this_thresholds_nonincome)
  
  # Add low threshold info to the decision path summary
  this_thresholds_sm <- this_thresholds %>%
    mutate(low_thresh_5k = ifelse(threshold < 5000, 1, 0),
           low_thresh_10k = ifelse(threshold < 10000, 1, 0)) %>%
    group_by(path_id) %>%
    dplyr::summarize(low_thresh_5k = max(low_thresh_5k),
                     low_thresh_10k = max(low_thresh_10k)) %>%
    ungroup() %>%
    dplyr::summarize(low_thresh_5k = sum(low_thresh_5k),
                     low_thresh_10k = sum(low_thresh_10k)) %>%
    ungroup()
  
  decision_path_summary <- decision_path_summary %>%
    bind_rows(term_dec_path_df_s %>%
                bind_cols(this_thresholds_sm))
  
}


saveRDS(tree_statistics_s, 
        file.path(kOutputDir, '/07_DATA_model_s_tree_stats.rds'))
saveRDS(thresholds_nonincome, 
        file.path(kOutputDir, '/07_DATA_model_s_thresholds_nonincome.rds'))
saveRDS(decision_path_summary,
        file.path(kOutputDir, '/07_DATA_model_s_decision_path_summary.rds'))

# Get mean decision path info
decision_path_info_mean <- decision_path_summary %>% 
  replace(is.na(.), 0) %>%
  dplyr::select(-tree_number) %>%
  dplyr::summarize_all(mean)

decision_path_info_mean %>%
  fwrite(file,path(kOutputDir, '/07_DATA_model_s_decision_path_summary_mean.csv'))

