#
# To run the code, change the output path to a writeable
# directory, and make sure you have opened the project file
# in R Studio.
#
# Requires h2o.  Install directions:
# http://h2o-release.s3.amazonaws.com/h2o/rel-zermelo/1/index.html
#

require(dplyr)
require(h2o)

#
# Data output path ----
# Change this path to run code in your environment.
# Inputs come from web; see 01_data_process.R script
#

#kOutputDir <- file.path('/mnt/batch/tasks/shared/LS_root/mounts/clusters/valtest005/code/Users/vcarey/',
#                        '/Data/20201022_fairness/2020_12_02_rf')
kOutputDir <- file.path('/mnt/batch/tasks/shared/LS_root/mounts/clusters/valtest005/code/Users/vcarey/',
                                                '/Data/20201022_fairness/2021_01_12_xgb')

#
# Model type ----
# This project can be run using XGBoost or 
# Random forest models
#


# Model type to use ('xgboost' or 'randomForest')
#kModelType <- 'randomForest'
kModelType <- 'xgboost'

#
# Load common libraries ----
# Do not modify code past this point
#

# Install packages if needed
pkg_list <- c('tidyverse', 'data.table', 'dplyr', 'yaImpute',
              'ggplot2', 'Hmisc', 'rvest', 'zeallot', 'bit64',
              'PRROC', 'devtools', 'glmnet', 'iml', 'fastDummies')
installed_pkg <- installed.packages()[,1] %>% as.vector()
new_pkg <- pkg_list[!pkg_list %in% installed_pkg]
if (length(new_pkg) > 0) {
  install.packages(new_pkg)
}

library(data.table)
library(tidyverse)
library(ggplot2)
library(tidyverse)

#
# Source required functions ----
#

source('00_scripts/get_t_info.R')
source('00_scripts/model_load.R')
source('00_scripts/metrics_by_group.R')
source('00_scripts/get_corrleations.R')
source('00_scripts/plot_model_phi_side_by_side.R')
source('00_scripts/get_predictions.R')
