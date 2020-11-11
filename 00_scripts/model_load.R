#
# Functions related to loading models
#

model_load <- function(model_name, model_name_table,
                       base_path = kOutputDir) {
  return(h2o.loadModel(file.path(kOutputDir, 
                                 model_id_df[model_id_df$m == model_name, 2])))
}
