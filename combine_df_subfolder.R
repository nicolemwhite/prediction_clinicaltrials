#combine_df_subfolder
source('99_packages.R')
source('99_functions.R')

filenames = list.files('data/processed')

dat = NULL

for (k in seq_along(filenames)){
  load(paste('data/processed',filenames[k],sep='/'))
  dat = bind_rows(dat,studies)
}

save(dat,file='data/analysis_ready/study_data_v1.rda')
