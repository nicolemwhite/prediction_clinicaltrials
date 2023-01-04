# get-sample-sizes.R
# get historical data on sample size from clinicaltrials.gov
# see here for help https://stackoverflow.com/questions/44320008/parse-html-data-using-r
# March 2021
# Last updated: Jan 2022 (NW); sample_historical() refactored to catch all fields under study status and design
source('99_packages.R')
source('99_functions.R')

#studies <- readRDS("data/scoredTrials.rds") #changed to final_studies for consistency
load("data/final_studies.rda")


studies <- dat_included[['id']]
# # get the trials already done
# to_load = dir('data', pattern='history')
# all_data = NULL
# for (file in to_load){
#   load(paste('xml_folder', file, sep='/')) 
#   all_data = bind_rows(all_data, data)
# }
# 
# if(!is.null(all_data)){
#   already_done = select(all_data, id) %>%
#     unique() %>%
#     pull(id)
#   
#   studies = filter(studies, !id %in% already_done)
# }


data = NULL
start = 1
stop = min(length(studies),1000) #set to 1000 for final run
pb <- progress_bar$new(total=stop)


for (k in start:stop){
  # get the web page of the study's history
  url_start = 'https://clinicaltrials.gov/ct2/history/'
  url = paste(url_start, studies[k], sep='')
  site_search(url=url, destfile='web/history.html') # search with pauses if the site is tired of me
  
  # read the html page of the study changes
  page <- read_html('web/history.html') 
  
  # extract full history of study changes
  table = tibble(
   # version = str_remove_all(page %>% html_nodes("td:nth-child(1)") %>% html_text(), pattern='\\r|\\n'),
    dates = str_remove_all(page %>% html_nodes("td:nth-child(4)") %>% html_text(), pattern='\\r|\\n'),
    links = page %>% html_nodes("td:nth-child(4)")  %>% html_nodes("a") %>% html_attr("href")) %>%
    #changes = str_remove_all(page %>% html_nodes("td:nth-child(5)") %>% html_text(), pattern='\\r|\\n') %>% unlist()) %>%
    mutate(dates = as.Date(dates, '%B %d, %Y')) %>% # convert date
    arrange(dates) # order by date, just in case

  
  #get first entry/earliest version on record
  early = sample_historical(id = studies[k], intable = table, index=1)
  #get last recorded entry
  latest = sample_historical(id = studies[k], intable = table, index=nrow(table))
  

  
  #join early and latest
  data[[k]] = full_join(early,latest,by='field_label',suffix=c('_first','_last'))
  
  
  #if the latest entry has overall status = unknown & there are >2 updates, take the previous entry
  if (nrow(table)>2 & grepl('Unknown',filter(latest,field_label=='Overall Status')[['field_value']])){
    previous = sample_historical(id = studies[k], intable = table, index=nrow(table)-1) %>% rename(field_value_prev = field_value)
    data[[k]] = full_join(data[[k]],previous,by='field_label')
    }
 
  # 
  
  #if (!is.na(early$sample_size_type) & !is.na(early$sample_size)){
  #  data = bind_rows(data, early) #lets keep missing initial sample size
  #}
  ## only look for latest posting if table has more than 1 row - overwritten
# 
#   if(nrow(table_c)>=1){
#     late = sample_historical(id = studies$id[k], intable = table, index=nrow(table)) # from the last row in the table
#     if (!is.na(late$sample_size_type) & !is.na(late$sample_size)){
#       data = bind_rows(data, late)
#     }
#   }
#   

  # occasional save
  if(k %% 50 == 0){
    saveRDS(data, file="data/clintrials_sample_sizes_v2.rds")
  }
  
  # clean up downloaded pages
  to_remove = dir('web', pattern='.html')
  file.remove(paste('web/', to_remove, sep=''))
  
  pb$tick()
} # end of loop

names(data)<-studies[start:stop]
data <- bind_rows(data,.id='id')
data = distinct(data)


# save
saveRDS(data, file="data/clintrials_sample_sizes_v2.rds")

## data are combined in 2_combine_clinicaltrials_samplesize.R