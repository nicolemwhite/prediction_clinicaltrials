# 1_historical_clintrials.R
# get historical data on sample size from clinicaltrials.gov
# see here for help https://stackoverflow.com/questions/44320008/parse-html-data-using-r
# March 2021
source('99_packages.R')
source('99_functions.R')

load('data/processed_studies_test.rda')
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
stop = min(nrow(studies),1000)

for (k in 101:stop){
  # get the web page of the study's history
  url_start = 'https://clinicaltrials.gov/ct2/history/'
  url = paste(url_start, studies$id[k], sep='')
  site_search(url=url, destfile='web/history.html') # search with pauses if the site is tired of me
  
  # read the html page of the study changes
  page <- read_html('web/history.html') 
  
  # extract table of study changes
  table = tibble(
    dates = str_remove_all(page %>% html_nodes("td:nth-child(4)") %>% html_text(), pattern='\\r|\\n') ,
    links = page %>% html_nodes("td:nth-child(4)")  %>% html_nodes("a") %>% html_attr("href")
  ) %>%
    mutate(dates = as.Date(dates, '%B %d, %Y')) %>% # convert date
    arrange(dates) # order by date, just in case
  early = sample_historical(id = studies$id[k], intable = table, index=1)
  if (!is.na(early$sample_size_type) & !is.na(early$sample_size)){
    data = bind_rows(data, early)
  }
  ## only look for latest posting if table has more than 1 row
  if(nrow(table)>1){
    late = sample_historical(id = studies$id[k], intable = table, index=nrow(table)) # from the last row in the table
    if (!is.na(late$sample_size_type) & !is.na(late$sample_size)){
      data = bind_rows(data, late)
    }
  }
  # occasional save
  if(k %% 4000 ==0){
    outfile = paste('data/clintrials_history_mult', k,'.RData', sep='')
    save(data, k, file=outfile)
    data = NULL
  }
  
  # clean up downloaded pages
  to_remove = dir('web', pattern='.html')
  file.remove(paste('web/', to_remove, sep=''))
  
} # end of loop
data = distinct(data)

# save
save(data, k, file=paste0('data/clintrials_history.RData'))

## data are combined in 2_combine_clinicaltrials_samplesize.R