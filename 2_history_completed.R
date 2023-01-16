#2_history_completed.R
source('99_packages.R')
source('99_functions.R')
dat <- readRDS("data/clintrials_sample_sizes_v2.rds")

#overall study statuse
dat_status = filter(dat,field_label=="Overall Status") %>% select(id,field_value_first,field_value_last,field_value_prev) %>% 
  mutate_at('field_value_last',~str_remove_all(.,'\\[.*$') %>% str_trim)

load('data/completed_studies.rda')

already_done<-readRDS("data/clintrials_history_completed_studies.rds")

to_do <- setdiff(completed_studies,unique(already_done$id))

#get full history for completed studies
history_completed = NULL
start = which(completed_studies %in% to_do)[1]
stop = which(completed_studies %in% to_do) %>% tail(1)
pb <- progress_bar$new(total=stop)


for (k in start:stop){
  # get the web page of the study's history
  url_start = 'https://clinicaltrials.gov/ct2/history/'
  url = paste(url_start, completed_studies[k], sep='')
  site_search(url=url, destfile='web/history.html') # search with pauses if the site is tired of me

  # read the html page of the study changes
  page <- read_html('web/history.html')

  # extract full history of study changes
  table = tibble(
    # version = str_remove_all(page %>% html_nodes("td:nth-child(1)") %>% html_text(), pattern='\\r|\\n'),
    dates = str_remove_all(page %>% html_nodes("td:nth-child(4)") %>% html_text(), pattern='\\r|\\n'),
    links = page %>% html_nodes("td:nth-child(4)")  %>% html_nodes("a") %>% html_attr("href")) %>%
    mutate(dates = as.Date(dates, '%B %d, %Y')) %>% # convert date
    arrange(dates) # order by date, just in case


  #get first entry/earliest version on record
  record_history = lapply(1:nrow(table),function(x) sample_historical(id = completed_studies[k], intable = table, index=x))
  history_completed[[k]]<-bind_rows(record_history,.id='index') %>% add_column(id=completed_studies[k])
  to_remove = dir('web', pattern='.html')
  file.remove(paste('web/', to_remove, sep=''))
  pb$tick()
}




# progress save
#names(history_completed)<-completed_studies[start:stop]
history_completed <- bind_rows(history_completed)
ad = distinct(history_completed)
history_completed<-bind_rows(already_done,ad)


saveRDS(history_completed, file="data/clintrials_history_completed_studies.rds")




