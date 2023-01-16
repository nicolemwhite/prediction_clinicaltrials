#2_history_stopped_unknown.R
source('99_packages.R')
source('99_functions.R')

load('data/stopped_unknown_studies.rda')


if (file.exists("data/clintrials_history_stopped_unknown_studies.rds")){
already_done<-readRDS("data/clintrials_history_stopped_unknown_studies.rds")
to_do <- setdiff(stopped_unknown_studies,unique(already_done$id))
}
if (!file.exists("data/clintrials_history_stopped_unknown_studies.rds")){
  to_do <- stopped_unknown_studies
  }

#get full history for completed studies
history_stopped_unknown = NULL
start = which(stopped_unknown_studies %in% to_do)[1]
stop = which(stopped_unknown_studies %in% to_do) %>% tail(1)
pb <- progress_bar$new(total=length(to_do))

for (k in start:stop){
  # get the web page of the study's history
  url_start = 'https://clinicaltrials.gov/ct2/history/'
  url = paste(url_start, stopped_unknown_studies[k], sep='')
  site_search(url=url, destfile='web/history.html') # search with pauses if the site is tired of me
  
  # read the html page of the study changes
  page <- read_html('web/history.html')
  
  # extract full history of study changes
  table = tibble(
    dates = str_remove_all(page %>% html_nodes("td:nth-child(4)") %>% html_text(), pattern='\\r|\\n'),
    links = page %>% html_nodes("td:nth-child(4)")  %>% html_nodes("a") %>% html_attr("href")) %>%
    mutate(dates = as.Date(dates, '%B %d, %Y')) %>% # convert date
    arrange(dates) # order by date, just in case
  
  
  #get first entry/earliest version on record
  record_history = lapply(1:nrow(table),function(x) sample_historical(id = stopped_unknown_studies[k], intable = table, index=x))
  history_stopped_unknown[[k]]<-bind_rows(record_history,.id='index') %>% add_column(id=stopped_unknown_studies[k])
  to_remove = dir('web', pattern='.html')
  file.remove(paste('web/', to_remove, sep=''))
  pb$tick()
}

history_stopped_unknown <- bind_rows(history_stopped_unknown)
history_stopped_unknown = distinct(history_stopped_unknown)


if (file.exists("data/clintrials_history_stopped_unknown_studies.rds")){
  ad<-history_stopped_unknown
  history_stopped_unknown<-bind_rows(already_done,ad)
}




saveRDS(history_stopped_unknown, file="data/clintrials_history_stopped_unknown_studies.rds")

#already run
# ##Suspended/Terminated/Withdrawn/Unknown - take retrospective history from last update to find date of status change
# stopped_unknown_studies = filter(dat_status,field_value_last %in% c('Suspended','Terminated','Withdrawn','Unknown status')) %>% distinct(id) %>% pull
# 
# 
# #get full history
# history_stopped_unknown = NULL
# start = 1
# stop = length(stopped_unknown_studies)
# pb <- progress_bar$new(total=stop)
# 
# 
# for (k in start:stop){
#   # get the web page of the study's history
#   url_start = 'https://clinicaltrials.gov/ct2/history/'
#   url = paste(url_start, stopped_unknown_studies[k], sep='')
#   site_search(url=url, destfile='web/history.html') # search with pauses if the site is tired of me
#   
#   # read the html page of the study changes
#   page <- read_html('web/history.html') 
#   
#   # extract full history of study changes
#   table = tibble(
#     # version = str_remove_all(page %>% html_nodes("td:nth-child(1)") %>% html_text(), pattern='\\r|\\n'),
#     dates = str_remove_all(page %>% html_nodes("td:nth-child(4)") %>% html_text(), pattern='\\r|\\n'),
#     links = page %>% html_nodes("td:nth-child(4)")  %>% html_nodes("a") %>% html_attr("href")) %>%
#     mutate(dates = as.Date(dates, '%B %d, %Y')) %>% # convert date
#     arrange(dates) # order by date, just in case
#   
#   
#   #get first entry/earliest version on record
#   record_history = lapply(1:nrow(table),function(x) sample_historical(id = stopped_unknown_studies[k], intable = table, index=x))
#   history_stopped_unknown[[k]]<-bind_rows(record_history,.id='index')
#   to_remove = dir('web', pattern='.html')
#   file.remove(paste('web/', to_remove, sep=''))
#   pb$tick()
# }
# 
# names(history_stopped_unknown)<-stopped_unknown_studies[start:stop]
# history_stopped_unknown <- bind_rows(history_stopped_unknown,.id='id')
# history_stopped_unknown = distinct(history_stopped_unknown)
# saveRDS(history_stopped_unknown, file="data/clintrials_history_stopped_unknown_studies.rds")

history_stopped_unknown<-readRDS("data/clintrials_history_stopped_unknown_studies.rds")