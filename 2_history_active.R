#2_history_active.R
source('99_packages.R')
source('99_functions.R')
dat <- readRDS("data/clintrials_sample_sizes_v2.rds")

#overall study status
dat_status = filter(dat,field_label=="Overall Status") %>% select(id,field_value_first,field_value_last,field_value_prev) %>% 
  mutate_at('field_value_last',~str_remove_all(.,'\\[.*$') %>% str_trim)

load('data/active_studies.rda')

already_done<-readRDS("data/clintrials_history_active_studies.rds")

to_do <- setdiff(recruit_studies,unique(already_done$id))

#get full history for completed studies
history_recruit = NULL
start = which(recruit_studies %in% to_do)[1]
stop = which(recruit_studies %in% to_do) %>% tail(1)
pb <- progress_bar$new(total=stop)


for (k in start:stop){
  # get the web page of the study's history
  url_start = 'https://clinicaltrials.gov/ct2/history/'
  url = paste(url_start, recruit_studies[k], sep='')
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
  record_history = lapply(1:nrow(table),function(x) sample_historical(id = recruit_studies[k], intable = table, index=x))
  history_recruit[[k]]<-bind_rows(record_history,.id='index') %>% add_column(id=recruit_studies[k])
  to_remove = dir('web', pattern='.html')
  file.remove(paste('web/', to_remove, sep=''))
  pb$tick()
}


# progress save: last k=90
#names(history_recruit[start:stop])<-recruit_studies[start:stop]
history_recruit <- bind_rows(history_recruit)
history_recruit = distinct(history_recruit)

#start_k <- min(history_recruit$id)
#stop_k <- max(history_recruit$id)

ad<-history_recruit
#ad = ad %>% mutate_at('id',~recruit_studies[start:stop][as.numeric(.)])
history_recruit<-bind_rows(already_done,ad)

saveRDS(history_recruit, file="data/clintrials_history_active_studies.rds")
