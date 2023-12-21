#2_history_completed.R
source('99_packages.R')
source('99_functions.R')

load("data/final_studies_with_meta_R1.rda")

web_history = NULL
studies = filter(decisions,final_decision =='include',overall_status %in% c('Completed')) %>% distinct(NCT) %>% pull()

f <- "data/clintrials_history_completed.rds"
if (file.exists(f)){
  already_done<-readRDS(f)
  to_do <- setdiff(studies,unique(already_done$NCT))
}
if(!file.exists(f)){to_do <- studies;already_done<-NULL}

#get full history for completed studies
start = which(studies %in% to_do)[1]
stop = which(studies %in% to_do) %>% tail(1)
pb <- progress_bar$new(total=stop)


for (k in start:stop){
  # get the web page of the study's history
  url_start = 'https://classic.clinicaltrials.gov/ct2/history/'
  url = paste(url_start, studies[k], sep='')
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
  record_history = lapply(1:nrow(table),function(x) sample_historical(id = studies[k], intable = table, index=x))
  web_history[[k]]<-bind_rows(record_history,.id='index') %>% add_column(NCT=studies[k])
  to_remove = dir('web', pattern='.html')
  file.remove(paste('web/', to_remove, sep=''))
  pb$tick()
}


# progress save: last k=90
#names(history_recruit[start:stop])<-recruit_studies[start:stop]
#start_k <- min(history_recruit$id)
#stop_k <- max(history_recruit$id)

web_history <- bind_rows(web_history) %>% distinct() %>% select(NCT,index,field_label,field_value) %>% group_by(NCT,index) %>% summarise(across(field_label:field_value,~list(.x)),.groups='drop')
already_done<-bind_rows(already_done,web_history)
already_done <- already_done %>% mutate_at('index',~as.numeric(.)) %>% arrange(NCT,index)
saveRDS(already_done, file="data/clintrials_history_completed.rds")
