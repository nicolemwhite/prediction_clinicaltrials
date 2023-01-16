#time_to_completion_v2.R
#takes study updates until end of 2022 - ost xml download
source('99_packages.R')
source('99_functions.R')
dat <- readRDS("data/clintrials_sample_sizes_v2.rds")

#overall study statuse
dat_status = filter(dat,field_label=="Overall Status") %>% select(id,field_value_first,field_value_last,field_value_prev) %>% 
  mutate_at('field_value_last',~str_remove_all(.,'\\[.*$') %>% str_trim)

#hierarcy of rules

#if overall_status is completed:
## take last value for study completion (actual)
## if study completion is missing, take primary completion (actual)
## if both study and primary completion as missing, flag as an exclusion (update date not a good indicator)
completed_studies = filter(dat_status,field_value_last=="Completed") %>% distinct(id) %>% pull
completed_dates = filter(dat,id %in% completed_studies,
                         field_label %in% c('First Submitted thatMet QC Criteria','Study Start','Primary Completion','Study Completion','Overall Status')) %>%
  select(id,field_label,field_value_last) %>% spread(field_label,field_value_last) %>% select(id,`First Submitted thatMet QC Criteria`,`Study Start`,`Primary Completion`,`Study Completion`,`Overall Status`) %>% janitor::clean_names()


completed_dates = completed_dates %>% mutate(date_type_primary = case_when(
  grepl('Actual',primary_completion) ~ 'Actual',
  grepl('Anticipated',primary_completion) ~ 'Anticipated',
  grepl('Missing',primary_completion) ~ 'Missing',
  TRUE ~ 'Free-text'),
  date_type_study = case_when(
    grepl('Actual',study_completion) ~ 'Actual',
    grepl('Anticipated',study_completion) ~ 'Anticipated',
    grepl('Missing',study_completion) ~ 'Missing',
    TRUE ~ 'Free-text'),
)

#apply date rule for completion
completed_dates = completed_dates %>% mutate(
  date_end = case_when(
    #study_completion first
    date_type_study=='Actual'~str_remove_all(study_completion,'\\[.*$') %>% str_trim,
    date_type_study=='Free-text'~study_completion,
    #primary completion if study_completion is missing
    (date_type_study=='Missing' & date_type_primary=='Actual') ~ str_remove_all(primary_completion,'\\[.*$') %>% str_trim) %>% mdy) #else NA

#study start date - if only month-year available, assume 1 day of the month
completed_dates = completed_dates %>% mutate(date_start = ifelse(str_detect(study_start,','),study_start,paste0(str_remove_all(study_start,'\\s{1}.*$'),' 1, ',str_remove_all(study_start,'^.*\\s{1}'))) %>% mdy) %>% 
  select(id,date_start,date_end,overall_status)


#end completed studies

#if overall_status is: Active, not recruiting/Enrolling by invitation/Not yet recruiting/Recruiting 
## take last update that met QC

recruit_studies = filter(dat_status,field_value_last %in% c('Active, not recruiting','Enrolling by invitation','Not yet recruiting','Recruiting')) %>% distinct(id) %>% pull
recruit_dates = filter(dat,id %in% recruit_studies,
                       field_label %in% c('Study Start','Last Update Submitted thatMet QC Criteria','Overall Status')) %>%
  select(id,field_label,field_value_last) %>% spread(field_label,field_value_last) %>% select(id,`Study Start`,`Last Update Submitted thatMet QC Criteria`,`Overall Status`) %>% janitor::clean_names() %>%
  mutate_at('last_update_submitted_that_met_qc_criteria',~mdy(.)) %>%
  rename('date_end'=last_update_submitted_that_met_qc_criteria) %>%
  mutate(date_start = ifelse(str_detect(study_start,','),study_start,paste0(str_remove_all(study_start,'\\s{1}.*$'),' 1, ',str_remove_all(study_start,'^.*\\s{1}'))) %>% mdy) %>%
  select(id,date_start,date_end,overall_status)


#end active studies

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

#history of study status - get the first index where status changed to suspended/terminated/withdrawn
history_status = filter(history_stopped_unknown,field_label=='Overall Status') %>% select(id,index,field_value) %>% 
  mutate(overall_status = str_remove_all(field_value,'\\[.*$') %>% str_trim,reason=str_remove_all(field_value,'^.*\\[|\\]') ) 

index_stopped = filter(history_status,overall_status %in% c('Suspended','Terminated','Withdrawn')) %>% group_by(id) %>% slice_min(index) %>% ungroup() %>% select(id,index)
stopped_dates = left_join(index_stopped,history_stopped_unknown,by=c('id','index')) %>%  filter(field_label %in% c('Study Start','Last Update Submitted thatMet QC Criteria')) %>%
  select(id,field_label,field_value) %>% spread(field_label,field_value) %>% select(id,`Study Start`,`Last Update Submitted thatMet QC Criteria`) %>% janitor::clean_names() %>%
  mutate_at('last_update_submitted_that_met_qc_criteria',~mdy(.)) %>%
  rename('date_end'=last_update_submitted_that_met_qc_criteria) %>%
  mutate(date_start = ifelse(str_detect(study_start,','),study_start,paste0(str_remove_all(study_start,'\\s{1}.*$'),' 1, ',str_remove_all(study_start,'^.*\\s{1}'))) %>% mdy) %>%
  select(id,date_start,date_end) %>% add_column('overall_status'='Stopped',.after='id')

#unknown - take last known status
index_unknown = filter(history_status,overall_status=='Unknown status') %>% mutate(
  index = as.numeric(index),index_prev = as.character(ifelse(index>1,index-1,1))) %>% select(id,index_prev)
last_known_dates = left_join(index_unknown,history_stopped_unknown,by=c('id','index_prev'='index')) %>%  filter(field_label %in% c('Study Start','Last Update Submitted thatMet QC Criteria','Overall Status')) %>%
  select(id,field_label,field_value) %>% spread(field_label,field_value) %>% select(id,`Study Start`,`Last Update Submitted thatMet QC Criteria`,`Overall Status`) %>% janitor::clean_names() %>%
  mutate_at('last_update_submitted_that_met_qc_criteria',~mdy(.)) %>%
  mutate_at('overall_status',~str_remove_all(.,'^.*:\\s{1}|\\]$') %>% str_trim) %>%
  rename('date_end'=last_update_submitted_that_met_qc_criteria) %>%
  mutate(date_start = ifelse(str_detect(study_start,','),study_start,paste0(str_remove_all(study_start,'\\s{1}.*$'),' 1, ',str_remove_all(study_start,'^.*\\s{1}'))) %>% mdy) %>%
  select(id,date_start,date_end,overall_status) 


#combine studies into a single df
study_dates <- bind_rows(completed_dates,recruit_dates,stopped_dates,last_known_dates)

#for outstanding ids, take last post
outstanding_ids <-setdiff(unique(dat$id),unique(study_dates$id))
index_outstanding = filter(history_status,id %in% outstanding_ids) %>% group_by(id) %>% slice_max(index) %>% ungroup() %>% select(id,index)
outstanding_dates = left_join(index_outstanding,history_stopped_unknown,by=c('id','index')) %>%  filter(field_label %in% c('Study Start','Last Update Submitted thatMet QC Criteria','Overall Status')) %>%
  select(id,field_label,field_value) %>% spread(field_label,field_value) %>% select(id,`Study Start`,`Last Update Submitted thatMet QC Criteria`,`Overall Status`) %>% janitor::clean_names() %>%
  mutate_at('last_update_submitted_that_met_qc_criteria',~mdy(.)) %>%
  mutate_at('overall_status',~str_remove_all(.,'^.*:\\s{1}|\\]$') %>% str_trim) %>%
  rename('date_end'=last_update_submitted_that_met_qc_criteria) %>%
  mutate(date_start = ifelse(str_detect(study_start,','),study_start,paste0(str_remove_all(study_start,'\\s{1}.*$'),' 1, ',str_remove_all(study_start,'^.*\\s{1}'))) %>% mdy) %>%
  select(id,date_start,date_end,overall_status) 


study_dates <- bind_rows(study_dates,outstanding_dates)
#set exclusions
study_dates = study_dates %>% mutate(include_exclude = case_when(
  year(date_start)<2000 ~ 'Exclude:Pre-2000',
  is.na(date_end) ~ 'Exclude:Unknown primary/study completion dates',
  is.na(date_start) ~ 'Exclude:Unknown study start date',
  (!is.na(date_start) & !is.na(date_end) & date_start>=date_end) ~ 'Exclude: Not started as of last update',
  TRUE ~ 'Include'))

save(study_dates,file='data/times_to_study_completion.rda')

# add screening decisions
dat_decisions <- read.csv("data/rayyan/final_decisions_with_NCT.csv") %>%
  select(NCT, screening_final_decision = final_decision)
study_dates <- inner_join(study_dates, dat_decisions, by = c("id" = "NCT"))

#add meta data and "multiple designs" label
load("data/final_studies.rda")

dat_mult_designs <- 
  screening_results %>% 
  filter(NCT %in% study_dates$id) %>%
  mutate(multiple_designs = str_detect(labels, "multiple")) %>%
  select(NCT, multiple_designs)

study_dates <- inner_join(study_dates, dat_mult_designs, by = c("id" = "NCT"))

#plot - time to completed
censor_day <- 365.25*10
surv_dat = filter(study_dates,include_exclude=='Include') %>% mutate(status = ifelse(overall_status=='Completed',1,0),
                                                                     start=0,stop=as.numeric(date_end - date_start,'days'),
                                                                     #10-year censoring
                                                                     status_c = ifelse(stop>censor_day,0,status),
                                                                     overall_status_c = ifelse(stop>censor_day,'Censored',overall_status),
                                                                     stop_c = pmin(censor_day,stop))


g <- with(surv_dat,cuminc(ftime=stop_c/365.25,fstatus=overall_status_c,cencode='Censored')) %>% ggcompetingrisks()

head(g$data)


png('manuscript/figures/Figure4.png',width=1000,height=600)
g$data %>% mutate('Last known status'=str_remove_all(name,'^[0-9] ') %>% str_trim) %>% 
  ggplot(aes(x=time,y=est,ymin=est-1.96*sqrt(var),ymax=est+1.96*sqrt(var),group=`Last known status`,colour=`Last known status`,fill=`Last known status`))+geom_path()+geom_ribbon(alpha=0.1)+
  scale_x_continuous('Years since study start',breaks=0:10)+
  scale_y_continuous('Proportion of study records',breaks=seq(0,1,0.05))+
  #scale_colour_manual(values=cbPalette)+scale_fill_manual(values=cbPalette)+
  theme_minimal()+theme(panel.grid.minor = element_blank(),axis.text = element_text(size=12),text=element_text(size=12),
                        legend.position = 'top',legend.direction='horizontal')
invisible(dev.off())
