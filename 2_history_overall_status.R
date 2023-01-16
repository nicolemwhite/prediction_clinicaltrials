#2_history_overall_status.R
source('99_packages.R')
source('99_functions.R')
history_completed <- readRDS("data/clintrials_history_completed_studies.rds")
history_active <- readRDS("data/clintrials_history_active_studies.rds")
history_stopped_unknown <- readRDS("data/clintrials_history_stopped_unknown_studies.rds")


history_all_studies <- bind_rows(history_completed,history_active,history_stopped_unknown)

#once done, extract full history of status changes for each id, including dates posted
history_overall_status = filter(history_all_studies,field_label %in% c('Overall Status','Date Posted')) %>% distinct() %>%
  spread(field_label,field_value) %>% janitor::clean_names()

#for each change in status, take the earliest date posted
history_status_changes  = history_overall_status %>% group_by(id,overall_status) %>% slice_min(date_posted) %>% ungroup() %>% arrange(id,date_posted) %>% distinct(id,date_posted,overall_status)
#add column for next date updated. if missing, no more followup

history_status_changes = history_status_changes %>% group_by(id) %>% mutate(date_posted_next = lead(date_posted,1),overall_status_next=lead(overall_status,1)) %>%
  select(id,date_posted,date_posted_next,overall_status,overall_status_next) %>% 
  mutate_at(c('date_posted','date_posted_next'),~as.Date(.)) %>%
  ungroup()

#first posted date
history_first_posted = filter(history_all_studies,index==1,field_label=='First Submitted thatMet QC Criteria') %>%
  mutate(date_first_posted = mdy(field_value)) %>% select(id,date_first_posted)


#get date of study being labelled as unknown
unknown_dates = filter(history_status_changes,grepl("Unknown",overall_status_next)) %>% select(id,date_posted_next,overall_status_next)
unknown_dates = unknown_dates %>% mutate_at('overall_status_next',~str_remove_all(.,pattern='\\[.*$') %>% str_trim)
unknown_dates = right_join(history_first_posted,unknown_dates,by='id') %>% distinct()

#filter to non-missing dates to track changes
history_status_changes = history_status_changes %>% filter(!is.na(date_posted_next))


null_status_changes <- setdiff(unique(history_all_studies$id),unique(history_status_changes$id))

#note run: null changes
#history_all_studies %>% filter(id %in% null_status_changes) %>% head




#plot history of status changes for studies that posted them
#add first post that met QC criteria, study start (index = 1)

#days in model time
history_status_changes = right_join(history_first_posted,history_status_changes,by='id') %>%
  mutate(start = as.numeric(date_posted - date_first_posted,'days'),stop = as.numeric(date_posted_next - date_first_posted,'days') - 1) %>%
  mutate_at('overall_status',~str_remove_all(.,pattern='\\[.*$') %>% str_trim)

plot_history = history_status_changes %>% select(id,start,stop,overall_status) %>% 
  mutate_at('overall_status',~case_when(
    . %in% c('Suspended','Terminated','Withheld') ~ 'Suspended/Terminated/Withheld',
    TRUE ~ .
  )) %>% group_by(id,overall_status) %>% mutate(day = list(seq(start,stop,1))) %>% ungroup() %>%
  select(id,day,overall_status) %>% unnest(cols=day)

#plot up to 5 years
max_day= ceiling(365.25*25)
censor_day = ceiling(365.25*5)
#add unknown dates
ad = unknown_dates %>% group_by(id) %>% mutate(day=list(seq(as.numeric(date_posted_next - date_first_posted,'days'),max_day,1))) %>% select(id,day,overall_status_next) %>% rename('overall_status'=overall_status_next) %>%
  unnest(cols = day)                    

plot_history = bind_rows(plot_history,ad) %>% arrange(id,day)

g_final = plot_history %>% count(day,overall_status) %>% 
  group_by(overall_status) %>% mutate(cumulative_sum = cumsum(n)) %>% ungroup() %>%
  group_by(day) %>% mutate(prop = n/sum(n)) %>% ungroup() %>%
  filter(day<=censor_day) %>%
  ggplot(aes(x=day,y=prop,fill=overall_status))+geom_col(width=1)+theme_pubclean()+
  scale_x_continuous('Years since study first posted',breaks = seq(0,censor_day,365),labels=seq(0,censor_day,365)/365)+
  scale_y_continuous(paste0('Proportion of studies with status history (n = ',length(unique(plot_history$id)),')'),breaks=seq(0,1,0.1))+
  scale_fill_manual(values=cbPalette)+
  theme(legend.title = element_blank())

png('manuscript/figures/Figure5.png',width=1000,height=600)
g_final
dev.off()
