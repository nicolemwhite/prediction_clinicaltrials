#3_process_history.R
source('99_packages.R')
source('99_functions.R')

active <- readRDS("data/clintrials_history_active_studies.rds")
complete <- readRDS("data/clintrials_history_completed.rds")
unknown <- readRDS("data/clintrials_history_unknown_stopped.rds")

web_history = bind_rows(active,complete,unknown)

dat_dup = openxlsx::read.xlsx('data/manual checks/possible duplicates-20230713.xlsx')
exclude_dup = dat_dup %>% filter(duplicate=='y') %>% pull(b) %>% unique()

web_history = filter(web_history,!NCT %in% exclude_dup)

#get changes in overall status
#for each NCT and overall status, take the first date posted
dat_status = web_history %>% unnest(cols=c(field_label,field_value)) %>% filter(field_label %in% c('Date Posted','Overall Status')) %>% spread(field_label,field_value) 

dat_status_c = dat_status %>% group_by(NCT,`Overall Status`) %>% slice_min(`Date Posted`) %>% ungroup() %>% janitor::clean_names() %>% arrange(nct,index)

dat_status_c = dat_status_c %>% group_by(nct) %>% 
  mutate_at('overall_status',~str_remove_all(.,'\\s+\\[.*$')) %>%
  mutate_at('overall_status',~case_when(. %in% c('Suspended','Terminated','Withdrawn','Withheld') ~ 'Suspended/Terminated/Withdrawn',TRUE~as.character(.))) %>%
  mutate(date_update = lead(date_posted,1),date_start=min(date_posted),
                                                         from=overall_status,to=coalesce(lead(overall_status,1),overall_status)) %>% ungroup() %>%
  mutate_at(c('date_update','date_posted','date_start'),~as.Date(.)) %>% rowwise() %>%
  mutate_at('date_update',~replace_na(.,date_posted+years(5)))
  

to_plot = dat_status_c %>% mutate(start=pmax(0,as.numeric(date_posted - date_start)),stop=pmax(0,as.numeric(date_update - date_start)-1)) %>% 
  mutate_at('start',~ifelse(.==stop+1,stop,.)) %>% mutate_at('stop',~ifelse(start==.,.+1,.)) %>% rowwise() %>%
  mutate(day=list(seq(start,stop))) %>% unnest(cols=day) %>% select(nct,day,from,to,day) %>% distinct() %>%
  group_by(day,from) %>% summarise(n=n(),.groups='drop_last') %>% mutate(prop=n/sum(n)) %>% ungroup() %>%
  filter(between(day,1,1825))


ga = ggplot(to_plot,aes(x=day,y=prop,fill=from))+geom_col(width=1)+scale_fill_manual(values=c(cbPalette,'black'))+
  scale_x_continuous('Years since first posted',breaks=seq(0,1825,365),labels=seq(0,1825,365)/365,expand = c(0, 0), limits = c(0, NA))+theme_bw()+
  scale_y_continuous('Proportion of included studies',breaks=seq(0,1,0.1),expand = c(0, 0), limits = c(0, NA))+expand_limits(x=10)+
  theme(legend.title = element_blank(),legend.position = 'top',text=element_text(size=12),legend.text=element_text(size=12),axis.text = element_text(size=12),panel.grid.minor = element_blank())

png('manuscript/figures/status_over_time.png',width=9,height=6,units='in',res=300)
ga
invisible(dev.off())

# png('manuscript/figures/FigureS5B.png',width=8,height=6,units='in',res=300)
# gb
# invisible(dev.off())


#kaplan meier for time to completion
to_plot = dat_status_c %>% mutate(stop=pmax(1,as.numeric(date_posted - date_start)-1),status=ifelse(to=='Completed',1,0)) %>% 
  group_by(nct) %>% slice_max(index) %>% ungroup()
mod.fit <- survfit(Surv(stop,status)~1,data=to_plot)

# 
# #change time horizon to first 2 years
# dat_status_c = dat_status %>% group_by(NCT,`Overall Status`) %>% slice_min(`Date Posted`) %>% ungroup() %>% janitor::clean_names() %>% arrange(nct,index)
# 
# dat_status_c = dat_status_c %>% group_by(nct) %>% 
#   mutate_at('overall_status',~str_remove_all(.,'\\s+\\[.*$')) %>%
#   mutate_at('overall_status',~case_when(. %in% c('Suspended','Terminated','Withdrawn','Withheld') ~ 'Suspended/Terminated/Withdrawn',TRUE~as.character(.))) %>%
#   mutate(date_update = lead(date_posted,1),date_start=min(date_posted),
#          from=overall_status,to=coalesce(lead(overall_status,1),overall_status)) %>% ungroup() %>%
#   mutate_at(c('date_update','date_posted','date_start'),~as.Date(.)) %>% rowwise() %>%
#   mutate_at('date_update',~replace_na(.,date_posted+days(1)))
# 
# 
# to_plot = dat_status_c %>% mutate(start=pmax(0,as.numeric(date_posted - date_start)),stop=pmax(0,as.numeric(date_update - date_start)-1)) %>% 
#   mutate_at('start',~ifelse(.==stop+1,stop,.)) %>% mutate_at('stop',~ifelse(start==.,.+1,.)) %>% rowwise() %>%
#   mutate(day=list(seq(start,stop))) %>% unnest(cols=day) %>% select(nct,day,from,to,day) %>% distinct() %>%
#   group_by(day,from) %>% summarise(n=n(),.groups='drop_last') %>% mutate(prop=n/sum(n)) %>% ungroup() %>%
#   filter(between(day,1,720))
# 
# gc = ggplot(to_plot,aes(x=day,y=cumsum(prop),fill=from))+geom_col(width=1)+scale_fill_manual(values=c(cbPalette,'black'))+
#   scale_x_continuous('Months since first posted',breaks=seq(0,720,90),labels=round(seq(0,720,90)*12/360))+theme_minimal()+
#   scale_y_continuous('Proportion of included studies',breaks=seq(0,1,0.1))+
#   theme(legend.title = element_blank(),legend.position = 'top',text=element_text(size=12),panel.grid.minor = element_blank())
# 
# png('manuscript/figures/FigureS5C.png',width=8,height=6,units='in',res=300)
# gc
# invisible(dev.off())


#sample size information
dat_sample_size = web_history %>% unnest(cols=c(field_label,field_value)) %>% filter(field_label %in% c('Enrollment','Date Posted')) %>% spread(field_label,field_value) %>% janitor::clean_names()
dat_sample_size = dat_sample_size %>% filter(grepl('Actual|Anticipated',enrollment)) %>%
  mutate_at('enrollment',~str_remove_all(.,'[\\[\\]]')) %>% separate(enrollment,c('n','stage'))

completed_sample_anticipated = filter(dat_sample_size,stage=='Anticipated') %>% group_by(nct) %>% slice_min(index) %>% ungroup() %>% select(nct,date_posted,n) %>% distinct()
completed_sample_actual = filter(dat_sample_size,stage=='Actual') %>% group_by(nct) %>% slice_max(index) %>% ungroup() %>% select(nct,date_posted,n) 

completed_sample_sizes = bind_rows(list(Anticipated = completed_sample_anticipated,Actual = completed_sample_actual),.id='stage') %>% 
  select(nct,stage,n) %>% spread(stage,n)

completed_sample_sizes %>% count(actual_ony = sum(!is.na(Actual) & is.na(Anticipated)),anticipated_avail=sum(!is.na(Anticipated) & is.na(Actual)),both_avail = sum(!is.na(Anticipated) & !is.na(Actual)))

#join with all id to get missing entries

dat_blandr = completed_sample_sizes %>%  mutate_at(c('Anticipated','Actual'),~as.numeric(.))

dat_blandr %>% summarise(median(Actual,na.rm=T),quantile(Actual,0.25,na.rm=T),quantile(Actual,0.75,na.rm=T))
dat_blandr %>% summarise(median(Anticipated,na.rm=T),quantile(Anticipated,0.25,na.rm=T),quantile(Anticipated,0.75,na.rm=T))


#exclusions
exclude_nct = dat_status_c %>% filter(overall_status=='Suspended/Terminated/Withdrawn') %>% distinct(nct) %>% pull()


#summary completeness

#10.1136/bmjopen-2021-053377
to_plot = filter(dat_blandr,!is.na(Anticipated),!is.na(Actual))

to_plot = to_plot %>% filter(!nct %in% exclude_nct)

g1 = ggplot(to_plot,aes(x=Actual/Anticipated))+geom_histogram(binwidth=0.5,colour='black',fill=cbPalette[1])+theme_bw()+
  geom_vline(aes(xintercept=1),linetype='dashed')+
  scale_x_continuous('Sample size ratio: Actual/Anticipated',breaks=seq(0,15,1))+
  scale_y_continuous('Count',breaks=seq(0,200,20))



to_plot = mutate(to_plot,
                 samplesize_target_log = log(Anticipated+0.1), # add small constant because of zeros
                 samplesize_actual_log = log(Actual+0.1),
                 diff_log = samplesize_actual_log - samplesize_target_log,
                 diff = Actual/Anticipated,
                 diff_perc = 100*(exp(diff)-1), # percent difference
                 aver = (samplesize_actual_log + samplesize_target_log)/2 )
# check
#filter(to_plot, diff< log(0.1)) %>% select(samplesize_target, samplesize_actual) # where sample was 10% of target
# stats
stats = summarise(to_plot, median=median(log(diff)), mean=mean(log(diff)), sd=sd(log(diff))) %>%
  mutate(z = qnorm(0.975),
         lower = exp(mean - z*sd), # limits of agreement
         upper = exp(mean + z*sd)) %>%
  ungroup() %>% mutate_at(c('mean','median'),~exp(.))


# plot
g.theme = theme_bw() + theme(panel.grid.minor = element_blank())
n_exclude = filter(to_plot,diff>10) %>% nrow()
g2 = ggplot(data=filter(to_plot,between(diff,0,10)), aes(x=aver, y=diff))+
  geom_point(size=1.5,alpha=0.7)+
  scale_x_continuous('Average sample size (log scale)', breaks=seq(0,15,2),sec.axis = sec_axis(~exp(.),breaks=c(10,100,500,1000,5000,10000,100000,500000),name='Average sample size (original scale)',labels=scales::comma))  +
  scale_y_continuous('Sample size ratio: Actual/Anticipated',breaks = seq(0,20,1))+
  geom_hline(data=stats, aes(yintercept=mean), linewidth=1, col=cbPalette[7])+
  geom_hline(data=stats, aes(yintercept=lower), linewidth=1,linetype='dashed', col=cbPalette[3])+
  geom_hline(data=stats, aes(yintercept=upper), linewidth=1,linetype='dashed', col=cbPalette[3])+g.theme


png('manuscript/figures/bland_altman_plot.png',width=800,height=400)
g2
invisible(dev.off())

png('manuscript/figures/Figure4.png',width=8,height=12,units='in',res=300)
ggarrange(ga,g2,nrow=2,labels=LETTERS[1:2])
invisible(dev.off())

#add tags
load('data/final_studies_with_meta_R1.rda')
included_studies = filter(decisions,final_decision=='include') %>% mutate(
  analysis = case_when(Development==TRUE & (is.na(Validation)|Validation==FALSE) ~ 'Development',
                       Development==TRUE & Validation==TRUE ~ 'Development + Validation',
                       (is.na(Development)|Development==FALSE) & Validation==TRUE ~ 'Validation',
                       (is.na(Development)|Development==FALSE) & (is.na(Validation)|Validation==FALSE) ~ 'Unclear'),
  outcome = case_when(Diagnostic==TRUE & (is.na(Prognostic)|Prognostic==FALSE) ~ 'Diagnostic',
                      Diagnostic==TRUE & Prognostic==TRUE ~ 'Diagnostic + Prognostic',
                      (is.na(Diagnostic)|Diagnostic==FALSE) & Prognostic==TRUE ~ 'Prognostic',
                      (is.na(Diagnostic)|Diagnostic==FALSE) & (is.na(Prognostic)|Prognostic==FALSE) ~ 'Unclear')) %>% select(NCT,analysis,outcome)
to_plot = left_join(to_plot,included_studies,by=c('nct'='NCT'))

to_plot_1 = filter(to_plot,between(diff,0,10),analysis!="Unclear") %>% mutate_at('analysis',~factor(.,levels=c('Development','Validation','Development + Validation')))
stats_1 = to_plot_1 %>% group_by(analysis) %>% summarise(mean=mean(log(diff)), sd=sd(log(diff))) %>%
  mutate(z = qnorm(0.975),
         lower = exp(mean - z*sd), # limits of agreement
         upper = exp(mean + z*sd)) %>%
  ungroup() %>% mutate_at('mean',~exp(.))


g3=ggplot(data=to_plot_1, aes(x=aver, y=diff))+
  geom_point(size=1.5,alpha=0.7)+facet_grid(~analysis)+
  scale_x_continuous('Average sample size (log scale)', breaks=seq(0,15,2),sec.axis = sec_axis(~exp(.),breaks=c(10,100,1000,10000,500000),name='Average sample size (original scale)',labels=scales::comma))  +
  scale_y_continuous('Sample size ratio: Actual/Anticipated',breaks = seq(0,20,1))+
  geom_hline(data=stats_1, aes(yintercept=mean), linewidth=1, col=cbPalette[7])+
  geom_hline(data=stats_1, aes(yintercept=lower), linewidth=1,linetype='dashed', col=cbPalette[3])+
  geom_hline(data=stats_1, aes(yintercept=upper), linewidth=1,linetype='dashed', col=cbPalette[3])+g.theme


to_plot_2 = filter(to_plot,between(diff,0,10),outcome!="Unclear") %>% mutate_at('outcome',~factor(.,levels=c('Diagnostic','Prognostic','Diagnostic + Prognostic')))
stats_2 = to_plot_2 %>% group_by(outcome) %>% summarise(mean=mean(log(diff)), sd=sd(log(diff))) %>%
  mutate(z = qnorm(0.975),
         lower = exp(mean - z*sd), # limits of agreement
         upper = exp(mean + z*sd)) %>%
  ungroup() %>% mutate_at('mean',~exp(.))

g4=ggplot(data=to_plot_2, aes(x=aver, y=diff))+
  geom_point(size=1.5,alpha=0.7)+facet_grid(~outcome)+
  scale_x_continuous('Average sample size (log scale)', breaks=seq(0,15,2),sec.axis = sec_axis(~exp(.),breaks=c(10,100,1000,10000,500000),name='Average sample size (original scale)',labels=scales::comma))  +
  scale_y_continuous('Sample size ratio: Actual/Anticipated',breaks = seq(0,20,1))+
  geom_hline(data=stats_2, aes(yintercept=mean), linewidth=1, col=cbPalette[7])+
  geom_hline(data=stats_2, aes(yintercept=lower), linewidth=1,linetype='dashed', col=cbPalette[3])+
  geom_hline(data=stats_2, aes(yintercept=upper), linewidth=1,linetype='dashed', col=cbPalette[3])+g.theme

png('manuscript/figures/Figure4_alt.png',width=800,height=800,units = 'px')
ggarrange(g3,g4,nrow=2,labels=LETTERS[1:2])
invisible(dev.off())


# 
# 
# #add date first posted, study start date; take first record;s et to first day of the month for consistency
# first_posted = filter(web_history,index==1) %>% unnest(cols=c(field_label,field_value)) %>% filter(field_label %in% c('First Posted','Study Start')) %>% spread(field_label,field_value) %>% select(NCT,`First Posted`,`Study Start`)  %>%
#   mutate_at('First Posted',~str_remove_all(.,'\\s+\\[.*$') %>% mdy(.,quiet = T)) %>%
#   mutate_at('Study Start',~ paste0(format(mdy(.),'%Y-%m'),'-01') %>% as.Date(.))
# 
# dat_status_c = dat_status_c %>% left_join(first_posted,by='NCT') %>% janitor::clean_names() %>% mutate_at('date_posted',~as.Date(.))

#for each NCT, take the next date and minus 1 day to get start,stop
dat_status_c = dat_status_c %>% arrange(nct,date_posted) %>% mutate(start = pmax(0,as.numeric(date_posted-dat_first_posted,'days'))) %>%
  group_by(nct) %>% mutate(stop=lead(start,1)-1,to = lead(overall_status,1)) %>% rename('from'=overall_status) %>%
  select(nct,from,to,start,stop) %>% mutate(total=n()) %>% 
  mutate_at('stop',~case_when(!is.na(.)~.,(total==1 & is.na(.))~start)) %>% ungroup()


dat_ipd = web_history %>% unnest(cols=c(field_label,field_value)) %>% filter(field_label %in% c('Date Posted','Plan to Share IPD')) %>% spread(field_label,field_value) %>%
  mutate_at('Plan to Share IPD',~replace_na(.,'Missing')) %>%
  mutate(ipd_c = case_when(grepl('^No',`Plan to Share IPD`) ~'No',
                              grepl('^Undecided',`Plan to Share IPD`) ~ 'Undecided',
                              grepl('^Yes',`Plan to Share IPD`) ~ 'Yes',
                           `Plan to Share IPD`=='Missing' ~ 'Missing'))

dat_ipd = dat_ipd %>% group_by(NCT,ipd_c) %>% slice_min(`Date Posted`) %>% ungroup() %>% select(NCT,index,`Date Posted`,ipd_c) %>% janitor::clean_names()



#first and final decisions
ipd_record = dat_ipd %>% group_by(nct) %>% summarise(first_report=min(date_posted),last_report=max(date_posted),index=max(index)) %>% ungroup()

#add first posted date
first_posted = web_history %>% unnest(cols=c(field_label,field_value)) %>% filter(field_label=='First Posted',index==1) %>%
  mutate_at('field_value',~str_remove_all(.,'\\s+\\[.*$') %>% mdy(.)) %>% select(NCT,field_value) %>% rename('first_posted'=field_value)

ipd_record = left_join(ipd_record,first_posted,by=c('nct'='NCT')) %>% 
  mutate_at(c('first_report','last_report'),~ymd(.)) %>%
  filter(first_report>as.Date('2016-01-01'),last_report<as.Date('2022-03-03'))

dat_ipd = dat_ipd %>% mutate_at('date_posted',~ymd(.))

ipd_record = ipd_record %>% left_join(dat_ipd,by=c('nct','index','first_report'='date_posted')) %>% rename('first_response'=ipd_c) %>%
  left_join(dat_ipd,by=c('nct','index','last_report'='date_posted')) %>% rename('last_response'=ipd_c) %>% distinct() 

distinct(ipd_record) %>% count(last_response)

web_history_ipd = readRDS("data/clintrials_history_ipd.rds")

ipd_sharing_info = web_history_ipd %>% unnest(cols=c(field_label,field_value)) %>% filter(field_label %in% c('Plan to Share IPD','Supporting Information','URL')) %>% spread(field_label,field_value) %>% janitor::clean_names()

ipd_sharing_info = left_join(ipd_record,ipd_sharing_info,by=c('nct','index')) %>% filter(last_response=='Yes') %>% mutate('ipd_sharing_details'=str_remove_all(plan_to_share_ipd,'^Yes\\.|^Yes')) %>% 
  select(nct,last_report,last_response,ipd_sharing_details) %>% 
  rename('plan_to_share_ipd'=last_response,'date_last_update'=last_report) %>%
  mutate_at('ipd_sharing_details',~ifelse(.=="",'No details provided',.))

openxlsx::write.xlsx(ipd_sharing_info,file='manuscript/supplement/SFile3.xlsx')






# #IPD introduced: December 2015
# #get full history for studies with IPD=Yes
# studies = ipd_record %>% filter(first_response=='Yes'|last_response=='Yes') %>% distinct(nct) %>% pull()
# 
# start = 1
# stop = length(studies)
# pb <- progress_bar$new(total=stop)
# web_history_ipd = NULL
# 
# for (k in start:stop){
#   # get the web page of the study's history
#   url_start = 'https://classic.clinicaltrials.gov/ct2/history/'
#   url = paste(url_start, studies[k], sep='')
#   site_search(url=url, destfile='web/history.html') # search with pauses if the site is tired of me
#   
#   # read the html page of the study changes
#   page <- read_html('web/history.html')
#   
#   # extract full history of study changes
#   table = tibble(
#     dates = str_remove_all(page %>% html_nodes("td:nth-child(4)") %>% html_text(), pattern='\\r|\\n'),
#     links = page %>% html_nodes("td:nth-child(4)")  %>% html_nodes("a") %>% html_attr("href")) %>%
#     mutate(dates = as.Date(dates, '%B %d, %Y')) %>% # convert date
#     arrange(dates) # order by date, just in case
#   
#   
#   #get first entry/earliest version on record
#   record_history = lapply(1:nrow(table),function(x) sample_historical(id = studies[k], intable = table, index=x))
#   web_history_ipd[[k]]<-bind_rows(record_history,.id='index') %>% add_column(NCT=studies[k])
#   to_remove = dir('web', pattern='.html')
#   file.remove(paste('web/', to_remove, sep=''))
#   pb$tick()
# }
# 
# web_history_ipd <- bind_rows(web_history_ipd) %>% distinct() %>% select(NCT,index,field_label,field_value) %>% group_by(NCT,index) %>% summarise(across(field_label:field_value,~list(.x)),.groups='drop')
# web_history_ipd <- web_history_ipd %>% mutate_at('index',~as.numeric(.)) %>% arrange(NCT,index)
# saveRDS(web_history_ipd, file="data/clintrials_history_ipd.rds")



