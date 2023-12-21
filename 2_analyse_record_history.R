#2_analyse_record_history.R
source('99_packages.R')
source('99_functions.R')

#need to update

#start with completed studies
dat_completed = readRDS("data/clintrials_history_completed_studies.rds")


#get sample sizes over time - [Actual] and/or [Anticipated]
dat_completed_enrol = filter(dat_completed,field_label=='Enrollment',grepl('Actual|Anticipated',field_value)) %>%
  mutate(field_type = str_remove_all(field_value,'[\\[\\]]')) %>% separate(field_type,c('n','stage'))

#add date posted
dates_history =filter(dat_completed,field_label=='Date Posted') %>% rename('date_updated'=field_value) %>% select(-field_label)

#join
dat_completed_enrol = dat_completed_enrol %>% inner_join(dates_history,by=c('id','index')) %>% mutate_at('index',~as.numeric(.)) %>% mutate_at('date_updated',~as.POSIXct(.,format="%Y-%m-%d"))

#for each id, take the first anticipated sample size and the final actual size
completed_sample_anticipated = filter(dat_completed_enrol,stage=='Anticipated') %>% group_by(id) %>% slice_min(date_updated) %>% ungroup() %>% select(id,date_updated,stage,n) %>% distinct()
completed_sample_actual = filter(dat_completed_enrol,stage=='Actual') %>% group_by(id) %>% slice_max(date_updated) %>% ungroup() %>% select(id,date_updated,stage,n) %>% distinct()

completed_sample_sizes = bind_rows(completed_sample_anticipated,completed_sample_actual)
#join with all id to get missing entries
no_info = setdiff(unique(dat_completed[['id']]),completed_sample_sizes[['id']])

dat_blandr = completed_sample_sizes %>% select(id,stage,n) %>% spread(stage,n) %>% select(id,Anticipated,Actual) %>%
  mutate_at(c('Anticipated','Actual'),~as.numeric(.))


#summary completeness
ftab_sample_size = dat_blandr %>% count(anticipated_known = !is.na(Anticipated),actual_known=!is.na(Actual))

#10.1136/bmjopen-2021-053377
to_plot = filter(dat_blandr,!is.na(Anticipated),!is.na(Actual))
g1 = ggplot(to_plot,aes(x=Actual/Anticipated))+geom_histogram(binwidth=0.5,colour='black',fill=cbPalette[1])+theme_bw()+
  geom_vline(aes(xintercept=1),linetype='dashed')+
  scale_x_continuous('Sample size ratio: Actual/Anticipated',breaks=seq(0,15,1))+
  scale_y_continuous('Count',breaks=seq(0,200,20))



to_plot = mutate(to_plot,
                 samplesize_target_log = log(Anticipated+0.1), # add small constant because of zeros
                 samplesize_actual_log = log(Actual+0.1),
                 diff = samplesize_actual_log - samplesize_target_log,
                 diff_perc = 100*(exp(diff)-1), # percent difference
                 aver = (samplesize_actual_log + samplesize_target_log)/2 )
# check
#filter(to_plot, diff< log(0.1)) %>% select(samplesize_target, samplesize_actual) # where sample was 10% of target
# stats
stats = summarise(to_plot, mean=mean(diff), sd=sd(diff)) %>%
  mutate(z = qnorm(0.975),
         lower = mean - z*sd, # limits of agreement
         upper = mean + z*sd) %>%
  ungroup()


# plot
g.theme = theme_bw() + theme(panel.grid.minor = element_blank())
g2 = ggplot(data=to_plot, aes(x=aver, y=diff))+
  geom_point(pch=1)+
  scale_x_continuous('Average sample size (log scale)', breaks=seq(0,15,2))+
  scale_y_continuous('Sample size ratio: Actual/Anticipated',breaks = seq(-4,4,1))+
  geom_hline(data=stats, aes(yintercept=mean), lty=2, col='red')+
  geom_hline(data=stats, aes(yintercept=lower), lty=2, col='green')+
  geom_hline(data=stats, aes(yintercept=upper), lty=2, col='green')+g.theme

#status over time
first_posted = dat_completed %>% filter(field_label=='First Posted') %>% select(id,field_value)
overall_status = dat_completed %>% filter(field_label=='Overall Status')  %>% rename('overall_status'=field_value) %>%
  inner_join(first_posted,by='id') %>% select(-field_label) %>%
  inner_join(dates_history,by=c('id','index')) %>% mutate_at('index',~as.numeric(.)) %>% mutate_at('date_updated',~as.POSIXct(.,format="%Y-%m-%d")) %>%
  mutate_at('field_value',~str_remove_all(.,pattern='\\[.*$') %>% str_trim %>% mdy(.)) %>% rename('date_posted'=field_value) %>%
  mutate(time_elapsed = pmax(0,as.numeric(as.Date(date_updated) - date_posted,'days')/365.25)) %>% distinct()

png('manuscript/figures/Figure4.png',width=800,height=400,units = 'px')
ggarrange(g1,g2,nrow=1,ncol=2,labels=LETTERS[1:2])
invisible(dev.off())
