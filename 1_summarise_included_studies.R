#1_summarise_included_studies.R
source('99_packages.R')
source('99_functions.R')

#use final decision
screening_results = read.csv('data/rayyan/final_decisions_with_NCT.csv') %>% select(NCT,final_decision,notes)

#extract rayyan tags
#extract all text between RAYYAN-LABELS: and '|'
screening_results = screening_results %>% mutate(tags = gsub('.*RAYYAN-LABELS:([^.\\|])','\\1',notes)) 
excluded_studies = filter(screening_results,final_decision=='Exclude')

all_between = c('RAYYAN-LABELS: ([^ ]+)')
screening_results = screening_results %>% 
  mutate(labels = str_extract(notes, pattern = all_between),
         labels = str_remove(labels, pattern = 'RAYYAN-LABELS: '),
         labels = tolower(labels),
         "Prognostic" = str_detect(labels, 'prognos'),
         "Diagnostic" = str_detect(labels,'diagnos'),
         "Multiple designs" = str_detect(labels, 'multiple'),
         "Clinical score" = str_detect(labels, 'clinical'),
         "Development"  = str_detect(labels, 'development'),
         "Validation" = str_detect(labels,'validation'))

included_studies = filter(screening_results,final_decision=='Include') %>% select(NCT,Prognostic:Validation) %>%
  gather(Keyword,present,-NCT) %>%
  filter(present==TRUE) %>%
  group_by(NCT) %>% mutate_at('Keyword',~list(.)) %>% ungroup() %>% select(NCT,Keyword) %>% distinct()


excluded_studies = filter(screening_results,final_decision=='Exclude') %>% select(NCT,Prognostic:Validation) %>%
  gather(Keyword,present,-NCT) %>%
  filter(present==TRUE) %>%
  group_by(NCT) %>% mutate_at('Keyword',~list(.)) %>% ungroup() %>% select(NCT,Keyword)

png('manuscript/figures/included_records_tags.png',width=8,height=6,units='in',res=300)
included_studies %>% distinct(NCT,.keep_all = T) %>% ggplot(aes(x=Keyword)) + geom_bar() + scale_x_upset() + scale_y_continuous('Number of included records\nidentified on clinicaltrials.gov',breaks=seq(0,225,by=25)) + theme(panel.grid.minor = element_blank())
invisible(dev.off())


#add additional fields from xml files (entire sample)
load('Z:/clinicaltrials/data/analysis_ready/all_studies.rda')
nct_included = included_studies %>% pull(NCT) 
dat_included = filter(dat, id %in% nct_included) %>% inner_join(included_studies,by=c('id'='NCT'))

save(dat_included,included_studies,excluded_studies,screening_results,file='data/final_studies.rda')

rm(dat);gc() #free up some memory - dat no longer needed


#plot screening record decisions over time. need to join with study year
dat_included = dat_included %>% mutate(year_posted = as.numeric(gsub(".*, ","",posted)))
png('manuscript/figures/incuded_records.png',width=1000,height=600)
ggplot(dat_included,aes(year_posted))+geom_histogram(aes(y=cumsum(..count..)),binwidth=1,fill='darkgrey',colour='black')+
  scale_x_continuous('Year first posted',breaks=seq(2000,2022,2))+
  scale_y_continuous('Cumulative number of included records',breaks=seq(0,1000,100))+
  theme_minimal()+theme(panel.grid.minor = element_blank(),axis.text = element_text(size=12),text=element_text(size=12))
invisible(dev.off())


#split into prognostic vs diagnostic
dat_prognostic = filter(screening_results,final_decision=='Include',Prognostic==TRUE) %>% mutate(grp = case_when(
  (Development==TRUE & Validation==FALSE) ~ 'Development',
  (Development==FALSE & Validation==TRUE) ~ 'Validation',
  (Development==TRUE & Validation==TRUE) ~ 'Development + Validation')) %>% mutate_at('grp',~replace_na(.,'Missing')) %>% left_join(select(dat_included,id,year_posted),by=c('NCT'='id'))

dat_diagnostic = filter(screening_results,final_decision=='Include',Diagnostic==TRUE) %>% mutate(grp = case_when(
  (Development==TRUE & Validation==FALSE) ~ 'Development',
  (Development==FALSE & Validation==TRUE) ~ 'Validation',
  (Development==TRUE & Validation==TRUE) ~ 'Development + Validation')) %>% mutate_at('grp',~replace_na(.,'Missing')) %>% left_join(select(dat_included,id,year_posted),by=c('NCT'='id'))


ftab_outcome = bind_rows(list('Prognostic'=dat_prognostic,'Diagnostic'=dat_diagnostic),.id='Outcome') %>% 
  mutate_at('grp',~factor(.,levels=c('Development','Validation','Development + Validation','Missing'))) %>%
  count(Outcome,year_posted,grp)

#keyword search by clinical problem - examples

dat_included %>% filter(grepl('COVID(.*)19|SARS(.*)CoV(.*)2',official_title,ignore.case = T)|grepl('COVID(.*)19|SARS(.*)CoV(.*)2',detailed_summary,ignore.case = T)) %>% nrow()
dat_included %>% filter(grepl('sepsis',official_title,ignore.case = T)) %>% nrow()
dat_included %>% filter(grepl('stroke',official_title,ignore.case = T)|grepl('stroke',official_title,ignore.case = T)) %>% nrow()
dat_included %>% filter(grepl('atrial fibrillation',official_title,ignore.case = T)|grepl('atrial fibrillation',detailed_summary,ignore.case = T)) %>% nrow()



ftab_outcome %>% ggplot(aes(x=year_posted,y=n,group=grp,colour=grp))+geom_point()+geom_line()+facet_grid(Outcome~.)+
  scale_x_continuous('Year first posted',breaks=seq(2000,2022,2))+scale_y_continuous('Number of included records',breaks=seq(0,80,10))+
  theme_bw()+theme(strip.background = element_rect(fill='white'),
                   strip.text = element_text(size=12),axis.text.x = element_text(size=12),axis.text.y = element_text(size=12),
                   axis.text = element_text(size=12),legend.position = 'top',legend.direction = 'horizontal',legend.title = element_blank(),legend.text = element_text(size=11))

#missing tags
missing_p = filter(dat_prognostic,grp=='Missing') %>% pull(NCT)
missing_d = filter(dat_diagnostic,grp=='Missing') %>% pull(NCT)
missing_other = filter(screening_results,final_decision=='Include',Diagnostic==FALSE,Prognostic==FALSE) %>% pull(NCT)
missing_tags = c(missing_p,missing_d,missing_other) %>% unique()


dat_included = dat_included %>% mutate_at('mesh_terms',~list(str_split(.,pattern='\\|')))
