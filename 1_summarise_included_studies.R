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
  group_by(NCT) %>% mutate_at('Keyword',~list(.)) %>% ungroup() %>% select(NCT,Keyword)


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

save(dat_included,included_studies,excluded_studies,file='data/final_studies.rda')

rm(dat);gc() #free up some memory - dat no longer needed


#plot screening record decisions over time. need to join with study year
dat_included = dat_included %>% mutate(year_posted = as.numeric(gsub(".*, ","",posted)))

to_plot = dat_included %>% select(id,year_posted) %>% distinct()

ggplot(to_plot,aes(year_posted))+geom_histogram(aes(y=cumsum(..count..)),binwidth=1,fill='darkgrey',colour='black')+
  scale_x_continuous('Year first posted',breaks=seq(2000,2022))+
  scale_y_continuous('Cumulative number of eligible records',breaks=seq(0,1600,100))+
  theme_minimal()+theme(panel.grid.minor = element_blank(),axis.text = element_text(size=12),text=element_text(size=12))

