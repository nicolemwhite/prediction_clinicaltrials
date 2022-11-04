#1_summarise_included_studies.R
source('99_packages.R')
source('99_functions.R')

#use final decision
screening_results = read.csv('data/rayyan/final_decisions_with_NCT.csv') %>% select(NCT,final_decision,notes)

#extract rayyan tags
#extract all text between RAYYAN-LABELS: and '|'
screening_results = screening_results %>% mutate(tags = gsub('.*RAYYAN-LABELS:([^.\\|])','\\1',notes)) 

all_between = c('RAYYAN-LABELS: ([^ ]+)')
screening_results = screening_results %>% 
  mutate(labels = str_extract(notes, pattern = all_between),
         labels = str_remove(labels, pattern = 'RAYYAN-LABELS: '),
         labels = tolower(labels),
         "Prognostic" = str_detect(labels, 'prognos'),
         "Diagnostic" = str_detect(labels,'diagnos'),
         #"Multiple designs" = str_detect(labels, 'multiple'), #remove; internal label - quote in text
         #"Clinical score" = str_detect(labels, 'clinical'), #removel internal label - quote in text
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

g1 = included_studies %>% distinct(NCT,.keep_all = T) %>% ggplot(aes(x=Keyword)) + geom_bar() + scale_x_upset() + scale_y_continuous('Number of included records\nfrom clinicaltrials.gov',breaks=seq(0,250,by=25)) + theme(panel.grid.minor = element_blank())

png('manuscript/figures/included_records_tags.png',width=8,height=6,units='in',res=300)
g1
invisible(dev.off())


#add additional fields from xml files (entire sample)
load('Z:/clinicaltrials/data/analysis_ready/all_studies.rda')
nct_included = included_studies %>% pull(NCT) 
dat_included = filter(dat, id %in% nct_included) %>% inner_join(included_studies,by=c('id'='NCT'))
dat_included = dat_included %>% mutate(year_posted = as.numeric(gsub(".*, ","",posted)))

save(dat_included,included_studies,excluded_studies,screening_results,file='data/final_studies.rda')

rm(dat);gc() #free up some memory - dat no longer needed


#plot screening record decisions over time. need to join with study year

g2 = ggplot(dat_included,aes(year_posted))+geom_histogram(aes(y=cumsum(..count..)),binwidth=1,fill='darkgrey',colour='black')+
  scale_x_continuous('Year first posted',breaks=seq(2000,2022,2))+
  scale_y_continuous('Cumulative number of included records',breaks=seq(0,1000,100))+
  theme_minimal()+theme(panel.grid.minor = element_blank(),axis.text = element_text(size=12),text=element_text(size=12))

png('manuscript/figures/incuded_records.png',width=1000,height=600)
g2
invisible(dev.off())


#split into prognostic vs diagnostic, development vs validation
dat_classification = filter(screening_results,final_decision=='Include') %>% mutate(grp = case_when(
  (Development==TRUE & Validation==FALSE) ~ 'Development',
  (Development==FALSE & Validation==TRUE) ~ 'Validation',
  (Development==TRUE & Validation==TRUE) ~ 'Development + Validation'),
  Outcome = case_when(
    (Prognostic==TRUE & Diagnostic==FALSE) ~ 'Prognostic',
    (Prognostic==FALSE & Diagnostic==TRUE) ~ 'Diagnostic',
    (Prognostic==TRUE & Diagnostic==TRUE) ~ 'Diagnostic + Prognostic')) %>% left_join(select(dat_included,id,year_posted),by=c('NCT'='id'))

all_years = tibble(Outcome = rep(c('Prognostic','Diagnostic','Diagnostic + Prognostic'),each=3),grp=rep(c('Development','Validation','Development + Validation'),3),year_posted=list(2000:2022)) %>% unnest(cols='year_posted')

ftab_outcome = dat_classification %>% count(Outcome,year_posted,grp)
ftab_outcome = right_join(ftab_outcome,all_years,by=c('Outcome','grp','year_posted')) %>% mutate_at('n',~replace_na(.,0)) %>%  mutate_at('grp',~factor(.,levels=c('Development','Validation','Development + Validation'))) %>% mutate_at('Outcome',~factor(.,levels=c('Diagnostic','Prognostic','Diagnostic + Prognostic'))) 
ftab_outcome = ftab_outcome %>% arrange(Outcome,grp,year_posted) %>% group_by(Outcome,grp) %>% mutate(n_cusum = cumsum(n)) %>% ungroup() %>% rename('Study type'=grp)

g3 = ftab_outcome %>% ggplot(aes(x=year_posted,y=n_cusum,colour=`Study type`,linetype=Outcome))+geom_point(size=2)+geom_line(size=0.75)+
  scale_x_continuous('Year first posted',breaks=seq(2000,2022,2))+scale_y_continuous('Cumulative number of included records',breaks=seq(0,650,50))+
  scale_colour_manual(values=cbPalette)+
  theme_minimal()+theme(strip.background = element_rect(fill='white'),
                        strip.text = element_text(size=12),axis.text.x = element_text(size=12),axis.text.y = element_text(size=12),
                        axis.text = element_text(size=12),legend.position = c(0.2,0.75),legend.title = element_blank(),legend.text = element_text(size=11))

png('manuscript/figures/included_records_outcome_studytype.png',width=8,height=6,units='in',res=300)
g3
invisible(dev.off())


#combined fig
png('manuscript/figures/Figure2.png',width=15,height=10,units='in',res=300)
ggarrange(g2,g3,g1,nrow=2,ncol=2,labels=LETTERS[1:3])
invisible(dev.off())


#keyword search by clinical problem - examples
dat_included %>% filter(grepl('COVID(.*)19|SARS(.*)CoV(.*)2',official_title,ignore.case = T)|grepl('COVID(.*)19|SARS(.*)CoV(.*)2',detailed_summary,ignore.case = T)) %>% nrow()
dat_included %>% filter(grepl('sepsis',official_title,ignore.case = T)) %>% nrow()
dat_included %>% filter(grepl('stroke',official_title,ignore.case = T)|grepl('stroke',official_title,ignore.case = T)) %>% nrow()
dat_included %>% filter(grepl('atrial fibrillation',official_title,ignore.case = T)|grepl('atrial fibrillation',detailed_summary,ignore.case = T)) %>% nrow()
dat_included = dat_included %>% mutate_at('mesh_terms',~str_split(.,pattern='\\|'))

mesh_terms = dat_included %>% select(id,mesh_terms) %>% unnest(cols='mesh_terms')

