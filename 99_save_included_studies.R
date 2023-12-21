#99_save_included_studies.R
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


#add additional fields from xml files (entire sample)
load('Z:/clinicaltrials/data/analysis_ready/all_studies.rda')
nct_included = included_studies %>% pull(NCT) 
dat_included = filter(dat, id %in% nct_included) %>% inner_join(included_studies,by=c('id'='NCT'))
dat_included = dat_included %>% mutate(year_posted = as.numeric(gsub(".*, ","",posted)))

save(dat_included,included_studies,excluded_studies,screening_results,file='data/final_studies.rda')

#write to excel file
included_keywords = included_studies %>% unnest(Keyword) %>% add_column(value=TRUE) %>% spread(Keyword,value,fill=FALSE)
excluded_keywords = excluded_studies %>% unnest(Keyword) %>% add_column(value=TRUE) %>% spread(Keyword,value,fill=FALSE)


write.xlsx(list('Include'=included_keywords),file='data/final_studies_keywords.xlsx')
