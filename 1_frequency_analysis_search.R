#1_frequency_analysis_search.R

source('99_packages.R')
source('99_functions.R')
#use final decision
screening_results = read.csv('data/rayyan/final_decisions_with_NCT.csv') %>% select(NCT,final_decision,notes)

#extract rayyan tags
#extract all text between RAYYAN-LABELS: and '|'
screening_results = screening_results %>% mutate(tags = gsub('.*RAYYAN-LABELS:([^.\\|])','\\1',notes)) 
decisions = screening_results %>% select(NCT,final_decision)


#load full data to summarise search term hits
load('Z:/clinicaltrials/data/analysis_ready/all_studies.rda')
decisions = left_join(decisions,dat,by=c('NCT'='id'))

rm(dat)
gc()

search_terms = c('machine learning','artificial intelligence','deep learning', 'prediction model', 'predictive model', 'prediction score', 'predictive score', 'warning score', 'risk score', 'risk prediction', 'prognostic model', 'diagnostic model')
  
#create additional columns to identify which search terms were found in each record
search_hits = decisions %>% mutate(across(official_title:keywords,~str_extract_all(string=tolower(.x),pattern = paste0('\\b',str_c(search_terms,collapse='\\b|\\b'),'\\b')) %>% 
                                            sapply(function(x) str_c(unique(x),collapse =';'))))
search_hits_l = search_hits %>% gather(variable,value,-NCT,-final_decision) %>% filter(!is.na(value),value!='') %>% rowwise() %>% mutate_at('value',~(str_split(.,';')))
ftab_hits = search_hits_l %>% unnest(value) %>% count(final_decision,variable,value)
ggplot(ftab_hits,aes(x=variable,y=n,fill=value,group=final_decision))+geom_col()+facet_grid(~final_decision)+coord_flip()

search_hits_l %>% unnest(value) %>% count(final_decision,value) %>% spread(final_decision,n,fill=0) %>% arrange(-Include)


#next step
##frequency analysis of co-occuring terms
##just settle on whats already been done
#check terms from covid19 review, oncology review