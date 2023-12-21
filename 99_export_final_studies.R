#99_export_final_studies.R
## upadte: added search terms hits for easier filtering as part of manual tagging
source('99_packages.R')
source('99_functions.R')

#load full data to summarise search term hits
load('Z:/clinicaltrials/data/analysis_ready/all_studies.rda')

load('data/final_studies.rda')

screening_results = screening_results %>% mutate(tags = gsub('.*RAYYAN-LABELS:([^.\\|])','\\1',notes)) 
decisions = screening_results %>% select(NCT,final_decision)

decisions = dat %>% right_join(decisions,by=c('id'='NCT'))
rm(dat)

search_terms = c('machine learning','artificial intelligence','deep learning', 'prediction model', 'predictive model', 'prediction score', 'predictive score', 'warning score', 'risk score', 'risk prediction', 'prognostic model', 'diagnostic model')

#create additional columns to identify which search terms were found in each record

for(s in search_terms){
  decisions = decisions %>% rowwise() %>% mutate("{s}":=any(str_detect(c_across(official_title:keywords),s),na.rm=T)) %>% ungroup()
}

decisions = decisions %>% rename('NCT'=id) %>% left_join(select(screening_results,NCT,Development,Validation,Diagnostic,Prognostic),by='NCT')
  
decisions = select(decisions,NCT,final_decision,all_of(search_terms),Development:Prognostic) %>% add_column(commnent=NA)  
  
write.table(decisions,"data/verify_final_studies.csv",na='',row.names = F,sep=',')
