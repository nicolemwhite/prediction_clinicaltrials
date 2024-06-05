#R1_code_id_info.R

source('99_packages.R')
source('99_functions.R')

load('data/study_info_included_R1.rda')

#reported funding as grant or contract
study_history %>% unnest(cols=c('field_last','value_last')) %>% filter(field_last=='Secondary IDs',grepl('grant|contract',value_last,ignore.case=T)) 


#get words, ngrams
sponsor_words = filter(study_info) %>% group_by(lead_sponsor_agency_class) %>% unnest_tokens(words,lead_sponsor_agency,token='words') %>% count(lead_sponsor_agency_class,words) %>% ungroup()


sponsor_bigrams = filter(study_info) %>% group_by(lead_sponsor_agency_class) %>% unnest_tokens(bigram,lead_sponsor_agency,token='ngrams',n=2) %>% count(lead_sponsor_agency_class,bigram) %>% ungroup() %>% 
  separate(bigram,into=c('word1','word2'),sep=" ") %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word) %>%
  filter(!is.na(word1)) %>% 
  filter(!is.na(word2)) %>% 
  unite(bigram, word1, word2, sep=" ")
  

#code Other class based on lead sponsor
#university
#hospitals
study_info = study_info %>% mutate(university = if_else(grepl('universit',lead_sponsor_agency,ignore.case=T),1,0,missing=0),
                                   academy_institute = if_else(grepl('academ|institut',lead_sponsor_agency,ignore.case=T),1,0,missing=0),
                                   hospital = if_else(grepl('hospit',lead_sponsor_agency,ignore.case=T),1,0,missing=0),
                                   health_medical_center = if_else(grepl('\\bhealth(.*)cent(.*)\\b|\\bmedical(.*)cent(.*)\\b|clinic(.*)\\b',lead_sponsor_agency,ignore.case=T),1,0,missing=0),
                                   society_assn_foundation = ifelse(grepl('society|association|foundat',lead_sponsor_agency,ignore.case=T),1,0,missing=0))

#manual checks to refine classifications
study_info = study_info %>% mutate_at('university',~if_else(grepl('college(.*)london|KU Leuven',lead_sponsor_agency,ignore.case=T),1,.,missing=0)) %>%
  mutate_at('hospital',~if_else(grepl('\\bnhs\\b|\\bAssistance Publique\\b|\\bMemorial Centre\\b|\\bmedical school\\b|\\bmedical college\\b',lead_sponsor_agency,ignore.case=T),1,.,missing=0)) %>%
  mutate_at('academy_institute',~if_else(grepl('\\bcent(.*)disease\\b|\\bministry\\b|\\bnational cent(.*)\\b|\\bnational council\\b|research cent(.*)\\b',lead_sponsor_agency,ignore.case=T),1,.,missing=0)) %>%
  mutate_at('health_medical_center',~if_else(grepl('\\bcancer cent(.*)\\b|\\bheart cent(.*)',lead_sponsor_agency,ignore.case=T),1,.,missing=0))
study_info %>% count(lead_sponsor_agency_class)


