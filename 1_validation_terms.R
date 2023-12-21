#2_validation_terms.R
source('99_packages.R')
source('99_functions.R')
load('data/final_studies_with_meta_R1.rda') 
dat_dup = openxlsx::read.xlsx('data/manual checks/possible duplicates-20230713.xlsx')
exclude_dup = dat_dup %>% filter(duplicate=='y') %>% pull(b) %>% unique()

#filter to all studies that have been marked as validation
validation_studies = filter(decisions,final_decision=='include' & !NCT %in% exclude_dup,Validation==T)

#find instances of 'validat*' in titles and descripttions
validation_text = '\\bvalidation\\b|\\bdevelopment and validation\\b|\\bestablishment and validation\\b'
validation_type_text = 'external validat|externally validat|validated externally|internal validat|internally validat|validated internally'
#find instances of 'validat*' in titles and summaries
## for all instances try and determine the type of validation based on text

external_validation_text = 'external validat|externally validat|validated externally'
internal_validation_text = 'internal validat|internally validat|validated internally'
cross_validation_text = 'cross(\\-)?validat'
split_validation_text = '\\btrain*\\btest|\\btrain.*\\bvalidat'
validation_studies = validation_studies %>% mutate(
  in_title = ifelse(grepl(validation_text,official_title,ignore.case = T)|grepl(validation_text,brief_title,ignore.case = T),TRUE,FALSE),
  type_in_title = ifelse(grepl(validation_type_text,official_title,ignore.case = T)|grepl(validation_type_text,brief_title,ignore.case = T),TRUE,FALSE),
    external_validation = case_when(
    #external
    grepl(external_validation_text,official_title) ~ 'y',
    grepl(external_validation_text, brief_title) ~ 'y',
    grepl(external_validation_text,detailed_summary) ~ 'y',
    grepl(external_validation_text, brief_summary) ~ 'y',
    TRUE ~ 'n'),
  #internal
  internal_validation = case_when(grepl(internal_validation_text,official_title) ~ 'y',
  grepl(internal_validation_text, brief_title) ~ 'y',
  grepl(internal_validation_text,detailed_summary) ~ 'y',
  grepl(internal_validation_text, brief_summary) ~ 'y',
  TRUE ~ 'n'),
  #cross
  cross_validation = case_when(grepl(cross_validation_text,official_title) ~ 'y',
  grepl(cross_validation_text, brief_title) ~ 'y',
  grepl(cross_validation_text,detailed_summary) ~ 'y',
  grepl(cross_validation_text, brief_summary) ~ 'y',
  TRUE ~ 'n'),

  validation_type = ifelse(if_any(.cols=ends_with('_validation'), .fns= ~ . =='y'), 'y', 'n')
)


#summarise

validation_hits = list()

validation_hits[['external']] = validation_studies %>% group_by(NCT) %>% 
  summarise(across(official_title:keywords,~grepl(external_validation_text,tolower(.x)) %>% 
                     sapply(function(x) str_c(unique(x),collapse =';'))),.groups='drop') %>% pivot_longer(cols=-NCT)


validation_hits[['internal']] = validation_studies %>% group_by(NCT) %>% 
  summarise(across(official_title:keywords,~grepl(internal_validation_text,tolower(.x)) %>% 
                     sapply(function(x) str_c(unique(x),collapse =';'))),.groups='drop') %>% pivot_longer(cols=-NCT) 

validation_hits[['cross']] = validation_studies %>% group_by(NCT) %>% 
  summarise(across(official_title:keywords,~grepl(cross_validation_text,tolower(.x)) %>% 
                     sapply(function(x) str_c(unique(x),collapse =';'))),.groups='drop') %>% pivot_longer(cols=-NCT)

validation_hits[['splitsample']] = validation_studies %>% group_by(NCT) %>% 
  summarise(across(official_title:keywords,~grepl(split_validation_text,tolower(.x)) %>% 
                     sapply(function(x) str_c(unique(x),collapse =';'))),.groups='drop') %>% pivot_longer(cols=-NCT)

bind_rows(validation_hits) %>% distinct() %>% 
  filter(grepl('_title|_summary|_outcome_',name)) %>%
  ggplot(aes(NCT,name,fill=value))+geom_tile()


