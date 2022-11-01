library(tidyverse)
library(openxlsx)

dat = read.csv('data/rayyan/articles.csv',header=T,sep=',')

#as_tibble
dat = as_tibble(dat)
#select
dat = dat %>% select(key,title,notes)

#extract text between {}
dat = dat %>% mutate(decision=str_extract_all(notes, "(?<=\\{).+?(?=\\})"))


#find number of include, exclude, maybe
dat = dat %>% mutate(n_include = str_count(as.character(decision),"Included"),
                     n_exclude = str_count(as.character(decision),"Excluded"),
                     n_maybe = str_count(as.character(decision),"Maybe"))

#add number of decisions; flag for second screen needed
dat = dat %>% mutate(n_decisions = n_include+n_exclude,to_screen = ifelse(n_decisions<=1,1,0))

#add final decision label
dat = dat %>% mutate(final_decision = case_when(
  n_include>=2 ~ 'Include',
  n_exclude>=2 ~ 'Exclude',
  n_include==1 & n_exclude==1 ~ 'Conflict', #hard coded
  n_include>=1 & n_exclude==0 & n_maybe>=1 ~ 'Include' # include studies where one reviewer has included and the other is unsure
)) #else NA


#before saving conflicts, remove definite includes, excludes, conflicts already discussed
decided = dat %>% filter(grepl('DECISION:',decision)|final_decision %in% c('Include','Exclude')) 

#extract final decision from the notes
decided = decided %>% mutate(final_decision_conflict = gsub('^.*DECISION:','',notes)) %>% 
  mutate_at('final_decision_conflict',~case_when(grepl('include',.,ignore.case = T)~'Include',grepl('exclude',.,ignore.case = T) ~ 'Exclude'))

#coalesce decision fields
decided = decided %>% mutate_at('final_decision',~coalesce(final_decision_conflict,.))


openxlsx::write.xlsx(list('final decisions'=decided),
                     file='data/rayyan/final_screening_decisions.xlsx')


remaining = anti_join(dat,decided) %>% mutate_at('final_decision',~replace_na(.,'Second screen needed')) %>% select(-decision,-to_screen)

# remaining %>% plyr::count("final_decision")

openxlsx::write.xlsx(list('all outstanding'=remaining,
                          'conflicts' = remaining %>% filter(final_decision=='Conflict')),
                     file='data/rayyan/outstanding_records.xlsx')

