#1_exclusion_reasons.R

source('99_packages.R')
source('99_functions.R')
load('data/final_studies_with_meta_R1.rda')

excluded_studies = filter(decisions,final_decision=='exclude')

rayyan_decisions = read.csv('data/rayyan/final_decisions_with_NCT.csv') %>% select(NCT,final_decision,notes)

excluded_studies = left_join(excluded_studies,rayyan_decisions,by='NCT',suffix=c('_manual','_rayyan'))

excluded_studies %>% mutate(tags = gsub('.*RAYYAN-LABELS:([^.\\|])','\\1',notes),
                            user_notes = gsub('.*USER-NOTES:([^.\\|])','\\1',notes)) %>% filter(final_decision_manual=='exclude') %>% View()

str_match(excluded_studies$notes,'USER-NOTES:[(.*?)|]')


included_studies = filter(decisions,final_decision=='include')
included_studies = full_join(included_studies,rayyan_decisions,by='NCT',suffix=c('_manual','_rayyan'))
