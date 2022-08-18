#1_updated_decisions_abstract.R
library(tidyverse)
library(openxlsx)

abstracts_included = read.csv(file='abstract_included.csv',header = T,sep=',')
consensus_decisions = read.csv(file='rayyan_decisions.csv',header = T,sep=',')

#filter
add_abstracts = consensus_decisions %>% filter(final=='include') %>% select(key:decision6) %>% mutate_if(is.factor,as.character)

#as_tibble
abstracts_included = as_tibble(abstracts_included) %>% mutate_if(is.factor,as.character)


#intersect
colnames(abstracts_included)
colnames(consensus_decisions)
#inner_join
abstracts_included = bind_rows(abstracts_included,add_abstracts)
write_csv(abstracts_included,path='abstract_included_updated.csv')


conflicts = consensus_decisions %>% filter(final=='conflict')

#ties excluded versus included/maybe
conflicts = conflicts %>% mutate(action = case_when(exclude==(include+maybe) ~ 'tied: discuss',
                                                    exclude>(include+maybe) ~ 'ok to exclude?',
                                                    exclude<(include+maybe) ~ 'ok to carry over?'))
                                   
write_csv(conflicts,path = 'outstanding_abstract_conflicts.csv')
#excludes>include_maybe