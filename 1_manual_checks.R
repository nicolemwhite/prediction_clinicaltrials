#1_manual_checks.R
source('99_packages.R')
source('99_functions.R')

included_records <- read.xlsx('data/included_studies_check_Nicole.xlsx')
included_records = included_records %>% mutate_at('done',~ifelse(.=='y','include',.))
ad <- read.csv('data/verify_final_studies.csv') %>% select(NCT,final_decision,Development:comment)


ad = anti_join(ad,included_records,by='NCT') %>% mutate(done = case_when(final_decision=='Exclude' & comment=='ok' ~ 'exclude',
                                                                    final_decision=='Include' & comment=='exclude' ~ 'exclude',
                                                                    final_decision=='Include' & comment=='ok' ~ 'include')) %>%
  select(NCT,Development:Prognostic,done)

all_records = bind_rows(included_records,ad)
save(all_records,file = 'data/final_studies_R1.rda')

to_check = filter(included_records,done=='include') %>% mutate(
  analysis = case_when(Development==TRUE & is.na(Validation) ~ 'development',
                       Development==TRUE & Validation==TRUE ~ 'development + validation',
                       is.na(Development) & Validation==TRUE ~ 'validation',
                       is.na(Development) & is.na(Validation) ~ 'unclear'),
  outcome = case_when(Diagnostic==TRUE & is.na(Prognostic) ~ 'diagnostic',
                      Diagnostic==TRUE & Prognostic==TRUE ~ 'diagnostic + prognostic',
                      is.na(Diagnostic) & Prognostic==TRUE ~ 'prognostic',
                      is.na(Diagnostic) & is.na(Prognostic) ~ 'unclear'))


#check random 20% samples for validation
p = 0.2
n = 4


set.seed(38152)
checks_per_n = ceiling(p*nrow(to_check)/n)
random_checks = to_check %>% sample_n(n*checks_per_n) %>% select(NCT,analysis,outcome) %>% add_column('agree y/n'='','reason for disagreement if applicable'='') %>%
  mutate('who'=as.numeric(cut_number(row_number(),n=4)) %>% factor(levels=1:4,labels=c('AB','DB','RP','NW'))) 

write.xlsx(filter(random_checks,who=='AB') %>% select(-who),file='data/manual checks/included/Adrian to check.xlsx')
write.xlsx(filter(random_checks,who=='DB') %>% select(-who),file='data/manual checks/included/Dave to check.xlsx')
write.xlsx(filter(random_checks,who=='RP') %>% select(-who),file='data/manual checks/included/Rex to check.xlsx')
write.xlsx(filter(random_checks,who=='NW') %>% select(-who),file='data/manual checks/included/Nicole to check.xlsx')

#once all resolved, save as final_studies_R2.R



