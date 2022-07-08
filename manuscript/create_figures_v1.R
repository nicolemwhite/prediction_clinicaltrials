dat = dat %>% mutate(year = gsub(".*, ","",posted))

dat %>% filter(grepl('Observational',study_type)) %>% count(year)