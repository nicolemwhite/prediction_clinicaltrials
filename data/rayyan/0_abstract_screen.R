library(tidyverse)
library(openxlsx)

dat = read.csv('rayyan_results_310720.csv',header=T,sep=',')

#as_tibble
dat = as_tibble(dat)
#select
dat = dat %>% select(key,title,notes)

#extract text between {}
dat = dat %>% mutate(decision=str_extract_all(notes, "(?<=\\{).+?(?=\\})"))


#split decision by comma
dat = separate(dat, decision, into = paste0("decision", 1:6), sep = ',')

# by reviewer (1-6 max)
res = list()
res[[1]] = separate(dat,decision1,into = c('reviewer','result'),sep = "=>") %>% select(key,reviewer,result)
res[[2]] = separate(dat,decision2,into = c('reviewer','result'),sep = "=>") %>% select(key,reviewer,result)
res[[3]] = separate(dat,decision3,into = c('reviewer','result'),sep = "=>") %>% select(key,reviewer,result)
res[[4]] = separate(dat,decision4,into = c('reviewer','result'),sep = "=>") %>% select(key,reviewer,result)
res[[5]] = separate(dat,decision5,into = c('reviewer','result'),sep = "=>") %>% select(key,reviewer,result)
res[[6]] = separate(dat,decision6,into = c('reviewer','result'),sep = "=>") %>% select(key,reviewer,result)

res_dat = do.call('rbind',res) %>% arrange(key)


#count results
key_n = res_dat %>% filter(!is.na(result)) %>% count(key) 

# check for outstanding reviews
key_todo = key_n %>% filter(n==1) %>% pull(key)

# tally number of decision per record
res_tally = res_dat %>% filter(!is.na(result)) %>% 
  group_by(key) %>% count(result) %>% ungroup()

ggplot(res_tally,aes(x=result,y=key,fill=factor(n)))+geom_tile()+theme(axis.text.y = element_blank())

#conflicts
conflicts = res_tally %>% count(key) %>% filter(n>1) %>% pull(key)

conflict_dat = res_tally %>% filter(key %in% conflicts) %>% spread(.,result,n,fill=0)
conflict_dat = dat %>% right_join(.,conflict_dat,by='key') %>% distinct(.,key,.keep_all = T)
conflict_dat = conflict_dat %>% rename(exclude = '"Excluded"',
                                       include = '"Included"',
                                       maybe = '"Maybe"')

write.csv(conflict_dat %>% select(-notes),file='rayyan_decisions_original.csv')

#agreements
agreements = res_tally %>% count(key) %>% filter(n==1) %>% pull(key)
agree_dat = res_tally %>% filter(key %in% agreements) %>% count(result)

#include
include_fulltext = res_tally %>% filter(key %in% agreements & result=='\"Included\"') %>% pull(key)
include_dat  = dat %>% filter(key %in% include_fulltext) %>% select(-notes)
write.csv(include_dat,file='abstract_included.csv',sep=',',col.names = T,row.names = F)
#res_tally_wide = res_tally %>% spread(.,result,n,fill=0)
