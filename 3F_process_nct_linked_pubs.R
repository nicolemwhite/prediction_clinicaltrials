#3F_process_nct_linked_pubs.R
source('99_packages.R')
source('99_functions.R')
load('data/final_studies_with_meta_R1.rda')
load('data/manual checks/nct_pubmed_tiab_20231127.rda')


#get date first posted as reference for publications; i.e, will remove publications that are likley references
#if no day is recorded, assume 1st day of the month
cal_months = as.character(month(1:12,label=T,abbr=F)) %>% str_c(collapse = '|')
cal_days = paste0('\\b',1:31,'\\b') %>% str_c(collapse = '|')

nct_info = decisions %>% select(NCT,posted,overall_status,final_decision) %>%   mutate(year = str_extract(string=posted,pattern='19\\d+|20\\d+'),
                                                             month = str_extract(string=posted,pattern=cal_months),
                                                             day = str_extract(string=posted,pattern=cal_days)) %>%
  mutate_at('day',~replace_na(.,'01')) %>% unite("study_posted",year:day) %>% mutate_at('study_posted',~ymd(.))

pmid_info = pubmed_tiab %>% select(NCT_num,pmid,title,pub_date_entrez) %>% mutate_at('pub_date_entrez',~ymd(.))

nct_pmid_info = inner_join(nct_info,pmid_info,by=c('NCT'='NCT_num'))
pmid_info = pubmed_tiab %>% select(NCT_num,pmid,title,pub_date_entrez) %>% mutate_at('pub_date_entrez',~ymd(.))
res = anti_join(nct_info,pmid_info,by=c('NCT'='NCT_num'))
nct_pmid_info = inner_join(nct_info,pmid_info,by=c('NCT'='NCT_num'))
#add study start info recorded in web histories
active <- readRDS("data/clintrials_history_active_studies.rds")
complete <- readRDS("data/clintrials_history_completed.rds")
unknown <- readRDS("data/clintrials_history_unknown_stopped.rds")

web_history = bind_rows(active,complete,unknown)
rm(active,complete,unknown)

study_start_dates = web_history %>% unnest(cols=c(field_label,field_value)) %>% filter(field_label %in% c('Study Start','First Submitted thatMet QC Criteria')) %>% spread(field_label,field_value) %>% 
  mutate_at('Study Start',~ifelse(. %in% c('Missing','January 2100'),NA,.)) %>% filter(!is.na(`Study Start`)) %>%
  group_by(NCT) %>% slice_min(index) %>% ungroup() %>% #take first known study start 
  mutate(year = str_extract(string=`Study Start`,pattern='19\\d+|20\\d+'),
         month = str_extract(string=`Study Start`,pattern=cal_months),
         day = str_extract(string=`Study Start`,pattern=cal_days)) %>%
  mutate_at('day',~replace_na(.,'01')) %>% unite("study_start",year:day) %>% mutate_at('study_start',~ymd(.))



nct_pmid_info = select(study_start_dates,NCT,study_start) %>% right_join(nct_pmid_info,by='NCT')
nct_pmid_info = nct_pmid_info %>% filter(!is.na(study_start),study_start<pub_date_entrez) %>% select(NCT,study_posted,study_start,overall_status,pmid,title,pub_date_entrez)

write.csv(nct_pmid_info,file='data/publications/nct_pmid_linked_xml_20231127.csv',row.names = F)

#process results of ML classifer here too to get study start and posted dates as exclusion criteria
#seach last done Nov 22
possible_pubs_ml = read.xlsx('data/publications/done/unlinked_publications_done.xlsx',detectDates = T)


#add study start and study_posted
nct_info_dates = select(study_start_dates,NCT,study_start) %>% left_join(select(nct_info,NCT,final_decision,study_posted,overall_status),by='NCT')

possible_pubs_ml = nct_info_dates %>% right_join(possible_pubs_ml,by=c('NCT'='NCT_num')) %>%
  mutate(posted_before_pub = ifelse(study_posted<pub_date,1,0))

#write to csv for final coding
write.csv(possible_pubs_ml,file='data/publications/ml_classifier_linked_20231204.csv',row.names = F)


filter(possible_pubs_ml,final_decision=='include') %>% count(posted_before_pub,match)
