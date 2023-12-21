#4A_summarise_publication_numbers.R

source("99_packages.R")

#import review results with coding

nct_pmid_results = read.csv('data/publications/nct_pmid_linked_xml_20231127_coded.csv') #rerun and coded 2023-11-27
ml_pmid_results = read.csv('data/publications/ml_classifier_linked_20231204_coded.csv') #rechecked and coding done 4-12-2023

#total numbers before exclusions
nct_pmid_results %>% distinct(pmid) %>% nrow()
nct_pmid_results %>% distinct(NCT) %>% nrow()
nct_pmid_results %>% count(posted_before_pub)

#remove papers published before study was registered, and duplicate
nct_pmid_results %>% filter(posted_before_pub==1) %>% filter(!grepl('duplicate',note)) %>% nrow

nct_pmid_results_final = nct_pmid_results %>% filter(posted_before_pub==1) %>% filter(!grepl('duplicate',note)) %>% add_column(source='NCT linked or investigator submitted')
#tidy up
nct_pmid_results_final = nct_pmid_results_final %>% select(NCT:note,source) %>% rename('PMID'=pmid,'pub_date'=pub_date_entrez)

#ml classified articles
ml_pmid_results %>% distinct(PMID) %>% nrow()
ml_pmid_results %>% distinct(NCT) %>% nrow()
ml_pmid_results %>% count(posted_before_pub) #NA = excluded

ml_pmid_results_final = ml_pmid_results %>% filter(posted_before_pub==1) %>% filter(!grepl('duplicate',note)) %>% add_column(source='ML classifier')
ml_pmid_results_final %>% count(match)
#tidy up
ml_pmid_results_final = ml_pmid_results_final %>% select(NCT,study_start,study_posted,overall_status,PMID,match,title,pub_date,note,NCT_in_main_text,source)

combined_pmid_results = bind_rows(nct_pmid_results_final,ml_pmid_results_final) %>% distinct(NCT,PMID,.keep_all=TRUE)

#final check for duplicates
combined_pmid_results %>% select(NCT,PMID) %>% mutate(duplicate = duplicated(.)) %>% filter(duplicate==T)

save(combined_pmid_results,file='data/publication_outcomes_20231204.rda')
