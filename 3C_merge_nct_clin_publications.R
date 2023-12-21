#merge_nct_clin_publications.R
source("99_packages.R")
source("99_functions.R")


pubs_clin = read.xlsx('data/publications/clintrial_reported_publications.xlsx')
pubs_nct = read.xlsx('data/publications/done/nct_linked_publications_done.xlsx')

#RULES
#if match = 1, remove NCT and PMID from the data
#if match = 0, remove PMID but keep NCT in the data
#if match = 2, keep PMID and NCT in the data

nct_matches = select(pubs_nct,NCT_num,PMID,match)

dat_comb = left_join(pubs_clin,nct_matches,by=c('NCT_num','PMID'))