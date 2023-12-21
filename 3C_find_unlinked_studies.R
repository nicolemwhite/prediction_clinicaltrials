#3C_find_unlinked_studies.R
source("99_packages.R")
source("99_functions.R")
library('rentrez')
library('stringi')

collapse_xml_res = function(xml_res,sep_collapse=';'){
  if(is.null(nrow(xml_res))){out<-xml_res %>% sapply(function(x) str_c(x,collapse =sep_collapse))}
  else if (!is.null(nrow(xml_res))){out<-apply(xml_res,2,function(x) str_c(x,collapse =sep_collapse))}
  return(out)
} 

my_key<-readLines("../prediction_pubmed/apikey_donotshare.txt")
scoredTrials <- readRDS("data/scoredTrials.rds")

#read in screening results for linked studies
pubs_nct = read.xlsx('data/publications/done/nct_linked_publications_done.xlsx') %>% mutate_at('pub_date',~ymd(.))
pubs_ct = read.xlsx('data/publications/done/clintrial_reported_publications_done.xlsx') %>% mutate_at('pub_date',~ymd(.))


already_matched = bind_rows(pubs_nct,pubs_ct) %>% filter(match==1) 

#remove linked PMID, NCT
res = anti_join(filter(scoredTrials,NCT.Link==0,Trial.Results==0),already_matched,by='PMID') %>% anti_join(already_matched,by='NCT_num') %>% mutate_at('PMID',~as.character(.))


#take top scoring pmid per
load('../prediction_pubmed/data/records_2000_to_2022_v2.rda')

res = semi_join(res,dat,by=c('PMID'='pmid')) #matched to pubmed searth string

res = res %>% group_by(NCT_num) %>% slice_max(prob_adj) %>% ungroup() %>% arrange(-prob_adj)

#NCT quoted in pubmed
pubs_res = NULL
pmids = unique(res[['PMID']])
start=0
stop=length(pmids)
batch_size=200

for(f in seq(start,stop,batch_size)){
  Sys.sleep(1)
  search_xml <- entrez_fetch(db="pubmed", id=pmids[(f+1):min(stop,f+batch_size)],rettype="xml",parsed=T,api_key =my_key)
  pubmed_info = getNodeSet(search_xml,"//PubmedArticle")
  ad = tibble(pmid = sapply(pubmed_info,xpathSApply,"./MedlineCitation/PMID",xmlValue) %>% as.numeric(),
              title = sapply(pubmed_info,xpathSApply,".//Article//ArticleTitle",xmlValue),
              abstract=sapply(pubmed_info,xpathSApply,".//Article//Abstract",unlist(xmlValue)) %>% sapply(function(x) str_c(x,collapse =' ')),
              grant_info = collapse_xml_res(sapply(pubmed_info,xpathSApply,".//GrantList/Grant",function(x) unlist(lapply(xmlChildren(x),xmlValue)[c("GrantID","Agency","Country")]) %>% unique),sep_collapse = '|'),
              pub_date = sapply(pubmed_info,xpathSApply,".//PubmedData//PubMedPubDate[@PubStatus='pubmed']",function(x) unlist(lapply(xmlChildren(x),xmlValue)[c("Year","Month","Day")])) %>% apply(.,2,function(x) str_c(stri_pad_left(x,2,0),collapse='-')),
              pub_type_ui = collapse_xml_res(sapply(pubmed_info,xpathSApply,".//PublicationTypeList/PublicationType",xmlGetAttr,"UI")),
              doi = sapply(pubmed_info,xpathSApply,"./PubmedData/ArticleIdList/ArticleId[@IdType='doi']",xmlValue),
              registration = sapply(pubmed_info,xpathSApply,".//AccessionNumberList/AccessionNumber",unlist(xmlValue)) %>% sapply(function(x) str_c(x,collapse =' ')))
  pubs_res = bind_rows(pubs_res,ad) %>% distinct()
}


pubs_res = pubs_res %>% mutate_at('pmid',~as.character(.))
pubs_res = full_join(select(res,NCT_num,PMID),pubs_res ,by=c('PMID'='pmid')) %>% distinct()
pubs_res = pubs_res %>% add_column(match="",reject_reason="",.after='PMID')

load('data/clin_study_dates.rda')
ct_start_dates = filter(study_start_dates,NCT %in% pubs_res[['NCT_num']]) %>% select(NCT,study_start)
pubs_res = pubs_res %>% left_join(ct_start_dates,by=c('NCT_num'='NCT')) %>% mutate_at('pub_date',~ymd(.)) %>% mutate(pub_after_study_start=as.numeric(study_start<pub_date)) %>% select(-study_start)

write.xlsx(pubs_res,file='data/publications/unlinked_publications.xlsx')


#checks to add
pubs_res %>% filter(NCT_num==registration)
pubs_res %>% mutate(nct_in_abstract = str_extract_all(abstract,pattern='\\bNCT\\d+\\b',simplify=T)[,1]) %>% filter(registration!=nct_in_abstract,registration!="",nct_in_abstract!="")

