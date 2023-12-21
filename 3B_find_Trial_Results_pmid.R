#3B_find_Trial_Results_pmid.R
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
dat = filter(scoredTrials,NCT.Link==0,Trial.Results==1) %>% distinct()


#NCT quoted in pubmed
pubs_clin = NULL
pmids = unique(dat[['PMID']])
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
                  doi = sapply(pubmed_info,xpathSApply,"./PubmedData/ArticleIdList/ArticleId[@IdType='doi']",xmlValue))
pubs_clin = bind_rows(pubs_clin,ad) %>% distinct()
}


pubs_clin = full_join(select(dat,NCT_num,PMID),pubs_clin ,by=c('PMID'='pmid')) %>% distinct()
pubs_clin = pubs_clin %>% add_column(match="",reject_reason="",.after='PMID')

#remove all PMID published before study start date
load('data/clin_study_dates.rda')
ct_start_dates = filter(study_start_dates,NCT %in% pubs_clin[['NCT_num']]) %>% select(NCT,study_start)
pubs_clin = pubs_clin %>% left_join(clin_start_dates,by=c('NCT_num'='NCT')) %>% mutate_at('pub_date',~ymd(.)) %>% filter(study_start<pub_date) %>% select(-study_start)

write.xlsx(pubs_clin,file='data/publications/clintrial_reported_publications.xlsx')
