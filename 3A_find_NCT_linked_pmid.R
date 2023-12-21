#3A_find_NCT_linked_pmid.R
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

dat = filter(scoredTrials,NCT.Link==1) %>% distinct()

#NCT quoted in pubmed
dat_nct = NULL
pmids = unique(dat[['PMID']])
Sys.sleep(1)
search_xml <- entrez_fetch(db="pubmed", id=pmids,rettype="xml",parsed=T,api_key =my_key)
pubmed_info = getNodeSet(search_xml,"//PubmedArticle")
pubs_nct = tibble(pmid = sapply(pubmed_info,xpathSApply,"./MedlineCitation/PMID",xmlValue) %>% as.numeric(),
                  title = sapply(pubmed_info,xpathSApply,".//Article//ArticleTitle",xmlValue),
                  abstract=sapply(pubmed_info,xpathSApply,".//Article//Abstract",unlist(xmlValue)) %>% sapply(function(x) str_c(x,collapse =' ')),
                  grant_info = collapse_xml_res(sapply(pubmed_info,xpathSApply,".//GrantList/Grant",function(x) unlist(lapply(xmlChildren(x),xmlValue)[c("GrantID","Agency","Country")]) %>% unique),sep_collapse = '|'),
                  pub_date = sapply(pubmed_info,xpathSApply,".//PubmedData//PubMedPubDate[@PubStatus='pubmed']",function(x) unlist(lapply(xmlChildren(x),xmlValue)[c("Year","Month","Day")])) %>% apply(.,2,function(x) str_c(stri_pad_left(x,2,0),collapse='-')),
                  pub_type_ui = collapse_xml_res(sapply(pubmed_info,xpathSApply,".//PublicationTypeList/PublicationType",xmlGetAttr,"UI")),
                  doi = sapply(pubmed_info,xpathSApply,"./PubmedData/ArticleIdList/ArticleId[@IdType='doi']",xmlValue))

pubs_nct = full_join(select(dat,NCT_num,PMID),pubs_nct ,by=c('PMID'='pmid')) %>% distinct()
pubs_nct = pubs_nct %>% add_column(match="",reject_reason="",.after='PMID')

write.xlsx(pubs_nct,file='data/publications/nct_linked_publications.xlsx')
