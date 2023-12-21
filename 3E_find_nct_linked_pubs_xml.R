#3E_find_nct_linked_pubs_xml.R
source('99_packages.R')
source('99_functions.R')

get_pmid_links<-function(ct_url){
  full_ct_url <- paste0("https://classic.clinicaltrials.gov",ct_url) 
  pmids<-str_squish(grep(curlGetHeaders(full_ct_url), pattern = "Location", value = T) %>% str_remove_all(.,"Location:|\r\n")) %>% str_remove_all(pattern='\\?dopt=Abstract')
  return(pmids)
}

get_nct_linked_pubs <- function(nct_num){
  
  #get automated urls for all listed NCT-linked pubs
  address = paste0("https://classic.clinicaltrials.gov/ct2/show/study/",nct_num)
  download_xml(url=address, file='web/page.html')
  in_page_ct <- read_html('web/page.html') 
  pub_links = in_page_ct %>% html_nodes(xpath='body//a') %>% html_attrs() %>% bind_rows() %>% filter(grepl('^/ct2/bye',href)) %>% select(title,href)
  total_links <- nrow(pub_links)
  
  #for each url in pub_links, get the PubMed info
  dat_pmid = data.frame(NCT_num=nct_num)
  if(total_links>0){
    pmids = lapply(1:total_links,function(x) get_pmid_links(pub_links[[x,'href']]))
    if(!is_empty(pmids)){dat_pmid = data.frame(do.call(rbind,pmids)) %>% add_column(NCT_num=nct_num,.before=1)}
  }
  return(dat_pmid)
}

load('data/final_studies_with_meta_R1.rda')
decisions = filter(decisions) # %>% filter(final_decision=='include') # should this be exclude too? can then work out sens/spec of manual search

f <- "data/publications/nct_linked_from_xml.rda"
if (file.exists(f)){
  load(f)
  already_done<-nct_linked_candidate_pmid$NCT_num
  to_do <- setdiff(decisions$NCT,unique(already_done))
}
if(!file.exists(f)){to_do <- decisions$NCT;already_done<-NULL;nct_linked_candidate_pmid<-NULL;K<-length(to_do)}

if(!is_empty(to_do)){
  choose.nct<-sample(to_do,K)
  pb <- progress_bar$new(total=K)
  out=NULL
  for(x in seq_along(choose.nct)){out[[x]]<-get_nct_linked_pubs(choose.nct[x]); pb$tick()}
  
  ad = bind_rows(out) %>% rename('nlm'=X1,'pubmed'=X2)
  nct_linked_candidate_pmid = bind_rows(nct_linked_candidate_pmid,ad)
  
  save(nct_linked_candidate_pmid,file='data/publications/nct_linked_from_xml.rda')
}

#once done, save records with 1+ PMID as a .csv
if(is_empty(to_do)){
  nct_linked_forreview = filter(nct_linked_candidate_pmid,!is.na(pubmed)) %>% select(NCT_num,pubmed) %>% mutate(PMID = str_match(pubmed,pattern='[0-9]+'))
  write.csv(nct_linked_forreview,file='data/manual checks/nct linked xml 20231117.csv')
}


#search pubmed API for mentions of NCT number
library(rentrez)
library(stringi)

my_key<-readLines("../prediction_pubmed/apikey_donotshare.txt")


NCT_search = decisions$NCT
search_str = function(x){paste0(x,'[SI] OR ',x,'[TIAB]')}
pubmed_hits <- lapply(NCT_search,function(y) entrez_search(db="pubmed", term=search_str(y), use_history = F,api_key =my_key)$ids)
names(pubmed_hits)<-NCT_search

pubmed_hits_df = lapply(pubmed_hits,function(x) data.frame(x)) %>% bind_rows(.id='NCT_num')
pubmed_hits_df = pubmed_hits_df %>% mutate(pubmed=paste0('https://pubmed.ncbi.nlm.nih.gov/',x)) %>% rename('pmid'=x) %>% select(NCT_num,pubmed,pmid)
write.csv(pubmed_hits_df,file='data/manual checks/nct linked entrez 20231127.csv')


#####
#test MEDLINE search string
nct_linked = read.csv('data/manual checks/nct linked xml 20231117.csv')
pubmed_hits = read.csv('data/manual checks/nct linked entrez 20231127.csv')
all_pmids = union(nct_linked$PMID,pubmed_hits$pmid)

pmid_combined_hits = full_join(nct_linked,pubmed_hits,by=c('NCT_num','PMID'='pmid','pubmed'))

NCT_matched = unique(pmid_combined_hits$NCT_num)
pubmed_tiab = NULL

for (y in seq_along(NCT_matched)){
  to_do = filter(pmid_combined_hits,NCT_num==NCT_matched[y]) %>% distinct(PMID) %>% pull
  Sys.sleep(0.2)
  search_xml <- entrez_fetch(db="pubmed", id=to_do,rettype="xml",parsed = T,api_key =my_key)
  medline_citation_info = getNodeSet(search_xml,"//PubmedArticle") 
  
  if(length(medline_citation_info)>0){
  ad_tiab = tibble(pmid = sapply(medline_citation_info,xpathSApply,"./MedlineCitation/PMID",xmlValue),
                   title = sapply(medline_citation_info,xpathSApply,".//ArticleTitle",xmlValue),
                   abstract = lapply(medline_citation_info,xpathSApply,".//Abstract",xmlValue) %>% lapply(unlist),
                   pub_date_entrez = sapply(medline_citation_info,xpathSApply,".//PubmedData//PubMedPubDate[@PubStatus='entrez']",
                                            function(x) unlist(lapply(xmlChildren(x),xmlValue)[c("Year","Month","Day")])) %>% apply(.,2,function(x) str_c(stri_pad_left(x,2,0),collapse='-')) 
                   
                   ) %>% add_column(NCT_num=NCT_matched[y])
  pubmed_tiab = bind_rows(pubmed_tiab,ad_tiab)
  }
}

save(pubmed_tiab,file='data/manual checks/nct_pubmed_tiab_20231127.rda')


