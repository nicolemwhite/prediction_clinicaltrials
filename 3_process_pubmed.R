source("99_packages.R")
source("99_functions.R")
library('rentrez')
library('stringi')
my_key<-readLines("../prediction_pubmed/apikey_donotshare.txt")
collapse_xml_res = function(xml_res){
  if(is.null(nrow(xml_res))){out<-xml_res %>% sapply(function(x) str_c(x,collapse =';'))}
  else if (!is.null(nrow(xml_res))){out<-apply(xml_res,2,function(x) str_c(x,collapse =';'))}
  return(out)
} 

f_get_pubmed_tiab <- function(pmids){
  tiab = NULL
  pb <- progress_bar$new(total=length(pmids))
  for(f in pmids){
  url<-paste("http://eutils.ncbi.nlm.nih.gov/entrez/eutils/efetch.fcgi?db=pubmed&id=", f, "&retmode=XML", sep="")
  RETRY(verb = "GET", url = url, times = 5,quiet = FALSE, terminate_on = NULL)
  download_xml(url=url, file='web/pubmed.html')
  page <- read_html('web/pubmed.html') 
  
  ad = tibble(pmid = f,
                title = html_nodes(page,'articletitle') %>% html_text() %>% str_c(collapse=' '),
                abstract = html_nodes(page,'abstracttext') %>% html_text() %>% str_c(collapse = ' '))
  tiab = bind_rows(tiab,ad)
  
  pb$tick()
  }
  return(tiab)
}

f_get_pubmed_ids <- function(indata=scoredTrials,prob_cutoff=c(0,1),nct_cited=0,clintrials_cited =0){
  dat_sub = filter(indata,between(prob_adj,prob_cutoff[1],prob_cutoff[2]),NCT.Link==nct_cited,Trial.Results==clintrials_cited)
  pmids = dat_sub %>% distinct(NCT_num,PMID) %>% mutate_at('PMID',~as.character(.))
  return(pmids)
}

scoredTrials <- readRDS("data/scoredTrials.rds")

#NCT quoted in pubmed
pmids = f_get_pubmed_ids(nct_cited=1)
Sys.sleep(1)
search_xml <- entrez_fetch(db="pubmed", id=pmids$PMID,rettype="xml",parsed=T,api_key =my_key)
pubmed_info = getNodeSet(search_xml,"//PubmedArticle")
pubs_nct = tibble(pmid = sapply(pubmed_info,xpathSApply,"./MedlineCitation/PMID",xmlValue),
            title = sapply(pubmed_info,xpathSApply,".//Article//ArticleTitle",xmlValue),
            abstract=sapply(pubmed_info,xpathSApply,".//Article//Abstract",unlist(xmlValue)) %>% sapply(function(x) str_c(x,collapse =' ')),
            pub_date = sapply(pubmed_info,xpathSApply,".//PubmedData//PubMedPubDate[@PubStatus='pubmed']",function(x) unlist(lapply(xmlChildren(x),xmlValue)[c("Year","Month","Day")])) %>% apply(.,2,function(x) str_c(stri_pad_left(x,2,0),collapse='-')),
            pub_type_ui = collapse_xml_res(sapply(pubmed_info,xpathSApply,".//PublicationTypeList/PublicationType",xmlGetAttr,"UI")),
            mesh_terms = lapply(pubmed_info,xpathSApply,".//MedlineCitation/MeshHeadingList/MeshHeading/DescriptorName",xmlValue) %>% lapply(unlist))

pubs_nct = full_join(pmids,pubs_nct ,by=c('PMID'='pmid')) %>% distinct()

#Pubs listed in clintrials
pmids = f_get_pubmed_ids(nct_cited=0,clintrials_cited = 1)
start = 0; stop = length(pmids)
batch_size=250
pubs_clin = NULL
for(f in seq(start,stop,batch_size)){
  Sys.sleep(1)
  search_xml <- entrez_fetch(db="pubmed", id=pmids$PMID[(f+1):min(stop,f+batch_size)],rettype="xml",parsed=T,api_key =my_key)
  pubmed_info = getNodeSet(search_xml,"//PubmedArticle")
  ad = tibble(pmid = sapply(pubmed_info,xpathSApply,"./MedlineCitation/PMID",xmlValue),
              title = sapply(pubmed_info,xpathSApply,".//Article//ArticleTitle",xmlValue),
              abstract=sapply(pubmed_info,xpathSApply,".//Article//Abstract",unlist(xmlValue)) %>% sapply(function(x) str_c(x,collapse =' ')),
              pub_date = sapply(pubmed_info,xpathSApply,".//PubmedData//PubMedPubDate[@PubStatus='pubmed']",function(x) unlist(lapply(xmlChildren(x),xmlValue)[c("Year","Month","Day")])) %>% apply(.,2,function(x) str_c(stri_pad_left(x,2,0),collapse='-')),
              pub_type_ui = collapse_xml_res(sapply(pubmed_info,xpathSApply,".//PublicationTypeList/PublicationType",xmlGetAttr,"UI")),
              mesh_terms = lapply(pubmed_info,xpathSApply,".//MedlineCitation/MeshHeadingList/MeshHeading/DescriptorName",xmlValue) %>% lapply(unlist))
  pubs_clin = bind_rows(pubs_clin,ad) %>% distinct()
}

pubs_clin = full_join(pmids,pubs_clin ,by=c('PMID'='pmid')) %>% distinct()


pubmed_tiab = bind_rows(pubs_nct,pubs_clin) %>% distinct()
save(pubmed_tiab,file='data/pubmed_publication_data.rda')

#other pubs
res <- anti_join(scoredTrials %>% mutate_at('PMID',~as.character(.)),pubmed_tiab,by=c('PMID')) 

#prob_seq = c(seq(1,0.1,-0.1),0.05)



pubs_other = NULL

for (p in 1:10){
  Sys.sleep(1)
  pmids = f_get_pubmed_ids(indata=res,prob_cutoff = prob_seq[(p+1):p], nct_cited=0,clintrials_cited = 0)
  
  start = 0; stop = length(pmids$PMID)
  batch_size=250
  
  out = NULL
  for(f in seq(250,stop,batch_size)){
    Sys.sleep(1)
    search_xml <- entrez_fetch(db="pubmed", id=pmids$PMID[(f+1):min(stop,f+batch_size)],rettype="xml",parsed=T,api_key =my_key)
    pubmed_info = getNodeSet(search_xml,"//PubmedArticle")
    if(length(pubmed_info)>0){
    ad = tibble(pmid = sapply(pubmed_info,xpathSApply,"./MedlineCitation/PMID",xmlValue),
                title = sapply(pubmed_info,xpathSApply,".//Article//ArticleTitle",xmlValue),
                abstract=sapply(pubmed_info,xpathSApply,".//Article//Abstract",unlist(xmlValue)) %>% sapply(function(x) str_c(x,collapse =' ')),
                pub_date = sapply(pubmed_info,xpathSApply,".//PubmedData//PubMedPubDate[@PubStatus='entrez']",function(x) unlist(lapply(xmlChildren(x),xmlValue)[c("Year","Month","Day")])) %>% apply(.,2,function(x) str_c(stri_pad_left(x,2,0),collapse='-')),
                pub_type_ui = collapse_xml_res(sapply(pubmed_info,xpathSApply,".//PublicationTypeList/PublicationType",xmlGetAttr,"UI")),
                mesh_terms = lapply(pubmed_info,xpathSApply,".//MedlineCitation/MeshHeadingList/MeshHeading/DescriptorName",xmlValue) %>% lapply(unlist))
    out = bind_rows(out,ad) %>% distinct()
    }
  }
  if(!is.null(out)){
  out = inner_join(pmids,out ,by=c('PMID'='pmid')) %>% distinct()
  res <- anti_join(res,out,by=c('PMID'))
  }
  pubs_other[[p]]<-out
  
}

pubs_other = bind_rows(pubs_other)


pubmed_tiab = bind_rows(pubs_nct,pubs_clin,pubs_other) %>% distinct()

save(pubmed_tiab,file='data/pubmed_publication_data.rda')
