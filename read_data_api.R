devtools::install_github("serghiou/clinicaltrialr")
#code from github repo readme
library(clinicaltrialr)
library(pbapply)
library(tidyverse)
url_str<-"https://clinicaltrials.gov/ct2/results?titles=diagnostic+OR+prognostic+OR+prediction+model+OR+machine+learning+OR+artificial+intelligence+OR+algorithm+OR+score+OR+deep+learning+OR+regression"
results <- ct_read_results(url_str)



# Not run:


# library(rclinicaltrials) #//throws errors in clinicaltrials_search.Try running clinicaltrials_download on HPC
# library(tidyverse)
# 
# title_str <- "diagnostic OR prognostic OR prediction model OR machine learning OR artificial intelligence OR algorithm OR score OR deep learning OR regression"
# search_query <- paste0('titles = ',title_str)
# #total number of hist for given title_str
# n_titles<-clinicaltrials_count(search_query)
# 
# search_results <- clinicaltrials_download(query=c(search_query),count=1)
# 
# #review titles
# search_results$study_info %>% nrow()

# library(ctrdata) #//includes other trial registries. Need to read vignettes more closely as requires set up on local database
# title_str <- "titles=diagnostic+OR+prognostic+OR+prediction+model+OR+machine+learning+OR+artificial+intelligence+OR+algorithm+OR+score+OR+deep+learning+OR+regression"
# ctrOpenSearchPagesInBrowser(
#   url = paste(title_str),
#   register = "CTGOV"
# )
# q<-crtGetQueryUrl()
# q <- ctrGetQueryUrl('https://clinicaltrials.gov/ct2/results?titles=diagnostic+OR+prognostic+OR+prediction+model+OR+machine+learning+OR+artificial+intelligence+OR+algorithm+OR+score+OR+deep+learning+OR+regression')
# q
# ctrOpenSearchPagesInBrowser(
#   url = q
# )
# 
# db <- nodbi::src_sqlite(
#   dbname = "sqlite_file.sql",
#   collection = "test"
# )
# 
# ctrLoadQueryIntoDb(
#   queryterm = q,
#   only.count = TRUE,
#   con = db
# )$n

