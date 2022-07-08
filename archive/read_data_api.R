devtools::install_github("serghiou/clinicaltrialr")
#code from github repo readme
library(clinicaltrialr)
library(pbapply)
library(tidyverse)
url_str<-"https://clinicaltrials.gov/ct2/results?titles=diagnostic+OR+prognostic+OR+prediction+model+OR+machine+learning+OR+artificial+intelligence+OR+algorithm+OR+score+OR+deep+learning+OR+regression"
results <- ct_read_results(url_str)



# Not run:


library(rclinicaltrials) #//throws errors in clinicaltrials_search.Try running clinicaltrials_download on HPC
library(tidyverse)

search_query_1 <- "('risk prediction' OR prediction score' OR 'predictive score' OR 'prediction model' OR 'predictive model' OR 'prediction algorithm' OR 'predictive algorithm' OR 'prediction tool' OR 'predictive tool' OR 'warning algorithm' OR 'warning score' OR 'prediction rule' OR 'predictive rule')"
search_query_2 <- "(development OR develop OR establishment OR establish)"
search_query_3 <- "(diagnosis OR diagnostic OR prognosis OR prognostic OR identification OR identify)"

search_query <- paste(search_query_1,search_query_2,search_query_3,sep=" AND ")


query_url <- "http://clinicaltrials.gov/ct2/results?"

#total number of hist for given title_str
n_titles<-clinicaltrials_count(search_query)

 
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

