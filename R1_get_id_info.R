#R1_get_id_info.R
## get funding ids for all included NCT

source('99_packages.R')
source('99_functions.R')

nct_included = read.csv('data/final_included_studies.csv') %>% pull(NCT)

# review files in zipped folder
all_results = unzip('Z:/clinicaltrials/AllPublicXML.zip',list=TRUE) %>% data.frame() %>%
  filter(Name!='Contents.txt')

home = getwd()
#get full list of NCT folders
nct_folders <- list.files('Z:/clinicaltrials/data'); nct_folders<-nct_folders[grepl('^NCT',nct_folders)]
xml_files <- sapply(nct_included,function(s) paste0('Z:/clinicaltrials/data/',substring(s,1,7),'xxxx','/',s,'.xml'))


N = length(xml_files)
study_info = NULL
for (f in 1:N){
go = xml_files[f]
cat('Processing record ID ', gsub('.xml','',xml_files[f]), '\n',sep='') # progress

# get the XML data into R
data <- xmlParse(go)
xml_data <- xmlToList(data) # only gets top level children, so vulnerable to missing information where there are multiple lists

org_study_id = null_na(sapply(getNodeSet(data,"//org_study_id"),xpathSApply,".",xmlValue))
lead_sponsor_agency = sapply(getNodeSet(data,"//agency"),xpathSApply,".",xmlValue)
lead_sponsor_agency_class =  sapply(getNodeSet(data,"//agency_class"),xpathSApply,".",xmlValue)

frame = data.frame(id=xml_data$id_info$nct_id,
                   org_study_id=null_na(xml_data$id_info$org_study_id),
                   secondary_id = null_na(xml_data$id_info$secondary_id),
                   lead_sponsor_agency = xml_data$sponsors$lead_sponsor$agency,
                   lead_sponsor_agency_class = xml_data$sponsors$lead_sponsor$agency_class)

study_info = bind_rows(study_info, frame)
}

study_history_first = NULL
study_history_last = NULL

url_start = 'https://classic.clinicaltrials.gov/ct2/history/'
for (f in 1:N){
  cat('Processing record ID ', gsub('.xml','',xml_files[f]), '\n',sep='') # progress
url = paste(url_start, nct_included[f], sep='')
download_xml(url=url, file='tmp.html')
in_page <- read_html('tmp.html') 
first_version = in_page %>% html_nodes("td:nth-child(4)")  %>% html_nodes("a") %>% html_attr("href") %>% first()
last_version = in_page %>% html_nodes("td:nth-child(4)")  %>% html_nodes("a") %>% html_attr("href") %>% last()

#first
address_first<- paste0(url,first_version)
download_xml(url=address_first, file='tmp.html')
in_page <- read_html('tmp.html') 
study_info_fields = in_page %>% html_nodes("*") %>% html_nodes("#StudyIdentificationBody") %>% html_nodes("td:nth-child(1)") %>% html_text() %>% str_remove_all(.,":") %>% trimws()
study_info_values = in_page %>% html_nodes("*") %>% html_nodes("#StudyIdentificationBody") %>% html_nodes("td:nth-child(2)") %>% html_text() %>% str_remove_all(.,":") %>% trimws()
frame = tibble(id=nct_included[f],field = list(study_info_fields),value=list(study_info_values))
study_history_first = bind_rows(study_history_first, frame)

#last
if(first_version!=last_version){
address_last <- paste0(url,last_version)
download_xml(url=address_last, file='tmp.html')
in_page <- read_html('tmp.html') 
study_info_fields = in_page %>% html_nodes("*") %>% html_nodes("#StudyIdentificationBody") %>% html_nodes("td:nth-child(1)") %>% html_text() %>% str_remove_all(.,":") %>% trimws()
study_info_values = in_page %>% html_nodes("*") %>% html_nodes("#StudyIdentificationBody") %>% html_nodes("td:nth-child(2)") %>% html_text() %>% str_remove_all(.,":") %>% trimws()
frame = tibble(id=nct_included[f],field = list(study_info_fields),value=list(study_info_values))
study_history_last = bind_rows(study_history_last, frame)
}

}

study_history = full_join(study_history_first,study_history_last,by='id',suffix=c('_first','_last'))
save(study_info,study_history,file='data/study_info_included_R1.rda')
