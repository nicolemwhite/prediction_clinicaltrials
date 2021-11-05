# 0_read_data_clintrials_xml.R
# Created by AGB
# Modified by NMW
# read the clinicaltrials.gov data from XML files using the web
# version reading XMLs from mega zip file, see https://www.clinicaltrials.gov/ct2/resources/download, from https://clinicaltrials.gov/AllPublicXML.zip 
# March 2021
library(XML)
library(tidyverse)
source('99_functions.R')
home = getwd()

# folders to download
#filepath to downloaded XML files
xml_files = dir('Z:/clinicaltrials/search_result')
xml_files = xml_files[grepl('NCT(.*).xml',xml_files)]

#not run: revisit later after multiple batches of xml downloads
# # does data already exist?
# existing_data = str_remove(dir('data/raw/', pattern='.RData'), '.RData')
# folders = setdiff(folders, existing_data)

N = length(xml_files)
studies = NULL

# loop through folders
for (f in 1:N){ # should be 1:N
  go = paste('Z:/clinicaltrials/search_result/', xml_files[f], sep='')
  #setwd(go)
  #xml_files = dir() # find all xml files in this folder (individual trials)
  
  cat('Processing record ID ', gsub('.xml','',xml_files[f]), '\n',sep='') # progress
  
  # get the XML data into R
  data <- xmlParse(go)
  xml_data <- xmlToList(data) # only gets top level children, so vulnerable to missing information where there are multiple lists
  #get high-level information
  id = null_na(xml_data$id_info$nct_id)
  brief_title = null_na(xml_data$brief_title)
  overall_status = null_na(xml_data$overall_status)
  last_known_status = null_na(xml_data$last_known_status)
  
  #dates
  submitted = null_na(xml_data$study_first_submitted)
  posted = null_na(xml_data$study_first_posted$text)
  updated = null_na(xml_data$last_update_submitted)
  
  #info on study design
  study_type = null_na(xml_data$study_type)  
  study_design_allocation = null_na(xml_data$study_design_info$allocation)
  study_design_purpose = null_na(xml_data$study_design_info$primary_purpose)
  
  #mesh terms and keyword lists
  mesh_terms = sapply(getNodeSet(data,"//mesh_term"),xpathSApply,".",xmlValue) %>% str_c(.,collapse = '|')
  keywords = sapply(getNodeSet(data,"//keyword"),xpathSApply,".",xmlValue) %>% str_c(.,collapse = '|')
  
  
  brief_summary = sapply(getNodeSet(data,"//brief_summary"),xpathSApply,"./textblock",xmlValue) %>% str_remove_all(.,'\\r|\\n|\\s{2,}')

  frame = data.frame(id=id,
                     brief_title=brief_title,
                     brief_summary=brief_summary,
                     overall_status=overall_status,
                     last_known_status=last_known_status,
                     submitted=submitted,
                     posted=posted,
                     updated=updated,
                     study_type=study_type,
                     study_design_allocation=study_design_allocation,
                     study_design_purpose=study_design_purpose,
                     mesh_terms=mesh_terms,
                     keywords=keywords)
  studies = bind_rows(studies, frame)
  
} #end of f loop

# save 
setwd(home)
save(studies, file='processed_studies.rda')

