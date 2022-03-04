#extract_xml_from_zip.R
source('99_packages.R')
source('99_functions.R')

#setseed
TeachingDemos::char2seed('clinicaltrials')

home = getwd()

# review files in zipped folder
all_results = unzip('Z:/clinicaltrials/AllPublicXML.zip',list=TRUE) %>% data.frame() %>%
  filter(Name!='Contents.txt') %>% mutate(FileName = gsub(".*/","",Name))

#take random sample to download (10% of all records)
n = floor(0.1*nrow(all_results)) 
sample_results = sample_n(all_results,n)

#extract relevant information to store in a combined data.frame
studies = NULL
for (k in 1:n){
  cat('Processing record ID ', gsub('(.*)/','',sample_results[k,"Name"]), '\n',sep='') # progress
  
result = getXMLrecord_zip(FilePath = sample_results[k,"Name"])
xml_data = result$xml
data = result$data

#get high-level information
id = null_na(xml_data$id_info$nct_id)
official_title = null_na(xml_data$official_title)
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


brief_summary = null_na(sapply(getNodeSet(data,"//brief_summary"),xpathSApply,"./textblock",xmlValue) %>% str_remove_all(.,'\\r|\\n|\\s{2,}'))
detailed_summary = null_na(sapply(getNodeSet(data,"//detailed_description"),xpathSApply,"./textblock",xmlValue) %>% str_remove_all(.,'\\r|\\n|\\s{2,}'))

#outcome measures
primary_outcome_measures = null_na(str_c(sapply(getNodeSet(data,"//primary_outcome"),xpathSApply,"./measure",xmlValue),collapse='. ') %>% str_remove_all(.,'\\r|\\n|\\s{2,}'))
primary_outcome_description = null_na(str_c(sapply(getNodeSet(data,"//primary_outcome"),xpathSApply,"./description",xmlValue),collapse=' ') %>% str_remove_all(.,'\\r|\\n|\\s{2,}'))
secondary_outcome_measures = null_na(str_c(sapply(getNodeSet(data,"//secondary_outcome"),xpathSApply,"./measure",xmlValue),collapse=' ') %>% str_remove_all(.,'\\r|\\n|\\s{2,}'))
secondary_outcome_description = null_na(str_c(sapply(getNodeSet(data,"//secondary_outcome"),xpathSApply,"./description",xmlValue),collapse=' ') %>% str_remove_all(.,'\\r|\\n|\\s{2,}'))

#add intervention type/name?

frame = data.frame(id=id,
                   official_title=official_title,
                   brief_title=brief_title,
                   brief_summary=brief_summary,
                   detailed_summary=detailed_summary,
                   overall_status=overall_status,
                   last_known_status=last_known_status,
                   submitted=submitted,
                   posted=posted,
                   updated=updated,
                   study_type=study_type,
                   study_design_allocation=study_design_allocation,
                   study_design_purpose=study_design_purpose,
                   primary_outcome_measures=primary_outcome_measures,
                   primary_outcome_description=primary_outcome_description,
                   secondary_outcome_measures=secondary_outcome_measures,
                   secondary_outcome_description=secondary_outcome_description,
                   mesh_terms=mesh_terms,
                   keywords=keywords)
studies = bind_rows(studies, frame)



}
