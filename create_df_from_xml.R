#create_df_from_xml
source('99_packages.R')
source('99_functions.R')

load('data/meta/record_info.rda')


#loop over folders; save folder specific data
for (k in seq_along(subfolder_n[['FolderName']])){
  studies = NULL
  folder_name = subfolder_n[k,'FolderName']
  xml_files = list.files(paste0('data/',folder_name))
  for (x in seq_along(xml_files)){
    study_record = paste(getwd(),'data',folder_name,xml_files[x],sep='/')
    go <- readLines(study_record)
    data = xmlParse(go)
    xml_data <- xmlToList(data) 
    
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

    #add intervention type//name?

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
    # 
    
  } #end of within subfolder loop
  #save studies
  fileout = paste0('data/processed/',folder_name,'.rda')
  save(studies,file=fileout)
  cat('Processed subfolder ',folder_name[[1]],'; ',k,'/',nrow(subfolder_n),'\n',sep='')
  
}
