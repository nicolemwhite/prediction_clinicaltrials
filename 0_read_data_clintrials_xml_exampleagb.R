# 0_read_data_clintrials_xml.R
# read the clinicaltrials.gov data from XML files using the web
# version reading XMLs from mega zip file, see https://www.clinicaltrials.gov/ct2/resources/download, from https://clinicaltrials.gov/AllPublicXML.zip 
# March 2021
library(XML)
library(dplyr)
library(stringr)
source('99_functions.R')
home = getwd()

# folders to download
folders = dir('U:/Research/Projects/ihbi/aushsi/aushsi_barnetta/meta.research/ANZCTR/data/AllAPIXML')
folders = folders[folders!='Contents.txt']
# does data already exist?
existing_data = str_remove(dir('data/raw/', pattern='.RData'), '.RData')
folders = setdiff(folders, existing_data)
N = length(folders)

# loop through folders
for (f in 370:N){ # should be 1:N
  go = paste('U:/Research/Projects/ihbi/aushsi/aushsi_barnetta/meta.research/ANZCTR/data/AllAPIXML/', folders[f], sep='')
  setwd(go)
  xml_files = dir() # find all xml files in this folder (individual trials)
  
  cat('folder = ', folders[f], '\n',sep='') # progress

 studies = excluded = NULL
 for (k in 1:length(xml_files)){
  # get the XML data into R
  data <- xmlParse(xml_files[k])
  xml_data <- xmlToList(data) # only gets top level children, so vulnerable to missing information where there are multiple lists

  ## extract individual parts of the XML
  # basics
  id = null_na(xml_data$id_info$nct_id)
  brief_title = null_na(xml_data$brief_title)
  study_type = null_na(xml_data$study_type)
  if(is.na(study_type) == TRUE){
    next # skip to next study
  }
  if(study_type %in% c("Observational [Patient Registry]","Observational")){
    next # skip to next study
  }
  
  secondary_id = null_na(xml_data$id_info$secondary_id)
  if(length(secondary_id) > 1){stop("Multple secondary IDs")}
  status = null_na(xml_data$overall_status)
  # exclude the following
  exclude = pmax(status=='Withheld' , study_type == 'Expanded Access' , str_detect(string=tolower(secondary_id), 'actrn'), na.rm=TRUE)
  if(exclude == 1){
    # reason for exclusion
    reason = ''
    reason = ifelse(str_detect(string=tolower(secondary_id), 'actrn')==TRUE, 'ANZCTR', reason )
    reason = ifelse(status=='Withheld', 'Withheld', reason )
    reason = ifelse(study_type == 'Expanded Access', 'Expanded Access', reason )
    #
    frame = data.frame(id = id, reason=reason, status=status, study_type = study_type, secondary_id=secondary_id)
    excluded = bind_rows(excluded, frame)
    next # move on to next study
  }
  # count the number of countries - not collected consistently
  #countries = xpathApply(data, "//location_countries")
  #n_countries = length(countries)
  # study design
  phase = null_na(xml_data$phase, collapse=TRUE) # can be multiple so collapse into single result
  #study_design_observational = null_na(xml_data$study_design_info$observational_model) # no longer using observational
  #study_design_time = null_na(xml_data$study_design_info$time_perspective)
  # text search for study design - could also search for pilot as a word
  longitudinal = length(grep('longitudinal', xml_data$detailed_description$textblock, ignore.case = TRUE)) > 0
  adaptive_trial = length(grep('adaptive clinical trial|adaptive trial|adaptive design', xml_data$detailed_description$textblock, ignore.case = TRUE)) > 0
  platform_trial = length(grep('platform clinical trial|platform trial|platform design', xml_data$detailed_description$textblock, ignore.case = TRUE)) > 0
  # more study design
  allocation = null_na(xml_data$study_design_info$allocation)
  masking = null_na(xml_data$study_design_info$masking)
  masking = stringi::stri_extract_first(masking, regex="\\w+") # simplify to just first word
  purpose = null_na(xml_data$study_design_info$primary_purpose)
  assignment = null_na(xml_data$study_design_info$intervention_model)
  # intervention type, can be multiple per study
  interventions = xpathApply(data, "//intervention")
  type = ''
  if(length(interventions)>0){
    for (l in 1:length(interventions)){
      temporary = xmlToList(interventions[[l]])
      type = c(type, temporary$intervention_type)
    }
  }
  behavioral = any(str_detect(string=type, pattern='Behavioral'))
  biological = any(str_detect(string=type, pattern='Biological'))
  combination = any(str_detect(string=type, pattern='Combination Product'))
  device = any(str_detect(string=type, pattern='Device'))
  diagnostic = any(str_detect(string=type, pattern='Diagnostic Test'))
  dietary = any(str_detect(string=type, pattern='Dietary Supplement'))
  drug = any(str_detect(string=type, pattern='Drug'))
  genetic = any(str_detect(string=type, pattern='Genetic'))
  procedure = any(str_detect(string=type, pattern='Procedure'))
  radiation = any(str_detect(string=type, pattern='Radiation'))
  other = any(str_detect(string=type, pattern='Other'))
  # eligibility
  gender = null_na(xml_data$eligibility$gender)
  min_age = null_na(xml_data$eligibility$minimum_age)
  max_age = null_na(xml_data$eligibility$maximum_age)
  volunteers = null_na(xml_data$eligibility$healthy_volunteers)
  # dates
  submitted = null_na(xml_data$study_first_submitted, date=TRUE)
  posted = null_na(xml_data$study_first_posted$text, date=TRUE)
  updated = null_na(xml_data$last_update_submitted, date=TRUE)
  #
  lead_sponsor_class = null_na(xml_data$sponsors$lead_sponsor$agency_class)
  #condition = null_na(xml_data$condition) # too varied to be useful ...
  n_condition = str_count(string=as(data,'character'), pattern='\\<condition\\>') # ... just take number
  # sample size
  if(class(xml_data$enrollment)=='character'){ # version without detail
    sample_size = as.numeric(null_na(xml_data$enrollment))
    sample_size_type = 'Not stated'
  }
  if(class(xml_data$enrollment)!='character'){
    sample_size = as.numeric(null_na(xml_data$enrollment$text))
    sample_size_type = null_na(xml_data$enrollment$.attrs)
  }
  #
  n_primary = str_count(string=as(data,'character'), pattern='\\<primary_outcome\\>')
  n_secondary = str_count(string=as(data,'character'), pattern='\\<secondary_outcome\\>') # not yet tested
  n_arms = null_na(xml_data$number_of_groups) # wording depending on study type
  if(is.na(n_arms)==TRUE){n_arms = null_na(xml_data$number_of_arms)}

  # frame with one result per trial
  frame = data.frame(id = id, 
                     brief_title = brief_title,
                     status = status,
                     submitted = submitted,
                     posted = posted, 
                     updated = updated, 
                     lead_sponsor_class = lead_sponsor_class,
                     biological = biological,
                     behavioral = behavioral,
                     combination = combination,
                     device = device,
                     diagnostic = diagnostic,
                     dietary = dietary,
                     drug = drug,
                     genetic = genetic,
                     procedure = procedure,
                     radiation = radiation,
                     other = other,
                     n_condition = n_condition,
                     n_arms = n_arms,
                     purpose = purpose, 
                     masking = masking, 
                     allocation = allocation, 
                     assignment = assignment, 
                     phase = phase,
                     #study_design_observational = study_design_observational, # no longer using observational
                     #study_design_time = study_design_time,
                     sample_size = sample_size, 
                     sample_size_type = sample_size_type,
                     gender = gender, 
                     min_age = min_age, 
                     max_age = max_age,
                     volunteers = volunteers,
                     n_primary = n_primary, 
                     n_secondary = n_secondary,
                     longitudinal = longitudinal,
                     adaptive_trial = adaptive_trial,
                     platform_trial = platform_trial,
                     stringsAsFactors = FALSE)
  if(nrow(frame) > 1){stop('Multiple results per study')}
  # concatenate
  studies = bind_rows(studies, frame)
  # tidy up
  remove(data, xml_data, frame)
  # progress
  if(k%%100==0)(cat('Up to ',k,'.\r',sep=''))
}

 # check of numbers, no longer needed, was all fine
 #if(is.null(excluded)==FALSE){diff = nrow(studies) + nrow(excluded) - length(xml_files)}
 #if(is.null(excluded)==TRUE){diff = nrow(studies) - length(xml_files)}
 #if(diff !=0) {cat('Numbers do not add up for', f, '.\n', sep='')}
 
# save 
setwd(home)
outfile = paste('U:/Research/Projects/ihbi/aushsi/aushsi_barnetta/meta.research/ANZCTR/data/raw/', folders[f], '.RData', sep='')
save(studies, excluded, file=outfile)
} # end of folders loop
