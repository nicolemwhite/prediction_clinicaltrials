#screen records.r
source('99_functions.R')
source('99_packages.R')
g.theme = theme_bw()+theme(legend.position = 'top',legend.direction='horizontal')
cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

data.file <- "Z:/clinicaltrials/data/analysis_ready/study_data_v1.rda"
metadata.file <- "Z:/clinicaltrials/data/meta/record_info.rda"
load(data.file)

scan_fields = c("official_title","brief_title","brief_summary","detailed_summary","keywords","mesh_terms",
                "primary_outcome_measures","primary_outcome_description","secondary_outcome_measures","secondary_outcome_description")

search_string_c = list()
search_string_c[[1]] <- c("machine learning","artificial intelligence","deep learning",
                          "prediction model","predictive model","prediction score","predictive score",
                          "warning score","risk score","risk prediction","risk model","scoring model") %>% str_c(collapse='|')
search_string_c[[2]] <- c("prognosis","prognostic","diagnose","diagnosis","diagnostic") %>% str_c(collapse='|')




##this is really slow but extract hits in one step. Run outside of markdown.
matches_model = select(dat,all_of(c("id",scan_fields))) %>% 
  rowwise() %>%
  mutate_at(vars(all_of(scan_fields)), ~c(str_extract_all(tolower(.x),search_string_c[[1]],simplify=T)) %>% subset(!is.na(.)) %>% unique(.) %>% str_c(collapse=';'))

matches_outcome = select(dat,all_of(c("id",scan_fields))) %>% 
  rowwise() %>%
  mutate_at(vars(all_of(scan_fields)), ~c(str_extract_all(tolower(.x),search_string_c[[2]],simplify=T)) %>% subset(!is.na(.)) %>% unique(.) %>% str_c(collapse=';'))

#take ids from matches_model to filter to prognostic or diagnostic
id_model = matches
matches_c = select(dat,all_of(c("id",scan_fields))) %>% 
  rowwise() %>%
  mutate_at(vars(all_of(scan_fields)), ~c(str_extract_all(tolower(.x),str_c(search_string_c,collapse='|'),simplify=T)) %>% subset(!is.na(.)) %>% unique(.) %>% str_c(collapse=';'))

