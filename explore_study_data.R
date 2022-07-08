#explore_xml_sample.R
source('99_functions.R')
source('99_packages.R')
g.theme = theme_bw()+theme(legend.position = 'top',legend.direction='horizontal')
cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

data.file <- "Z:/clinicaltrials/data/analysis_ready/study_data_v1.rda"
metadata.file <- "Z:/clinicaltrials/data/meta/record_info.rda"
load(data.file)
load(metadata.file)
str(dat)


#1. total records and overall completeness by record
dat = dat %>% mutate(
  FolderName=paste0(str_sub(id,1,7),'xxxx'),
  year_posted = (gsub('^.*, ','',posted)),
  n_complete = rowSums(!is.na(select(.,official_title:keywords))),
  perc_complete = 100*rowSums(!is.na(select(.,official_title:keywords))))

## plot total and cumulative records posted over time
fig1a = dat %>% count(year_posted) %>% 
  ggplot(aes(year_posted,n)) + geom_bar(stat='identity') +
  scale_x_discrete('Year posted') + scale_y_continuous('Total annual records',breaks=seq(0,10000,1000),labels = function(x) format(x, big.mark=',',scientific = FALSE))+g.theme


fig1b = dat %>% count(year_posted) %>% mutate(cumulative_total = cumsum(n)) %>% 
  ggplot(aes(year_posted,cumulative_total)) + geom_bar(stat='identity') +
  scale_x_discrete('Year posted') + scale_y_continuous('Cumulative total records',breaks=seq(0,100000,10000),labels = function(x) format(x, big.mark=',',scientific = FALSE))+g.theme

ggarrange(fig1a,fig1b,nrow=2,ncol=1,align='hv')

#completeness by field
dat %>% select(id:year_posted) %>% gather(variable,value,-c(FolderName,id,year_posted)) %>% group_by(year_posted,variable) %>% summarise(missing = sum(is.na(value)),.groups='drop') %>%
  filter(missing>0) %>% ggplot(aes(year_posted,missing,group=variable,colour=variable))+geom_line(size=1.5)+
  scale_y_continuous('Total records with missing information',breaks=seq(0,10000,1000),labels = function(x) format(x, big.mark=',',scientific = FALSE))+g.theme+
  scale_colour_manual(values=cbPalette)

#serach strings from summary document
## scan all title, description, keywords, mesh_terms
search_string <- list()
search_string[[1]] <- c("machine learning","artificial intelligence","deep learning") %>% str_c(collapse = '|')
search_string[[2]] <- c("predictive","prediction", "warning", "scoring","risk") %>% str_c(collapse = '|')
search_string[[3]] <- c("score","model","algorithm","rule") %>% str_c(collapse = '|')


scan_fields = c("official_title","brief_title","brief_summary","detailed_summary","keywords","mesh_terms",
                "primary_outcome_measures","primary_outcome_description","secondary_outcome_measures","secondary_outcome_description")

core_info = c("id","overall_status","last_known_status","submitted","posted","updated","study_type",'study_design_allocation',
              "study_design_purpose")

matches = lapply(seq_along(search_string), function(k)
  select(dat,all_of(c("id",scan_fields))) %>% 
    mutate_at(vars(all_of(scan_fields)),~ifelse(grepl(search_string[[k]],.x),1,0)) %>% 
    mutate(match_found = pmin(1,rowSums(select(.,all_of(scan_fields))),na.rm=T)))

#eg matches for search_string_1
matches[[1]] %>% filter(match_found==1) %>% gather(variable,value,-c(id,match_found)) %>%
  group_by(variable) %>% summarise(sum(value))

search_string_c = list()
search_string_c[[1]] <- c("machine learning","artificial intelligence","deep learning",
                          "prediction model","predictive model","prediction score","predictive score",
                          "warning score","risk score","risk prediction") %>% str_c(collapse='|')
search_string_c[[2]] <- c("prognosis","prognostic","diagnose","diagnosis","diagnostic") %>% str_c(collapse='|')


matches_c = select(dat,all_of(c("id",scan_fields))) %>% 
    mutate_at(vars(all_of(scan_fields)),~ifelse(grepl(search_string_c[[1]],.x,ignore.case=T) & grepl(search_string_c[[2]],.x,ignore.case=T),1,0)) %>% 
    mutate(match_found = pmin(1,rowSums(select(.,all_of(scan_fields)),na.rm=T)))



filter(matches_c,match_found==1) %>% gather(variable,match,-c(id,match_found)) %>% 
  mutate_at(c("match"),~factor(.x,levels=0:1,labels=c("No","Yes"))) %>%
  ggplot(aes(x=variable,y=id,fill=match)) + geom_tile() +g.theme+
  theme(axis.text.x = element_text(angle=40,hjust=1),axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())+scale_fill_manual(values=cbPalette)+
  scale_x_discrete('Field')


id_matches = filter(matches_c,match_found==1) %>% pull(id)
