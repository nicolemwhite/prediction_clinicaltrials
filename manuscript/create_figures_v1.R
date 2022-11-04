source('99_packages.R')
source('99_functions.R')



search_terms <- c("machine learning","artificial intelligence","deep learning","prediction model","predictive model","prediction score","predictive score","warning score","risk score","risk prediction","prognostic model","diagnostic model")


#load full dataset from local machine - too large for github
load(file='Z:/clinicaltrials/data/analysis_ready/all_studies.rda')

dat = dat %>% mutate(year = gsub(".*, ","",posted))
dat %>% filter(grepl('Observational',study_type)) %>% count(year)

# #read in included studies
# dat_rayyan = read.csv('data/included_studies.csv')
# 
# dat_rayyan %>% count(year) %>% ggplot(aes(x=year,y=n))+geom_bar(stat='identity')
# 
# png('manuscript/figures/screened_records.png',width=1000,height=600)
# ggplot(dat_rayyan,aes(year))+geom_histogram(aes(y=cumsum(..count..)),binwidth=1,fill='darkgrey',colour='black')+
#   scale_x_continuous('Year first posted',breaks=seq(2000,2022))+
#   scale_y_continuous('Cumulative number of records eligible for inclusion',breaks=seq(0,1600,100))+
#   theme_minimal()+theme(panel.grid.minor = element_blank(),axis.text = element_text(size=12),text=element_text(size=12))
# invisible(dev.off())

#flowchart using DiagrammeR

n_start = dat %>% filter(grepl('Observational',study_type)) %>% nrow() 
n_screened = 1465
n_included = 969

#saved manually - TODO fix
grViz(diagram = "digraph flowchart {
  node [fontname = arial, shape = rectangle]
  box0 [label = '@@1']
  box1 [label = '@@2']
  box2 [label = '@@3']
  
  exclude0 [label = '@@4']
  exclude1 [label = '@@5']

  box0 -> box1 -> box2;
  box0 ->exclude0;
  box1 -> exclude1;

}
  
  [1]: paste0(format(n_start,big.mark=','),' observational study records downloaded: 1 January 2000 to 3 March 2022')
  [2]: paste0(format(n_screened,big.mark=','),' records matched to 1 or more search terms')    
  [3]: paste0(format(n_included,big.mark=','),' records included\\nModel type:\\nDiagnostic = 436\\nPrognostic = 404\\nDiagnostic + Prognostic = 129\\nStudy type:\\nDevelopment only = 575\\nValidation only = 108\\n Development + Validation = 286')
  [4]: paste0(format(n_start-n_screened,big.mark=','),' records excluded')    
  [5]: paste0(format(n_screened - n_included,big.mark=','),' excluded after screening')

")


# #stacked probability plot for last_known_status; take all rayyan records as a first pass and updated once screening complete
# 
# 
# to_plot = dat_rayyan %>% select(key,overall_status,posted,updated)
# 
# to_plot %>% count(overall_status)
