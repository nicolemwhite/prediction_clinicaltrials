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
