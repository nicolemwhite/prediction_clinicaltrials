#1_summarise_included_studies.R
source('99_packages.R')
source('99_functions.R')

# load('data/final_studies_R1.rda') #revised after classification
# load('data/mesh/mesh_term_info.rda')
# 
# #frequency analysis - keyword hits
# search_terms = c('machine learning','artificial intelligence','deep learning', 'prediction model', 'predictive model', 'prediction score', 'predictive score', 'warning score', 'risk score', 'risk prediction', 'prognostic model', 'diagnostic model')
# 
# load('Z:/clinicaltrials/data/analysis_ready/all_studies.rda')
# decisions = inner_join(select(all_records,NCT:done),dat,by=c('NCT'='id')) %>% rename('final_decision'=done)
# 
# save(decisions,file='data/final_studies_with_meta_R1.rda') #revised after classification

search_terms = c('machine learning','artificial intelligence','deep learning', 'prediction model', 'predictive model', 'prediction score', 'predictive score', 'warning score', 'risk score', 'risk prediction', 'prognostic model', 'diagnostic model')
load('data/final_studies_with_meta_R1.rda')



dat_dup = openxlsx::read.xlsx('data/manual checks/possible duplicates-20230713.xlsx')
exclude_dup = dat_dup %>% filter(duplicate=='y') %>% pull(b) %>% unique()

decisions = filter(decisions,!NCT %in% exclude_dup)


load('data/mesh/mesh_term_info.rda')

#create additional columns to identify which search terms were found in each record
search_hits = select(decisions,-c(Development,Diagnostic,Prognostic,Validation)) %>% mutate(across(official_title:keywords,~str_extract_all(string=tolower(.x),pattern = paste0('\\b',str_c(search_terms,collapse='\\b|\\b'),'\\b')) %>% sapply(function(x) str_c(unique(x),collapse =';'))))
search_hits_l = search_hits %>% gather(variable,value,-NCT,-final_decision) %>% filter(!is.na(value),value!='') %>% rowwise() %>% mutate_at('value',~(str_split(.,';')))

#tidy up, collapse smaller categories
ftab_hits = search_hits_l %>% unnest(value) %>% 
  mutate(variable_c = case_when(
    grepl('^primary_outcome_',variable) ~ 'Primary Outcome',
    grepl('^secondary_outcome_',variable) ~ 'Secondary Outcome',
    grepl('_title$',variable) ~ 'Title',
    grepl('_summary$',variable) ~ 'Summary',
    TRUE ~ as.character(variable)
  )) %>% mutate_at('variable_c',~str_replace_all(.,'_',' ') %>% str_to_title(.) %>% factor(levels=c('Title','Summary','Primary Outcome','Secondary Outcome','Keywords'))) %>% 
  mutate_at('value',~str_to_title(.)) %>%
  distinct(NCT,final_decision,variable_c,value) %>%
  count(final_decision,variable_c,value)

totals = filter(ftab_hits,final_decision=='include') %>% dgroup_by(value) %>% summarise(m =abs(sum(n)),.groups='drop') %>% arrange(m)

png('manuscript/supplement/FigureS1.png',width=8,height=6,units='in',res=300)
filter(ftab_hits,final_decision %in% c('exclude','include')) %>% 
  mutate_at('n',~ifelse(final_decision=='exclude',(-1)*.,.)) %>% mutate_at('value',~factor(.,levels=totals[['value']])) %>%
ggplot(aes(x=value,y=n,fill=variable_c,group=final_decision))+geom_col(col='black')+geom_hline(aes(yintercept=0),linetype='solid',linewidth=1.2)+
  expand_limits(y=c(-200,400))+
  annotate("text",x=c(0.8,0.8),y=c(-150,150),label=c('Excluded','Included'),size=5)+
  scale_y_continuous('Number of records found',breaks=seq(-200,400,100),labels = c(200,100,0,seq(100,400,100)))+
  scale_x_discrete('Keyword searched')+
  coord_flip()+scale_fill_manual(values=cbPalette)+
  theme_bw() + theme(legend.position ='top',legend.title = element_blank(),text=element_text(size=12))
invisible(dev.off())

#filter the hits for machine learning, artificial intelligence and deep learning for further review
ftab_hits_review = search_hits_l %>% unnest(value) %>% 
  mutate(variable_c = case_when(
    grepl('^primary_outcome_',variable) ~ 'Primary Outcome',
    grepl('^secondary_outcome_',variable) ~ 'Secondary Outcome',
    grepl('_title$',variable) ~ 'Title',
    grepl('_summary$',variable) ~ 'Summary',
    TRUE ~ as.character(variable)
  )) %>% mutate_at('variable_c',~str_replace_all(.,'_',' ') %>% str_to_title(.) %>% factor(levels=c('Title','Summary','Primary Outcome','Secondary Outcome','Keywords'))) %>% 
  mutate_at('value',~str_to_title(.)) %>%
  filter(final_decision=='include',value %in% c('Artificial Intelligence','Deep Learning','Machine Learning')) %>% 
  distinct() #unique NCT-value-field combinations

#find the matching sentences in each record
extract_matching_text <- function(indata=decisions,field_label='official_title',field_value='Artificial Intelligence',NCT_num='NCT05260281'){
  lst1 <- strsplit(filter(indata,NCT==NCT_num) %>% pull(!!field_label), '(?<=\\.)\\s+', perl = TRUE)
  out = sapply(lst1, function(x) paste(grep(field_value, x, value = TRUE,ignore.case=T), collapse="..."))
  return(out)
}


#ML/AI/DP only
#add acronyms to get subsequent mentions in text
ftab_hits_review = ftab_hits_review %>% mutate(value_c = paste0('\\b',value,'\\b|\\b',gsub('\\b(\\pL)\\pL{2,}|.','\\U\\1',value,perl = TRUE),'\\b'))
ftab_hits_review = ftab_hits_review %>% rowwise() %>% mutate(matched_text = extract_matching_text(field_label=variable,field_value=value,NCT_num=NCT))
ftab_hits_review %>% select(NCT,variable,value,matched_text) %>% spread(variable,matched_text,fill="") %>% select(NCT,value,ends_with('title'),ends_with('summary'),starts_with('primary_outcome'),starts_with('secondary_outcome'),keywords)

#all search terms
ftab_hits_all_terms = filter(search_hits_l,final_decision=='include') %>% unnest(value) %>%  
  mutate(variable_c = case_when(
    grepl('^primary_outcome_',variable) ~ 'Primary Outcome',
    grepl('^secondary_outcome_',variable) ~ 'Secondary Outcome',
    grepl('_title$',variable) ~ 'Title',
    grepl('_summary$',variable) ~ 'Summary',
    TRUE ~ as.character(variable)
  )) %>% mutate_at('variable_c',~str_replace_all(.,'_',' ') %>% str_to_title(.) %>% factor(levels=c('Title','Summary','Primary Outcome','Secondary Outcome','Keywords'))) %>% 
  mutate_at('value',~str_to_title(.)) %>%
  select(NCT,variable_c,value) %>% distinct()

ftab_summary_search_terms = count(ftab_hits_all_terms,value,variable_c) %>% spread(variable_c,n,fill=0)
ad = ftab_hits_all_terms %>% distinct(NCT,value) %>% count(value,name='Total records found')

ftab_summary_search_terms = left_join(ad,ftab_summary_search_terms,by='value') %>% arrange(-`Total records found`)
ftab_summary_search_terms


#alluvial diagram linking search terms with manual classifications and publication matches
ad =  decisions %>% 
  mutate_at(c('Development','Validation','Diagnostic','Prognostic'),~replace_na(.,'FALSE')) %>%
  mutate(analysis = case_when(
      (Development==TRUE & Validation==FALSE) ~ 'Development',
      (Development==FALSE & Validation==TRUE) ~ 'Validation',
      (Development==TRUE & Validation==TRUE) ~ 'Development + Validation'),
      outcome = case_when(
        (Prognostic==TRUE & Diagnostic==FALSE) ~ 'Prognostic',
        (Prognostic==FALSE & Diagnostic==TRUE) ~ 'Diagnostic',
        (Prognostic==TRUE & Diagnostic==TRUE) ~ 'Diagnostic + Prognostic'))
  
ftab_alluvial = ftab_hits_all_terms %>% left_join(ad %>% select(NCT,analysis,outcome),by='NCT') %>% mutate_at(c('analysis','outcome'),~replace_na(.,'Unclear')) %>% distinct(NCT,value,analysis,outcome)

load('data/publications/times_to_publication_20231204.rda') #use dat_pubs_2 as includes NCT-linked + ML classifier
ad = dat_pubs_2 %>% select(NCT,pub_outcome)

ftab_alluvial = ftab_alluvial %>% left_join(ad,by='NCT') %>% mutate_at('pub_outcome',~factor(.,levels=0:2,labels=c('None','Unrelated publication','Matched publication')))


ftab_alluvial %>% count(value,analysis,pub_outcome) %>% spread(pub_outcome,n) %>%
  mutate(total_hits = None+`Unrelated publication`+`Matched publication`) %>%
  mutate(total_pubs = `Unrelated publication`+`Matched publication`) %>%
  mutate(pub_outcome = paste0(total_hits,'\n',`Matched publication`,'/',total_pubs)) %>%
  select(value,analysis,pub_outcome) %>% spread(analysis,pub_outcome,fill='0\n(--)') %>% flextable()
  

#try again as an upset plot
g1 = ftab_alluvial %>% 
  group_by(NCT,analysis) %>% summarise('Search term' = list(value),.groups='drop') %>%
  distinct(NCT,.keep_all = T) %>% mutate_at('analysis',~factor(.,levels=c('Development','Development + Validation','Validation','Unclear'))) %>%
  ggplot(aes(x=`Search term`,fill=analysis)) + geom_bar(colour='black') + 
  scale_fill_manual(values=cbPalette)+
  scale_x_upset(n_intersections = 25) + 
  expand_limits(y=100)+theme_minimal()+scale_y_continuous('Number of included records',breaks=seq(0,100,by=10)) + 
  theme(text=element_text(size=12),panel.grid.minor = element_blank(),plot.margin = margin(1,1,1,3, "cm"),legend.position = 'top',legend.title=element_blank())+
  theme_combmatrix(combmatrix.panel.point.color.fill = "black",
                   combmatrix.panel.point.size = 5,
                   combmatrix.panel.line.size = 0,
                   combmatrix.label.make_space = FALSE,
                   combmatrix.label.text = element_text(size=12))


png('manuscript/figures/upset_plot_analysis.png',width=8,height=10,units='in',res=300)
g1
invisible(dev.off())

###
ftab_hits_all_terms %>% distinct(NCT,value) %>%
  group_by(NCT) %>% summarise(Classification = list(value),.groups='drop') %>%
  left_join(decisions %>% select(NCT,analysis))

ftab_hits = ftab_hits_review %>% rowwise() %>% mutate(matched_text = extract_matching_text(field_label=variable,field_value=value,NCT_num=NCT))


write.xlsx(ftab_hits_review,file='data/manual checks/included/ML AI DL studies to check.xlsx')

included_studies = filter(decisions,final_decision=='include') 

ad  = select(included_studies,NCT,Diagnostic,Prognostic,Development,Validation) %>% 
  pivot_longer(cols=-c(NCT)) %>% filter(!is.na(value),value=='TRUE') %>%  
  group_by(NCT) %>% summarise(Classification = list(name),.groups='drop')

included_studies = left_join(included_studies,ad,by='NCT')

g1 = included_studies %>% distinct(NCT,.keep_all = T) %>% ggplot(aes(x=Classification)) + geom_bar(fill='darkgrey',colour='black') + scale_x_upset() + 
  expand_limits(y=300)+theme_minimal()+scale_y_continuous('Number of included records',breaks=seq(0,300,by=25)) + 
  theme(text=element_text(size=12),panel.grid.minor = element_blank(),plot.margin = margin(1,1,1,2, "cm"))+
  theme_combmatrix(combmatrix.panel.point.color.fill = "black",
                   combmatrix.panel.point.size = 5,
                   combmatrix.panel.line.size = 0,
                   combmatrix.label.make_space = FALSE,
                   combmatrix.label.text = element_text(size=12))


png('manuscript/figures/upset_plot.png',width=8,height=6,units='in',res=300)
g1
invisible(dev.off())


#plot screening record decisions over time. need to join with study year



included_studies = included_studies %>% mutate(year_posted = str_remove_all(posted,'^.*, ') %>% as.numeric)

included_studies = included_studies %>% mutate_at(vars(Development:Validation),~replace_na(.,FALSE)) %>%
  mutate(analysis = case_when(
    (Development==TRUE & Validation==FALSE) ~ 'Development',
    (Development==FALSE & Validation==TRUE) ~ 'Validation',
    (Development==TRUE & Validation==TRUE) ~ 'Development + Validation'),
    outcome = case_when(
      (Prognostic==TRUE & Diagnostic==FALSE) ~ 'Prognostic',
      (Prognostic==FALSE & Diagnostic==TRUE) ~ 'Diagnostic',
      (Prognostic==TRUE & Diagnostic==TRUE) ~ 'Diagnostic + Prognostic'))


g2 = ggplot(included_studies,aes(year_posted))+geom_histogram(aes(y=cumsum(..count..)),binwidth=1,fill='darkgrey',colour='black')+
  expand_limits(y=1000)+
  scale_x_continuous('Year first posted',breaks=seq(2000,2022,1),guide = guide_axis(n.dodge = 2))+
  scale_y_continuous('Cumulative number of included records',limits=c(0,928),breaks=seq(0,930,100))+
  theme_bw()+theme(panel.grid.minor = element_blank(),axis.text = element_text(size=12),text=element_text(size=12))

png('manuscript/figures/incuded_records_byyear.png',width=800,height=400)
g2
invisible(dev.off())


png('manuscript/figures/record_characteristics.png',width=800,height=800)
ggarrange(g2,g1,nrow=2,ncol=1,labels=LETTERS[1:2])
invisible(dev.off())



all_years = tibble(outcome = rep(c('Prognostic','Diagnostic','Diagnostic + Prognostic'),each=3),analysis=rep(c('Development','Validation','Development + Validation'),3),year_posted=list(2000:2022)) %>% unnest(cols='year_posted')

#check NAs
ftab_outcome = included_studies %>% count(outcome,year_posted) %>% mutate_at(c('outcome'),~replace_na(.,'Unclear'))
ftab_outcome = right_join(ftab_outcome,all_years,by=c('outcome','year_posted')) %>% 
  mutate_at('n',~replace_na(.,0)) %>% mutate_at('outcome',~factor(.,levels=c('Prognostic','Diagnostic','Diagnostic + Prognostic'))) 
ftab_outcome = ftab_outcome %>% arrange(outcome,year_posted) %>% group_by(outcome,analysis) %>% mutate(n_cusum = cumsum(n)) %>% ungroup()

g3 = ftab_outcome %>% ggplot(aes(x=year_posted,y=n_cusum,colour=outcome))+geom_point(size=2)+geom_line(size=0.75)+
  scale_x_continuous('Year first posted',breaks=seq(2000,2022,1),guide = guide_axis(n.dodge = 2))+scale_y_continuous('Cumulative number of included records',breaks=seq(0,650,50))+
  scale_colour_manual(values=cbPalette)+
  theme_minimal()+theme(strip.background = element_rect(fill='white'),
                        strip.text = element_text(size=12),axis.text.x = element_text(size=12),axis.text.y = element_text(size=12),
                        axis.text = element_text(size=12),legend.position = c(0.2,0.75),legend.title = element_blank(),legend.text = element_text(size=11))

png('manuscript/figures/included_records_outcome_studytype.png',width=8,height=6,units='in',res=300)
g3
invisible(dev.off())


###from 3_process hisotry
active <- readRDS("data/clintrials_history_active_studies.rds")
complete <- readRDS("data/clintrials_history_completed.rds")
unknown <- readRDS("data/clintrials_history_unknown_stopped.rds")

web_history = bind_rows(active,complete,unknown)

dat_dup = openxlsx::read.xlsx('data/manual checks/possible duplicates-20230713.xlsx')
exclude_dup = dat_dup %>% filter(duplicate=='y') %>% pull(b) %>% unique()

web_history = filter(web_history,!NCT %in% exclude_dup)

#get changes in overall status
#for each NCT and overall status, take the first date posted
dat_status = web_history %>% unnest(cols=c(field_label,field_value)) %>% filter(field_label %in% c('Date Posted','Overall Status')) %>% spread(field_label,field_value) 

dat_status_c = dat_status %>% group_by(NCT,`Overall Status`) %>% slice_min(`Date Posted`) %>% ungroup() %>% janitor::clean_names() %>% arrange(nct,index)

dat_status_c = dat_status_c %>% group_by(nct) %>% 
  mutate_at('overall_status',~str_remove_all(.,'\\s+\\[.*$')) %>%
  mutate_at('overall_status',~case_when(. %in% c('Suspended','Terminated','Withdrawn','Withheld') ~ 'Suspended/Terminated/Withdrawn',TRUE~as.character(.))) %>%
  mutate(date_update = lead(date_posted,1),date_start=min(date_posted),
         from=overall_status,to=coalesce(lead(overall_status,1),overall_status)) %>% ungroup() %>%
  mutate_at(c('date_update','date_posted','date_start'),~as.Date(.)) %>% rowwise() %>%
  mutate_at('date_update',~replace_na(.,date_posted+years(5)))


to_plot = dat_status_c %>% mutate(start=pmax(0,as.numeric(date_posted - date_start)),stop=pmax(0,as.numeric(date_update - date_start)-1)) %>% 
  mutate_at('start',~ifelse(.==stop+1,stop,.)) %>% mutate_at('stop',~ifelse(start==.,.+1,.)) %>% rowwise() %>%
  mutate(day=list(seq(start,stop))) %>% unnest(cols=day) %>% select(nct,day,from,to,day) %>% distinct() %>%
  group_by(day,from) %>% summarise(n=n(),.groups='drop_last') %>% mutate(prop=n/sum(n)) %>% ungroup() %>%
  filter(between(day,1,1825))


ga = ggplot(to_plot,aes(x=day,y=prop,fill=from))+geom_col(width=1)+scale_fill_manual(values=c(cbPalette,'black'))+
  scale_x_continuous('Years since first posted',breaks=seq(0,1825,365),labels=seq(0,1825,365)/365,expand = c(0, 0), limits = c(0, NA))+theme_bw()+
  scale_y_continuous('Proportion of included studies',breaks=seq(0,1,0.1),expand = c(0, 0), limits = c(0, NA))+expand_limits(x=10)+
  theme(legend.title = element_blank(),legend.position = 'top',text=element_text(size=12),panel.grid.minor = element_blank())
# gb = ggplot(to_plot,aes(x=day,y=n,fill=from))+geom_col(width=1)+scale_fill_manual(values=c(cbPalette,'black'))+
#   scale_x_continuous('Years since first posted',breaks=seq(0,5000,365.25),labels=seq(0,5000,365.25)/365.25)+theme_minimal()+
#   scale_y_continuous('Number of ongoing studies',breaks=seq(0,1000,100))+
#   theme(legend.title = element_blank(),legend.position = 'top',text=element_text(size=12),panel.grid.minor = element_blank())

png('manuscript/figures/status_over_time.png',width=8,height=6,units='in',res=300)
ga
invisible(dev.off())


#sample size information
dat_sample_size = web_history %>% unnest(cols=c(field_label,field_value)) %>% filter(field_label %in% c('Enrollment','Date Posted')) %>% spread(field_label,field_value) %>% janitor::clean_names()
dat_sample_size = dat_sample_size %>% filter(grepl('Actual|Anticipated',enrollment)) %>%
  mutate_at('enrollment',~str_remove_all(.,'[\\[\\]]')) %>% separate(enrollment,c('n','stage'))

completed_sample_anticipated = filter(dat_sample_size,stage=='Anticipated') %>% group_by(nct) %>% slice_min(index) %>% ungroup() %>% select(nct,date_posted,n) %>% distinct()
completed_sample_actual = filter(dat_sample_size,stage=='Actual') %>% group_by(nct) %>% slice_max(index) %>% ungroup() %>% select(nct,date_posted,n) 

completed_sample_sizes = bind_rows(list(Anticipated = completed_sample_anticipated,Actual = completed_sample_actual),.id='stage') %>% 
  select(nct,stage,n) %>% spread(stage,n)

completed_sample_sizes %>% count(actual_ony = sum(!is.na(Actual) & is.na(Anticipated)),anticipated_avail=sum(!is.na(Anticipated) & is.na(Actual)),both_avail = sum(!is.na(Anticipated) & !is.na(Actual)))

#join with all id to get missing entries

dat_blandr = completed_sample_sizes %>%  mutate_at(c('Anticipated','Actual'),~as.numeric(.))

dat_blandr %>% summarise(median(Actual,na.rm=T),quantile(Actual,0.25,na.rm=T),quantile(Actual,0.75,na.rm=T))
dat_blandr %>% summarise(median(Anticipated,na.rm=T),quantile(Anticipated,0.25,na.rm=T),quantile(Anticipated,0.75,na.rm=T))


#exclusions
exclude_nct = dat_status_c %>% filter(overall_status=='Suspended/Terminated/Withdrawn') %>% distinct(nct) %>% pull()


#summary completeness

#10.1136/bmjopen-2021-053377
to_plot = filter(dat_blandr,!is.na(Anticipated),!is.na(Actual))

to_plot = to_plot %>% filter(!nct %in% exclude_nct)


to_plot = mutate(to_plot,
                 samplesize_target_log = log(Anticipated+0.1), # add small constant because of zeros
                 samplesize_actual_log = log(Actual+0.1),
                 diff_log = samplesize_actual_log - samplesize_target_log,
                 diff = Actual/Anticipated,
                 diff_perc = 100*(exp(diff)-1), # percent difference
                 aver = (samplesize_actual_log + samplesize_target_log)/2 )
# check
#filter(to_plot, diff< log(0.1)) %>% select(samplesize_target, samplesize_actual) # where sample was 10% of target
# stats
stats = summarise(to_plot, median=median(log(diff)), mean=mean(log(diff)), sd=sd(log(diff))) %>%
  mutate(z = qnorm(0.975),
         lower = exp(mean - z*sd), # limits of agreement
         upper = exp(mean + z*sd)) %>%
  ungroup() %>% mutate_at(c('mean','median'),~exp(.))


# plot
g.theme = theme_bw() + theme(panel.grid.minor = element_blank())
n_exclude = filter(to_plot,diff>10) %>% nrow()
g3 = ggplot(data=filter(to_plot,between(diff,0,10)), aes(x=aver, y=diff))+
  geom_point(size=1.5,alpha=0.7)+
  scale_x_continuous('Average sample size (log scale)', breaks=seq(0,15,2),sec.axis = sec_axis(~exp(.),breaks=c(10,100,500,1000,5000,10000,100000,500000),name='Average sample size (original scale)',labels=scales::comma))  +
  scale_y_continuous('Sample size ratio: Actual/Anticipated',breaks = seq(0,20,1))+
  geom_hline(data=stats, aes(yintercept=mean), linewidth=1, col=cbPalette[7])+
  geom_hline(data=stats, aes(yintercept=lower), linewidth=1,linetype='dashed', col=cbPalette[3])+
  geom_hline(data=stats, aes(yintercept=upper), linewidth=1,linetype='dashed', col=cbPalette[3])+g.theme

png('manuscript/figures/Figure2.png',width=15,height=10,units='in',res=300)
ggarrange(g2,ga,g3,g1,nrow=2,ncol=2,labels=LETTERS[1:4])
invisible(dev.off())

###
# #combined fig
# png('manuscript/figures/Figure2.png',width=8,height=10928*.2,units='in',res=300)
# ggarrange(g2,g1,nrow=2,ncol=1,labels=LETTERS[1:2])
# invisible(dev.off())

#MESH

#summarise by major mesh categories
mesh_descriptor_tree = mesh_descriptor_tree %>% mutate(tree_parent = gsub('\\..*$','',tree_number)) 
mesh_descriptor_tree_c = select(mesh_descriptor_tree,descriptor_name,descriptor_ui,tree_parent,tree_number) %>% group_by(descriptor_name,descriptor_ui) %>% nest(data=c(tree_number,tree_parent))
dat_mesh = included_studies %>% select(NCT,year_posted,mesh_terms) %>% rename('mesh_term'=mesh_terms) %>% 
  mutate_at('mesh_term',~(str_split(.,pattern='\\|'))) %>% unnest(cols='mesh_term') %>%
  left_join(mesh_descriptor_tree_c,by=c('mesh_term'='descriptor_name')) %>%
  mutate_at('mesh_term',~ifelse(.=="",'Missing',.))

#ad classifications
ad = included_studies %>% select(NCT,analysis,outcome)
dat_mesh = dat_mesh %>% left_join(ad,by=c('NCT')) %>% unnest(cols=data)


tab2a = dat_mesh %>% distinct(NCT,tree_parent,analysis) %>% filter(grepl('^C',tree_parent)) %>% count(tree_parent,analysis) %>% mutate_at('analysis',~replace_na(.,'Unclear')) %>%
  group_by(analysis) %>% mutate(perc = 100*n/sum(n)) %>% ungroup() %>% mutate(cell=ifelse(perc>=0.1,paste0(n,' (',roundz(perc,1),'%)'),paste0(n,' (<0.1%)'))) %>% select(tree_parent,analysis,cell) %>% spread(analysis,cell,fill='0 (0.0%)') %>%
  select(tree_parent,Development,Validation,`Development + Validation`,Unclear)

tab2b = dat_mesh %>% distinct(NCT,tree_parent,outcome) %>% filter(grepl('^C',tree_parent)) %>% count(tree_parent,outcome) %>% mutate_at('outcome',~replace_na(.,'Unclear')) %>%
  group_by(outcome) %>% mutate(perc = 100*n/sum(n)) %>% ungroup() %>% mutate(cell=ifelse(perc>=0.1,paste0(n,' (',roundz(perc,1),'%)'),paste0(n,' (<0.1%)'))) %>% select(tree_parent,outcome,cell) %>% spread(outcome,cell,fill='0 (0.0%)') %>%
  select(tree_parent,Diagnostic,Prognostic,`Diagnostic + Prognostic`,Unclear)

tab2 = inner_join(tab2a,tab2b,by='tree_parent') %>% select(-c('Unclear.x','Unclear.y'))

#overall frequency by parent
dat_mesh %>% filter(grepl('^C',tree_parent)) %>% count(tree_parent,sort=T)
mesh_parent = filter(dat_mesh,tree_parent!='Missing') %>% distinct(NCT,tree_parent) %>% count(tree_parent,sort = T)  #%>% slice(1:10)

#here: split up tree numbers to get different levels of details. eg. stroke vs type of stroke
## i will use this code to process the pubmed dataset that 

#by parent - number and year first appeared

to_plot = filter(dat_mesh,grepl('^C',tree_parent)) %>% distinct(NCT,analysis,tree_parent) %>% count(analysis,tree_parent,sort = T)  #%>% slice(1:10)
top10_parent = to_plot %>% group_by(tree_parent) %>% summarise(n=sum(n),.groups='drop') %>% arrange(-n) %>% pull(tree_parent)
to_plot = filter(to_plot,tree_parent %in% top10_parent)
to_plot = to_plot %>% mutate_at('tree_parent',~factor(.,levels=top10_parent))
#add label for tree parent
to_plot = to_plot %>% left_join(select(dat_mesh,tree_number,mesh_term) %>% distinct(),by=c('tree_parent'='tree_number'))
#fix c23, c12
to_plot = to_plot %>% mutate_at('mesh_term',~case_when(tree_parent=='C23'~'Pathological Conditions, Signs and Symptoms',
                                             tree_parent=='C12'~'Urogenital Diseases',
                                             tree_parent=='C17' ~'Skin and Connective Tissue Diseases',
                                             tree_parent=='C20' ~ 'Immune System Diseases',
                                             tree_parent=='C05' ~ 'Musculoskeletal Diseases',
                                             tree_parent=='C15' ~ 'Hemic and Lymphatic Diseases',
                                             tree_parent=='C16' ~ 'Congenital, Hereditary, and Neonatal Diseases and Abnormalities',
                                             tree_parent=='C18' ~ 'Nutritional and Metabolic Diseases',
                                             tree_parent=='C07' ~ 'Stomatognathic Diseases',
                                             tree_parent=='C25' ~ 'Chemically-Induced Disorders',
                                             tree_parent=='C24' ~ 'Occupational Diseases',
                                             !is.na(.)~.)) %>%
  mutate(label = paste0(mesh_term,' (',tree_parent,')') %>% str_wrap(.,40)) %>%
  mutate_at('analysis',~factor(.,levels=c('Development','Validation','Development + Validation','Unclear')))

top10_parent = to_plot %>% group_by(label) %>% summarise(n=sum(n),.groups='drop') %>% arrange(-n) 
to_plot= to_plot %>% mutate_at('label',~factor(.,levels=rev(top10_parent[['label']])))



g4 = ggplot(filter(to_plot,!is.na(analysis)),aes(label,n,fill=analysis)) + geom_col()+scale_x_discrete('',position = "top")+scale_y_reverse('Total times indexed',breaks=seq(0,350,50))+coord_flip()+
  scale_fill_manual(values=cbPalette[4:6])+
  theme_minimal()+theme(panel.grid.minor = element_blank(),strip.background = element_rect(fill='white'),
                        strip.text = element_text(size=12),axis.text.x = element_text(size=12),axis.text.y = element_text(size=10),
                        axis.text = element_text(size=12),legend.position = c(0.25,0.2),legend.title = element_blank(),legend.text = element_text(size=11))

label_order = levels(to_plot$label)
#by outcome type
to_plot = filter(dat_mesh,grepl('^C',tree_parent)) %>% distinct(NCT,outcome,tree_parent) %>% count(outcome,tree_parent,sort = T)  #%>% slice(1:10)
top10_parent = to_plot %>% group_by(tree_parent) %>% summarise(n=sum(n),.groups='drop') %>% arrange(-n) %>% pull(tree_parent)
to_plot = filter(to_plot,tree_parent %in% top10_parent)
to_plot = to_plot %>% mutate_at('tree_parent',~factor(.,levels=top10_parent))
#add label for tree parent
to_plot = to_plot %>% left_join(select(dat_mesh,tree_number,mesh_term) %>% distinct(),by=c('tree_parent'='tree_number'))
#fix c23, c12
to_plot = to_plot %>% mutate_at('mesh_term',~case_when(tree_parent=='C23'~'Pathological Conditions, Signs and Symptoms',
                                                       tree_parent=='C12'~'Urogenital Diseases',
                                                       tree_parent=='C17' ~'Skin and Connective Tissue Diseases',
                                                       tree_parent=='C20' ~ 'Immune System Diseases',
                                                       tree_parent=='C05' ~ 'Musculoskeletal Diseases',
                                                       tree_parent=='C15' ~ 'Hemic and Lymphatic Diseases',
                                                       tree_parent=='C16' ~ 'Congenital, Hereditary, and Neonatal Diseases and Abnormalities',
                                                       tree_parent=='C18' ~ 'Nutritional and Metabolic Diseases',
                                                       tree_parent=='C07' ~ 'Stomatognathic Diseases',
                                                       tree_parent=='C25' ~ 'Chemically-Induced Disorders',
                                                       tree_parent=='C24' ~ 'Occupational Diseases',
                                                       !is.na(.)~.)) %>%
  mutate(label = paste0(mesh_term,' (',tree_parent,')') %>% str_wrap(.,40)) %>%
  mutate_at('label',~factor(.,levels=rev(unique(label_order)))) %>% mutate_at('outcome',~factor(.,levels=c('Diagnostic','Prognostic','Diagnostic + Prognostic','Unclear')))

top10_parent = to_plot %>% group_by(label) %>% summarise(n=sum(n),.groups='drop') %>% arrange(-n) 
to_plot= to_plot %>% mutate_at('label',~factor(.,levels=rev(top10_parent[['label']])))


g5 = ggplot(filter(to_plot,!is.na(outcome)),aes(label,n,fill=outcome)) + geom_col()+scale_x_discrete('',position = 'top')+scale_y_continuous('Total times indexed',breaks=seq(0,350,50))+coord_flip()+
  scale_fill_manual(values=cbPalette[1:3])+
  theme_minimal()+theme(panel.grid.minor = element_blank(),
                        strip.background = element_rect(fill='white'),
                        strip.text = element_text(size=12),axis.text.x = element_text(size=12),axis.text.y = element_text(size=10),
                        axis.text = element_text(size=12),legend.position = c(0.75,0.2),legend.title = element_blank(),legend.text = element_text(size=11))


png('manuscript/figures/Figure3A.png',height=6,width=12,units='in',res=300)
ggarrange(g4+theme(plot.margin=unit(c(1,0,0.5,0.1),"cm")),g5+theme(axis.text.y=element_blank(),plot.margin=unit(c(1,0.2,0.1,-0.5),"cm")),nrow=1,ncol=2,widths=c(0.42,0.25),align='h',labels=c("A",""))
invisible(dev.off())



#as a word cloud
to_plot = filter(dat_mesh,mesh_term!='Missing') %>% distinct(NCT,mesh_term,outcome) %>% count(mesh_term,outcome,sort = T)
lab_order = distinct(to_plot ,mesh_term) %>% pull(mesh_term)
to_plot = to_plot %>% mutate_at('mesh_term',~factor(.,levels=rev(lab_order)))

to_plot = to_plot %>% mutate_at('mesh_term',~as.character(.))

require(ggwordcloud)

# ggplot(filter(to_plot,n>1), aes(label = mesh_term, size = n)) +
#   geom_text_wordcloud(rm_outside = TRUE) +
#   scale_radius(range = c(0, 5), limits = c(0, NA)) +
#   theme_minimal()

g7d=ggplot(filter(to_plot,n>=2,outcome %in% c('Diagnostic')), aes(label = mesh_term, size = n)) +
  geom_text_wordcloud_area(rm_outside = TRUE,eccentricity = 1) +
  scale_size_area(max_size = 10) +
  theme_minimal()

g7p=ggplot(filter(to_plot,n>=2,outcome %in% c('Prognostic')), aes(label = mesh_term, size = n)) +
  geom_text_wordcloud_area(rm_outside = TRUE,eccentricity = 1) +
  scale_size_area(max_size = 10) +
  theme_minimal()


# g8 = ggplot(filter(to_plot,n>=5,mesh_term!='COVID-19'), aes(label = mesh_term, size = n)) +
#   geom_text_wordcloud_area(rm_outside = TRUE,eccentricity = 1) +
#   scale_size_area(max_size = 10) +
#   theme_minimal()
png('manuscript/figures/Figure3B.png',height=5,width=12,units='in',res=300)
ggarrange(g7p,g7d,nrow=1,ncol=2,labels=LETTERS[2:3])
invisible(dev.off())

dat_mesh_children = dat_mesh %>% select(NCT,year_posted,analysis,outcome,descriptor_ui,tree_number) %>%
  mutate(tree_child_1 = str_extract_all(tree_number,pattern='^[A-Z]\\d+\\.\\d+',simplify=T)[,1],
         tree_child_2 = str_extract_all(tree_number,pattern='^[A-Z]\\d+\\.\\d+\\.\\d+',simplify=T)[,1],
         tree_child_3 = str_extract_all(tree_number,pattern='^[A-Z]\\d+\\.\\d+\\.\\d+\\.\\d+',simplify=T)[,1],
         tree_child_4 = str_extract_all(tree_number,pattern='^[A-Z]\\d+\\.\\d+\\.\\d+\\.\\d+\\.\\d+',simplify=T)[,1],
         tree_child_5 = str_extract_all(tree_number,pattern='^[A-Z]\\d+\\.\\d+\\.\\d+\\.\\d+\\.\\d+\\.\\d+',simplify=T)[,1])


dat_mesh_children_c = dat_mesh_children %>% select(NCT,year_posted,analysis,outcome,starts_with('tree_child')) %>% pivot_longer(cols=-c(NCT,year_posted,analysis,outcome),values_to = 'tree_number',names_to='child') %>% filter(tree_number!="") %>% 
  mutate_at('child',~str_remove_all(.,'tree_child_')) %>%
  left_join(mesh_descriptor_tree,by='tree_number') %>% distinct() %>%
  select(NCT,year_posted,analysis,outcome,descriptor_ui,descriptor_name,tree_parent) %>% 
  group_by(NCT,year_posted,analysis,outcome,descriptor_ui,descriptor_name) %>% summarise(major_disease_concept=str_c(unique(tree_parent),collapse=';'),.groups='drop') %>%
  mutate_at(c('analysis','outcome'),~replace_na(.,'Unclear'))

openxlsx::write.xlsx(dat_mesh_children_c,file='manuscript/supplement/SFile2.xlsx')

# 
# #not run
# #upset plot
# to_upset = filter(dat_mesh,mesh_term!='Missing') %>% distinct(NCT,mesh_term) %>% group_by(NCT) %>% summarise(keywords=list(mesh_term),.groups='drop')
# to_upset %>% distinct(NCT,.keep_all = T) %>% ggplot(aes(x=keywords)) + geom_bar(fill='darkgrey',colour='black') + scale_x_upset(n_intersections = 20) 
# 
# ftab_mesh  = filter(dat_mesh,mesh_term!='Missing') %>% distinct(NCT,tree_parent,mesh_term) %>% count(mesh_term,tree_parent) %>%
#   group_by(mesh_term) %>% summarise('Disease group(s)' = paste(tree_parent,collapse=';'))
# 
# ad =  filter(dat_mesh,mesh_term!='Missing') %>% distinct(NCT,mesh_term) %>% count(mesh_term)
# 
# ftab_mesh = ftab_mesh %>% full_join(ad,by='mesh_term') %>% arrange(-n) %>% rename('MeSH term'=mesh_term)
# 
# ftab_mesh_1 = filter(dat_mesh,mesh_term!='Missing') %>% distinct(NCT,tree_child_2) %>% count(tree_child_2,sort = T) %>% filter(tree_child_2!="") %>%
#   mutate(tree_parent = str_extract_all(tree_child_2,pattern='^[A-Z]\\d+',simplify=T)[,1],tree_child_1=str_extract_all(tree_child_2,pattern='^[A-Z]\\d+\\.\\d+',simplify=T)[,1]) %>% select(tree_parent,tree_child_1,tree_child_2,n) %>% arrange(tree_child_1,-n)
# 
# ftab_mesh_1 = left_join(ftab_mesh_1,select(mesh_descriptor_tree,tree_number,descriptor_name),by=c('tree_parent'='tree_number')) %>% rename('descriptor_parent'=descriptor_name) %>%
#   left_join(select(mesh_descriptor_tree,tree_number,descriptor_name),by=c('tree_child_1'='tree_number')) %>% rename('descriptor_child_1'=descriptor_name) %>%
#   left_join(select(mesh_descriptor_tree,tree_number,descriptor_name),by=c('tree_child_2'='tree_number')) %>% rename('descriptor_child_2'=descriptor_name) %>% select(tree_parent,tree_child_1,tree_child_2,descriptor_parent,descriptor_child_1,descriptor_child_2,n) %>% arrange(tree_parent,tree_child_1,-n)
#   
# write.xlsx(list(ftab_mesh,ftab_mesh_1),file='manuscript/tables/mesh term summary.xlsx')
# 
