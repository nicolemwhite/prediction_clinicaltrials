#1_summarise_included_studies.R
source('99_packages.R')
source('99_functions.R')
load('data/final_studies.rda')
load('data/mesh/mesh_term_info.rda')

g1 = included_studies %>% distinct(NCT,.keep_all = T) %>% ggplot(aes(x=Keyword)) + geom_bar(fill='darkgrey',colour='black') + scale_x_upset() + 
  theme_minimal()+scale_y_continuous('Number of included records',breaks=seq(0,250,by=25)) + theme(panel.grid.minor = element_blank())

png('manuscript/figures/included_records_tags.png',width=8,height=6,units='in',res=300)
g1
invisible(dev.off())


#plot screening record decisions over time. need to join with study year

g2 = ggplot(dat_included,aes(year_posted))+geom_histogram(aes(y=cumsum(..count..)),binwidth=1,fill='darkgrey',colour='black')+
  scale_x_continuous('Year first posted',breaks=seq(2000,2022,2))+
  scale_y_continuous('Cumulative number of included records',breaks=seq(0,1000,100))+
  theme_minimal()+theme(panel.grid.minor = element_blank(),axis.text = element_text(size=12),text=element_text(size=12))

png('manuscript/figures/incuded_records.png',width=1000,height=600)
g2
invisible(dev.off())


#split into prognostic vs diagnostic, development vs validation
dat_classification = filter(screening_results,final_decision=='Include') %>% mutate(grp = case_when(
  (Development==TRUE & Validation==FALSE) ~ 'Development',
  (Development==FALSE & Validation==TRUE) ~ 'Validation',
  (Development==TRUE & Validation==TRUE) ~ 'Development + Validation'),
  Outcome = case_when(
    (Prognostic==TRUE & Diagnostic==FALSE) ~ 'Prognostic',
    (Prognostic==FALSE & Diagnostic==TRUE) ~ 'Diagnostic',
    (Prognostic==TRUE & Diagnostic==TRUE) ~ 'Diagnostic + Prognostic')) %>% left_join(select(dat_included,id,year_posted),by=c('NCT'='id'))

all_years = tibble(Outcome = rep(c('Prognostic','Diagnostic','Diagnostic + Prognostic'),each=3),grp=rep(c('Development','Validation','Development + Validation'),3),year_posted=list(2000:2022)) %>% unnest(cols='year_posted')

ftab_outcome = dat_classification %>% count(Outcome,year_posted,grp)
ftab_outcome = right_join(ftab_outcome,all_years,by=c('Outcome','grp','year_posted')) %>% mutate_at('n',~replace_na(.,0)) %>%  mutate_at('grp',~factor(.,levels=c('Development','Validation','Development + Validation'))) %>% mutate_at('Outcome',~factor(.,levels=c('Diagnostic','Prognostic','Diagnostic + Prognostic'))) 
ftab_outcome = ftab_outcome %>% arrange(Outcome,grp,year_posted) %>% group_by(Outcome,grp) %>% mutate(n_cusum = cumsum(n)) %>% ungroup() %>% rename('Study type'=grp)

g3 = ftab_outcome %>% ggplot(aes(x=year_posted,y=n_cusum,colour=`Study type`,linetype=Outcome))+geom_point(size=2)+geom_line(size=0.75)+
  scale_x_continuous('Year first posted',breaks=seq(2000,2022,2))+scale_y_continuous('Cumulative number of included records',breaks=seq(0,650,50))+
  scale_colour_manual(values=cbPalette)+
  theme_minimal()+theme(strip.background = element_rect(fill='white'),
                        strip.text = element_text(size=12),axis.text.x = element_text(size=12),axis.text.y = element_text(size=12),
                        axis.text = element_text(size=12),legend.position = c(0.2,0.75),legend.title = element_blank(),legend.text = element_text(size=11))

png('manuscript/figures/included_records_outcome_studytype.png',width=8,height=6,units='in',res=300)
g3
invisible(dev.off())

#summarise by major mesh categories
mesh_descriptor_tree = mesh_descriptor_tree %>% mutate(tree_parent = gsub('\\..*$','',tree_number)) 
dat_mesh = dat_included %>% select(id,year_posted,mesh_terms) %>% rename('mesh_term'=mesh_terms) %>% 
  mutate_at('mesh_term',~(str_split(.,pattern='\\|'))) %>% unnest(cols='mesh_term') %>%
  left_join(select(mesh_descriptor_tree,descriptor_name,tree_parent,tree_number),by=c('mesh_term'='descriptor_name')) %>%
  mutate_at('mesh_term',~ifelse(.=="",'Missing',.)) %>% mutate_at(c('tree_parent','tree_number'),~replace_na(.,'Missing'))

#ad classifications
ad = dat_classification %>% select(NCT,grp,Outcome)
dat_mesh = dat_mesh %>% left_join(ad,by=c('id'='NCT'))

#by parent - number and year first appeared

to_plot = filter(dat_mesh,tree_parent!='Missing') %>% distinct(id,grp,tree_parent) %>% count(grp,tree_parent,sort = T)  #%>% slice(1:10)
top10_parent = to_plot %>% group_by(tree_parent) %>% summarise(n=sum(n),.groups='drop') %>% arrange(-n) %>% slice(1:10) %>% pull(tree_parent)

to_plot = filter(to_plot,tree_parent %in% top10_parent)

to_plot = to_plot %>% mutate_at('tree_parent',~factor(.,levels=top10_parent))
#add label for tree parent
to_plot = to_plot %>% left_join(select(dat_mesh,tree_number,mesh_term) %>% distinct(),by=c('tree_parent'='tree_number'))
#fix c23, c12
to_plot = to_plot %>% mutate_at('mesh_term',~case_when(tree_parent=='C23'~'Pathological Conditions, Signs and Symptoms',
                                             tree_parent=='C12'~'Urogenital Diseases',
                                             !is.na(.)~.)) %>%
  mutate(label = paste0(mesh_term,' (',tree_parent,')') %>% str_wrap(.,20)) %>%
  mutate_at('label',~factor(.,levels=rev(unique(.))))

g4 = ggplot(to_plot,aes(label,n,fill=grp)) + geom_col()+scale_x_discrete('')+scale_y_continuous('Total times indexed',breaks=seq(0,350,50))+coord_flip()+
  scale_fill_manual(values=cbPalette)+
  theme_minimal()+theme(strip.background = element_rect(fill='white'),
                        strip.text = element_text(size=12),axis.text.x = element_text(size=12),axis.text.y = element_text(size=10),
                        axis.text = element_text(size=12),legend.position = c(0.75,0.2),legend.title = element_blank(),legend.text = element_text(size=11))


#combined fig
png('manuscript/figures/Figure2.png',width=15,height=10,units='in',res=300)
ggarrange(g2,g1,g3,g4,nrow=2,ncol=2,labels=LETTERS[1:4])
invisible(dev.off())

#now by mesh_term
to_plot = filter(dat_mesh,mesh_term!='Missing') %>% distinct(id,mesh_term) %>% count(mesh_term,sort = T) %>% slice(1:50)
lab_order = to_plot %>% pull(mesh_term)
to_plot = to_plot %>% mutate_at('mesh_term',~factor(.,levels=rev(lab_order)))

g5 = ggplot(to_plot,aes(mesh_term,n)) + geom_col(fill='darkgrey',colour='black')+scale_x_discrete('')+scale_y_continuous('Number of included records',breaks=seq(0,50,5))+coord_flip()+
  theme_minimal()+theme(strip.background = element_rect(fill='white'),
                        strip.text = element_text(size=12),axis.text.x = element_text(size=12),axis.text.y = element_text(size=12),
                        axis.text = element_text(size=12))


png('manuscript/figures/Figure3.png',height=10,width=10,units='in',res=300)
g5
invisible(dev.off())
