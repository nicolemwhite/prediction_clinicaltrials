#4C_mesh_publication.R
source('99_packages.R')
source('99_functions.R')

load('data/publications/times_to_publication_20231204.rda')
#exclude duplicate NCT
dat_dup = openxlsx::read.xlsx('data/manual checks/possible duplicates-20230713.xlsx')
exclude_dup = dat_dup %>% filter(duplicate=='y') %>% pull(b) %>% unique()

dat_included = filter(dat_included,!NCT %in% exclude_dup)
dat_included = dat_included %>% mutate(year_posted=year(study_posted),year_study_start=year(study_start))

load('data/mesh/mesh_term_info.rda')


mesh_descriptor_tree = mesh_descriptor_tree %>% mutate(tree_parent = gsub('\\..*$','',tree_number)) 
mesh_descriptor_tree_c = select(mesh_descriptor_tree,descriptor_name,descriptor_ui,tree_parent,tree_number) %>% group_by(descriptor_name,descriptor_ui) %>% nest(data=c(tree_number,tree_parent))
dat_mesh = dat_included %>% select(NCT,year_posted,year_study_start,mesh_terms) %>% rename('mesh_term'=mesh_terms) %>% 
  mutate_at('mesh_term',~(str_split(.,pattern='\\|'))) %>% unnest(cols='mesh_term') %>%
  left_join(mesh_descriptor_tree_c,by=c('mesh_term'='descriptor_name')) %>%
  mutate_at('mesh_term',~ifelse(.=="",'Missing',.))

ad = dat_included %>% select(NCT,analysis,outcome)
dat_mesh = dat_mesh %>% right_join(ad,by=c('NCT')) %>% unnest(cols=data)

top20_overall = dat_mesh %>% mutate(n=nchar(tree_number)) %>% group_by(NCT) %>% slice_max(n) %>% ungroup() %>% distinct(NCT,mesh_term) %>% count(mesh_term,sort=T) %>% slice(1:20) %>% pull(mesh_term)
#examples by disease group
top10_c23 = filter(dat_mesh,tree_parent=='C23') %>% distinct(NCT,mesh_term,analysis)  %>% count(mesh_term,sort=T) %>% slice(1:10) %>% pull(mesh_term)
top10_c04 = filter(dat_mesh,tree_parent=='C04') %>% distinct(NCT,mesh_term,analysis)  %>% count(mesh_term,sort=T) %>% slice(1:10) %>% pull(mesh_term)
top10_c14 = filter(dat_mesh,tree_parent=='C14') %>% distinct(NCT,mesh_term,analysis)  %>% count(mesh_term,sort=T) %>% slice(1:10) %>% pull(mesh_term)

#join with pub outcome data, including 
dat_mesh_pubs = dat_mesh %>% filter(mesh_term %in% top20_overall) %>% distinct(NCT,mesh_term) %>% left_join(select(dat_pubs_2,NCT,pub_outcome,source),by='NCT')
dat_mesh_pubs %>% group_by(mesh_term) %>% summarise(n=length(unique(NCT)),npubs_matched=sum(pub_outcome==1),ppubs=100*mean(pub_outcome==1)) %>% arrange(-n)

#take longer tree number
dat_mesh %>% mutate(n=nchar(tree_number)) %>% group_by(NCT) %>% slice_max(n) %>% ungroup() %>% distinct(NCT,mesh_term) %>% count(mesh_term,sort=T)
