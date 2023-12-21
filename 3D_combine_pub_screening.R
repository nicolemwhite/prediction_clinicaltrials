#3D_combine_pub_screening.R
source("99_packages.R")
source("99_functions.R")


pubs_nct = read.xlsx('data/publications/done/nct_linked_publications_done.xlsx') %>% mutate_at('pub_date',~ymd(.))
pubs_ct = read.xlsx('data/publications/done/clintrial_reported_publications_done.xlsx',detectDates = T) 
pubs_other = read.xlsx('data/publications/done/unlinked_publications_done.xlsx',detectDates = T) %>% mutate_at('match',~ifelse(.==1,2,.)) %>% mutate_at('PMID',~as.numeric(.))
dat_pubs = bind_rows(list(nct=pubs_nct,ct=pubs_ct,other=pubs_other),.id='stage') 


#examples of linked pubs
set.seed(57892)
out = bind_rows(pubs_nct,pubs_ct,.id='source') %>% filter(match==1) %>% sample_n(10) %>% select(NCT_num,PMID,title,source) %>% mutate_at('source',~case_when(.==1 ~ 'NCT-linked',.==2 ~ 'Investigator submitted'))

write.xlsx(out,file='manuscript/supplement/SFile1.xlsx')

# load included studies and scores from matching algorithm
dat_dup = openxlsx::read.xlsx('data/manual checks/possible duplicates-20230713.xlsx')
exclude_dup = dat_dup %>% filter(duplicate=='y') %>% pull(b) %>% unique()

#add study start dates
load('data/clin_study_dates.rda')
ct_start_dates = filter(study_start_dates,NCT %in% dat_pubs[['NCT_num']]) %>% select(NCT,study_start)

dat_pubs = inner_join(select(dat_pubs,NCT_num,PMID,pub_date,match,reject_reason,stage),ct_start_dates,by=c('NCT_num'='NCT')) %>% mutate(months_to_publication = as.numeric(pub_date - study_start)*12/365) %>% filter(months_to_publication>0)

#replace 2 with 1 - confirm is correct
dat_pubs = dat_pubs %>% mutate_at('match',~pmin(1,.))

load('data/final_studies_with_meta_R1.rda') 
dat_included = filter(decisions,final_decision=='include'& !NCT %in% exclude_dup) %>% select(NCT:Validation,overall_status)
dat_pubs = filter(dat_pubs,!NCT_num %in% exclude_dup)

dat_pubs = left_join(dat_included,dat_pubs,by=c('NCT'='NCT_num'))

write.csv(select(dat_pubs,NCT) %>% distinct(),file='data/final_included_studies.csv',row.names = F)

# #up to 10 years
# dat_pubs_1 <- filter(dat_pubs,match==1) %>% distinct(NCT,study_start,pub_date,stage,PMID,months_to_publication) %>% mutate(flag = case_when(stage=='nct'~1,stage=='ct'~2,stage=='other'~3)) %>% group_by(NCT)  %>% slice_min(months_to_publication) %>% slice_min(flag) %>% ungroup()
# 
# res = anti_join(dat_included,dat_pubs_1,by='NCT') %>% select(NCT) %>% left_join(ct_start_dates,by='NCT') %>% add_column(flag=0)
# 
# dat_pubs_1 = bind_rows(list('NCT-linked'=dat_pubs_1,'ML classifier'=res))
# 
# censor_month = 120
# last_updated = as.Date('2022-11-01')
# dat_pubs_1 = dat_pubs_1 %>% mutate_at('months_to_publication',~replace_na(.,censor_month)) %>%
#   mutate(
#     max_time = pmin(censor_month,as.numeric(last_updated-study_start)*12/365), #take 10 years since study start or up to 22nov 2022, whichever happens first
#     status_1 = ifelse(flag %in% 1:2 & months_to_publication<=max_time,1,0),
#     status_2 = ifelse(flag %in% 1:3 & months_to_publication<=max_time,1,0),
#     event_time_c = pmin(max_time,months_to_publication)) %>% left_join(dat_included,by='NCT') %>% mutate_at(vars(Development:Validation),~replace_na(.,FALSE)) %>%
#   mutate(analysis = case_when(
#     (Development==TRUE & Validation==FALSE) ~ 'Development',
#     (Development==FALSE & Validation==TRUE) ~ 'Validation',
#     (Development==TRUE & Validation==TRUE) ~ 'Development + Validation'),
#     outcome = case_when(
#       (Prognostic==TRUE & Diagnostic==FALSE) ~ 'Prognostic',
#       (Prognostic==FALSE & Diagnostic==TRUE) ~ 'Diagnostic',
#       (Prognostic==TRUE & Diagnostic==TRUE) ~ 'Diagnostic + Prognostic'))
# 
# 
# 
# 
# save(dat_pubs_1,file='data/matched_publications.rda')
# 
# cuminc_fit =with(dat_pubs_1,cuminc(ftime=event_time_c,fstatus=status_1,cencode=0))
# 
# #linked only
# gdata_1 = ggcompetingrisks(cuminc_fit,conf.int = T)$data %>% mutate_at('name',~str_remove_all(.,' 1')) %>% distinct()
# #linked + ML
# cuminc_fit =with(dat_pubs_1,cuminc(ftime=event_time_c,fstatus=status_2,cencode=0))
# gdata_2 = ggcompetingrisks(cuminc_fit,conf.int = T)$data %>% mutate_at('name',~str_remove_all(.,' 1')) %>% distinct()
# 
# g1 = ggplot(gdata_2,aes(x=time,y=est,ymin=est-1.96*std,ymax=est+1.96*std))+geom_path()+geom_ribbon(alpha=.2)+
#   expand_limits(y=0.4)+
#   scale_x_continuous('Years since study start',breaks=seq(0,120,12),labels=0:10,expand = c(0, 0), limits = c(0, NA))+
#   scale_y_continuous('Cumulative incidence,\ntime to first matching publication',breaks=seq(0,0.4,0.05),expand = c(0, 0), limits = c(0, NA))+theme_bw()+
#   theme(legend.background = element_blank(),text=element_text(size=12),axis.text=element_text(size=12),panel.grid.minor = element_blank(),plot.margin = margin(1,1,1,1, "cm"))
# 
# 
# dat_pubs_1 = dat_pubs_1 %>% mutate(strat = case_when(between(year(study_start),1990,2010)~'pre-2010 (n = 65)',
#                                                      between(year(study_start),2011,2015)~'2011 to 2015 (n = 126)',
#                                                      between(year(study_start),2016,2019) ~ '2016 to 2019 (n = 319)',
#                                                      TRUE ~ '2020 to 2022 (n = 403)'))
# 
# 
# 
# cuminc_fit_strat =with(dat_pubs_1,cuminc(ftime=event_time_c,fstatus=status_2,cencode=0,group=strat))
# gdata_strat = ggcompetingrisks(cuminc_fit_strat,conf.int = T)$data %>% mutate_at('name',~str_remove_all(.,' 1$')) %>% distinct() %>%
#   mutate_at('name',~factor(.,levels=c('pre-2010 (n = 65)','2011 to 2015 (n = 126)','2016 to 2019 (n = 319)','2020 to 2022 (n = 403)')))
# 
# # #make lines stop after 3 years for 2020 to 2022 and after 7 years for 2016 to 2019
# # gdata_strat = gdata_strat %>% mutate(exclude = case_when(
# #   name=='2016 to 2019 (n = 319)' & time > 84 ~ 1,
# #   name=='2020 to 2022 (n = 403)' & time > 36 ~ 1,
# #   TRUE ~ 0)) %>% filter(exclude==0)
# 
# g2 = ggplot(gdata_strat,aes(x=time,y=est,group=name,colour=name,fill=name))+geom_path(linewidth=1)+
#   expand_limits(y=0.4)+
#   scale_x_continuous('Years since study start',breaks=seq(0,120,12),labels=0:10,expand = c(0, 0), limits = c(0, NA))+
#   scale_y_continuous('Cumulative incidence,\ntime to first relevant publication',breaks=seq(0,0.4,0.05),expand = c(0, 0), limits = c(0, NA))+theme_bw()+
#   theme(legend.background = element_blank(),legend.position = c(0.3,0.75),legend.title=element_blank(),legend.text=element_text(size=12),text=element_text(size=12),axis.text=element_text(size=12),panel.grid.minor = element_blank(),
#         plot.margin = margin(1,1,1,1, "cm"))
# 
# 
# png('manuscript/figures/Figure4.png',width=8,height=8,units='in',res=300)
# ggarrange(g1,g2,nrow=2,ncol=1,labels=LETTERS[1:2])
# dev.off()
# 
# #censoring figure for the supplement
# cuminc_fit_cens =with(dat_pubs_1,cuminc(ftime=event_time_c,fstatus=status_2,cencode=1))
# gdata_cens = ggcompetingrisks(cuminc_fit_cens,conf.int = T)$data %>% mutate_at('name',~str_remove_all(.,' 0')) %>% distinct()
# 
# g3 = ggplot(gdata_cens,aes(x=time,y=est,ymin=est-1.96*std,ymax=est+1.96*std))+geom_path()+geom_ribbon(alpha=.2)+
#   expand_limits(y=1)+
#   scale_x_continuous('Years since study start',breaks=seq(0,120,12),labels=0:10,expand = c(0, 0), limits = c(0, NA))+
#   scale_y_continuous('Cumulative incidence of censoring',breaks=seq(0,1,0.1),expand = c(0, 0), limits = c(0, NA))+theme_bw()+
#   theme(legend.background = element_blank(),text=element_text(size=12),axis.text=element_text(size=12),panel.grid.minor = element_blank(),plot.margin = margin(1,1,1,1, "cm"))
# 
# 
# png('manuscript/supplement/FigureS7.png',width=8,height=8,units='in',res=300)
# g3
# dev.off()
# 
# ##
# gdata = bind_rows(list('NCT-linked + investigator-submitted'=gdata_1,'NCT-linked + investigator-submitted + ML classifier'=gdata_2),.id='group')
# 
# 
# gall = ggplot(gdata,aes(x=time,y=est,ymin=est-1.96*std,ymax=est+1.96*std,group=group,colour=group,fill=group))+geom_path()+geom_ribbon(alpha=.2)+
#   expand_limits(y=0.4)+
#   scale_x_continuous('Years since study start',breaks=seq(0,120,12),labels=0:10,expand = c(0, 0), limits = c(0, NA))+
#   scale_y_continuous('Cumulative incidence,\ntime to first matching publication',breaks=seq(0,0.4,0.05),expand = c(0, 0), limits = c(0, NA))+theme_bw()+
#   theme(legend.background = element_blank(),legend.position = c(0.35,0.85),legend.title=element_blank(),legend.text=element_text(size=12),text=element_text(size=12),axis.text=element_text(size=12),panel.grid.minor = element_blank(),plot.margin = margin(1,1.5,1,1, "cm"))
# png('manuscript/supplement/FigureS3.png',width=10,height=8,units='in',res=300)
# gall
# dev.off()
# 
# #
# #
# # #pre2020
# # cuminc_fit =with(filter(dat_pubs_1,year(study_start)<2020),cuminc(ftime=event_time_c,fstatus=status_1,cencode=0))
# # gdata_1 = ggcompetingrisks(cuminc_fit,conf.int = T)$data %>% mutate_at('name',~str_remove_all(.,' 1')) %>% distinct()
# # cuminc_fit =with(filter(dat_pubs_1,year(study_start)<2020),cuminc(ftime=event_time_c,fstatus=status_2,cencode=0))
# # gdata_2 = ggcompetingrisks(cuminc_fit,conf.int = T)$data %>% mutate_at('name',~str_remove_all(.,' 1')) %>% distinct()
# # gdata = bind_rows(list('NCT-linked + investigator-submitted'=gdata_1,'NCT-linked + investigator-submitted + ML classifier'=gdata_2),.id='group')
# #
# # g2020 = ggplot(gdata,aes(x=time,y=est,ymin=est-1.96*std,ymax=est+1.96*std,group=group,colour=group,fill=group))+geom_path()+geom_ribbon(alpha=.2)+
# #   expand_limits(y=0.3)+
# #   scale_x_continuous('Years since study start',breaks=seq(0,120,12),labels=0:10,expand = c(0, 0), limits = c(0, NA))+
# #   scale_y_continuous('Cumulative incidence\ntime to first relevant publication',breaks=seq(0,0.3,0.05),expand = c(0, 0), limits = c(0, NA))+theme_bw()+
# #   theme(legend.background = element_blank(),legend.title=element_blank(),legend.text=element_text(size=12),text=element_text(size=12),axis.text=element_text(size=12),panel.grid.minor = element_blank(),
# #         plot.margin = margin(1,1.5,1,1, "cm"))
# #
# #
# # #pre-2017
# # cuminc_fit =with(filter(dat_pubs_1,year(study_start)<2017),cuminc(ftime=event_time_c,fstatus=status_1,cencode=0))
# # gdata_1 = ggcompetingrisks(cuminc_fit,conf.int = T)$data %>% mutate_at('name',~str_remove_all(.,' 1')) %>% distinct()
# # cuminc_fit =with(filter(dat_pubs_1,year(study_start)<2017),cuminc(ftime=event_time_c,fstatus=status_2,cencode=0))
# # gdata_2 = ggcompetingrisks(cuminc_fit,conf.int = T)$data %>% mutate_at('name',~str_remove_all(.,' 1')) %>% distinct()
# # gdata = bind_rows(list('NCT-linked + investigator-submitted'=gdata_1,'NCT-linked + investigator-submitted + ML classifier'=gdata_2),.id='group')
# #
# #
# # g2017 = ggplot(gdata,aes(x=time,y=est,ymin=est-1.96*std,ymax=est+1.96*std,group=group,colour=group,fill=group))+geom_path()+geom_ribbon(alpha=.2)+
# #   expand_limits(y=0.3)+
# #   scale_x_continuous('Years since study start',breaks=seq(0,120,12),labels=0:10,expand = c(0, 0), limits = c(0, NA))+
# #   scale_y_continuous('Cumulative incidence\ntime to first relevant publication',breaks=seq(0,0.3,0.05),expand = c(0, 0), limits = c(0, NA))+theme_bw()+
# #   theme(legend.background = element_blank(),legend.title=element_blank(),legend.text=element_text(size=12),text=element_text(size=12),axis.text=element_text(size=12),panel.grid.minor = element_blank(),
# #         plot.margin = margin(1,1.5,1,1, "cm"))
# #
# 
# # png('manuscript/supplement/FigureS3.png',width=8,height=12,units='in',res=300)
# # ggarrange(gall,g2017,g2020,nrow=3,ncol=1,labels=c('All records','2000 to 2017','2000 to 2020'),common.legend = T,legend='bottom')
# # dev.off()
# 
# 
# ggplot(gdata,aes(x=time,y=est,ymin=est-1.96*std,ymax=est+1.96*std,group=name,colour=name,fill=name))+geom_path()+geom_ribbon(alpha=.2)+
#   expand_limits(y=0.3)+
#   scale_x_continuous('Years since study start',breaks=seq(0,120,12),labels=0:10,expand = c(0, 0), limits = c(0, NA))+
#   scale_y_continuous('Cumulative incidence, time to first relevant publication',breaks=seq(0,0.5,0.05),expand = c(0, 0), limits = c(0, NA))+theme_bw()+
#   theme(legend.background = element_blank(),legend.position = c(0.2,0.85),legend.title=element_blank(),legend.text=element_text(size=12),text=element_text(size=12),axis.text=element_text(size=12),panel.grid.minor = element_blank(),
#         plot.margin = margin(1,1.5,1,1, "cm"))
# 
# 
# #study type
# 
# cuminc_fit_d =with(filter(dat_pubs_1,Development==TRUE),cuminc(ftime=event_time_c,fstatus=status_2,cencode=0))
# cuminc_fit_dv =with(filter(dat_pubs_1,Development==TRUE,Validation==TRUE),cuminc(ftime=event_time_c,fstatus=status_2,cencode=0))
# cuminc_fit_v =with(filter(dat_pubs_1,Development==FALSE,Validation==TRUE),cuminc(ftime=event_time_c,fstatus=status_2,cencode=0))
# 
# 
# gdata_d = ggcompetingrisks(cuminc_fit_d,conf.int = T)$data %>% mutate_at('name',~str_remove_all(.,' 1')) %>% distinct()
# gdata_dv = ggcompetingrisks(cuminc_fit_dv,conf.int = T)$data %>% mutate_at('name',~str_remove_all(.,' 1')) %>% distinct()
# gdata_v = ggcompetingrisks(cuminc_fit_v,conf.int = T)$data %>% mutate_at('name',~str_remove_all(.,' 1')) %>% distinct()
# 
# gdata = bind_rows(list('Development only'=gdata_d,'Development + Validation'=gdata_dv,'Validation only'=gdata_v),.id='group')
# 
# png('manuscript/supplement/FigureS4.png',width=8,height=8,units='in',res=300)
# ggplot(gdata,aes(x=time,y=est,ymin=est-1.96*std,ymax=est+1.96*std,group=group,colour=group,fill=group))+geom_path()+geom_ribbon(alpha=.2)+
#   expand_limits(y=0.55)+
#   scale_x_continuous('Years since study start',breaks=seq(0,120,12),labels=0:10,expand = c(0, 0), limits = c(0, NA))+
#   scale_y_continuous('Cumulative incidence,\ntime to first relevant publication',breaks=seq(0,0.55,0.05),expand = c(0, 0), limits = c(0, NA))+theme_bw()+
#   theme(legend.background = element_blank(),legend.position = c(0.2,0.85),legend.title=element_blank(),legend.text=element_text(size=12),text=element_text(size=12),axis.text=element_text(size=12),panel.grid.minor = element_blank(),
#         plot.margin = margin(1,1.5,1,1, "cm"))
# dev.off()
# 
# 
# #model outcome
# 
# cuminc_fit_d =with(filter(dat_pubs_1,Diagnostic==TRUE),cuminc(ftime=event_time_c,fstatus=status_2,cencode=0))
# cuminc_fit_p =with(filter(dat_pubs_1,Prognostic==TRUE),cuminc(ftime=event_time_c,fstatus=status_2,cencode=0))
# 
# 
# gdata_d = ggcompetingrisks(cuminc_fit_d,conf.int = T)$data %>% mutate_at('name',~str_remove_all(.,' 1')) %>% distinct()
# gdata_p = ggcompetingrisks(cuminc_fit_p,conf.int = T)$data %>% mutate_at('name',~str_remove_all(.,' 1')) %>% distinct()
# 
# gdata = bind_rows(list('Diangostic'=gdata_d,'Prognostic'=gdata_p),.id='group')
# 
# png('manuscript/supplement/FigureS5.png',width=8,height=8,units='in',res=300)
# ggplot(gdata,aes(x=time,y=est,ymin=est-1.96*std,ymax=est+1.96*std,group=group,colour=group,fill=group))+geom_path()+geom_ribbon(alpha=.2)+
#   expand_limits(y=0.45)+
#   scale_x_continuous('Years since study start',breaks=seq(0,120,12),labels=0:10,expand = c(0, 0), limits = c(0, NA))+
#   scale_y_continuous('Cumulative incidence,\ntime to first relevant publication',breaks=seq(0,0.45,0.05),expand = c(0, 0), limits = c(0, NA))+theme_bw()+
#   theme(legend.background = element_blank(),legend.position = c(0.2,0.85),legend.title=element_blank(),legend.text=element_text(size=12),text=element_text(size=12),axis.text=element_text(size=12),panel.grid.minor = element_blank(),
#         plot.margin = margin(1,1.5,1,1, "cm"))
# dev.off()
# 
# 
# 
# 
# 





