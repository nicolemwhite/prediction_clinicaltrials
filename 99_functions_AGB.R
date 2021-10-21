# 99_functions.R
# functions for reading data
# March 2021

# function to extract sample size from historical clintrials data
sample_historical = function(intable, id, index){
   # get the earliest and latest results
   address = paste(url, intable$links[index], sep='')
   download_xml(url=address, file='web/page.html')
   in_page <- read_html('web/page.html') 
   
   # extract the study status ...
   study_status = as.character(in_page %>% html_nodes("#StudyStatus") %>% html_nodes("td"))
   # ... then get the latest status
   l_index = which(str_detect(string=study_status, pattern='Overall Status'))
   status = str_trim(str_remove_all(study_status[l_index+1], pattern='\\n|^<td>|</td>$'))
   
   # if missing status then exclude
   if(length(status)==0){
      f = data.frame(id = id, date = table$dates[index], status = 'Missing', sample_size_type = NA, sample_size = NA)
      return(f)
   }
   # if withheld then no data
   if(status=='Withheld'){
      f = data.frame(id = id, date = table$dates[index], status = 'Withheld', sample_size_type = NA, sample_size = NA)
      return(f)
   }
   
   # extract the study design ...
   study_design = as.character(in_page %>% html_nodes("#StudyDesign") %>% html_nodes("td"))
   # ... then get the sample size
   e_index = which(str_detect(string=study_design, pattern='Enrollment'))
   text = str_remove_all(study_design[e_index+1], pattern='\\n|^<td>|</td>$')
   split = str_split(string=text, pattern=' ', n=2)
   if(length(split) == 0){ # for occasional incomplete reports
      sample_size = NA
      sample_size_type = ''
   }
   if(length(split) > 0){ # for occasional incomplete reports
      sample_size = as.numeric(split[[1]][1])
      sample_size_type = str_remove_all(string=split[[1]][2], pattern='[^A-Z|a-z]')
   }
   f = data.frame(id = id, date = table$dates[index], status = status, sample_size_type = sample_size_type, sample_size = sample_size)
   return(f)
}

# function to replace null with NA, used by clintrials reading
null_na = function(x, collapse=FALSE, date=FALSE){
  y = ifelse(is.null(x), NA, x)
  if(collapse==TRUE){# collapse vector into one character
     y = paste(unique(y), sep='', collapse=', ') 
  }
  if(date==TRUE){ # convert to date
     q = str_detect(string=y, pattern='\\?') # search for question mark, does not seem to be an issue
     y = ifelse(q==FALSE, as.Date(y, '%B %d, %Y'), 'Question') # flag `?` for now
     y = as.Date(y, origin='1970-01-01')
  }
  return(y)
}

# function to extract XML results for clinicaltrials.gov, no longer used
my_extract_xml = function(x, part, name, collapse=FALSE, count=FALSE, char=FALSE){
   path = paste(".//Struct[@Name='", part, "']//Field[@Name='", name, "']", sep='')
   get = xml_text(xml_find_all(x, path))
   length_get = length(get)
   if(length_get == 0){get = NA} # replace missing with NA
   if(length_get > 1){
      if(collapse==TRUE){get = paste(get, sep='', collapse=', ')} # combine
   }
   if(count == TRUE){
      get = length_get
   }
   if(char == TRUE){
      get = nchar(get) # just count characters
   }
   get[get=='N/A'] = 'Not Applicable' # replace N/A with 'Not Applicable' - avoid confusion with NA = missing
   return(get)
}

# converting exclusion age from text to number (ANZCTR)
convert_age = function(type, number){
 if(type %in% c('No limit' , 'Not stated', 'N/A') == TRUE){ 
   if(type=='N/A'){type = 'No limit'} # assume N/A means no limit
   t = type
   num = NA
 }
 if(type %in% c('No limit' , 'Not stated', 'N/A') == FALSE){
   t = 'Restricted'
   number = as.numeric(number)
   # scale number to years:
   if(type %in% c('Minutes','Minute')){num = number / (365.25*24*60)} 
   if(type %in% c('Hours','Hour')){num = number / (365.25*24)} 
   if(type %in% c('Days','Day')){num = number / 365.25} 
   if(type %in% c('Weeks','Week')){num = number / 52} 
   if(type %in% c('Months','Month')){num = number / 12} 
   if(type %in% c('Years','Year')){num = number / 1} 
   # if zero
   if(num==0){type = 'No limit'; num=NA}
 }
 toreturn = data.frame(type=t, num = num)
 return(toreturn)
}

# converting exclusion age from text to number (clinicaltrials.gov)
convert_age_clintrials = function(number){
   num = 99
   if(is.na(number) == TRUE | number =='N/A'){ 
      t = 'No limit'
      num = NA
   }
   if(is.na(num)==FALSE){ # not changed above
      t = 'Restricted'
      num = as.numeric(str_remove_all(number, '[^0-9]')) # just the number
      # scale number to years:
      if(str_detect(string=number, pattern='Minute')){num = num / (365.25*24*60)} 
      if(str_detect(string=number, pattern='Hour')){num = num / (365.25*24)} 
      if(str_detect(string=number, pattern='Day')){num = num / 365.25} 
      if(str_detect(string=number, pattern='Week')){num = num / 52} 
      if(str_detect(string=number, pattern='Month')){num = num / 12} 
      if(str_detect(string=number, pattern='Year')){num = num / 1} 
      # if zero
      if(num==0){type = 'No limit'; num=NA}
   }
   toreturn = data.frame(type = t, num = num)
   return(toreturn)
}

# nice variable name for anzctr - not complete
nice_anzctr_trials = function(inname){
   outname = case_when(
      inname == 'studytype' ~ 'Study type',
      inname == 'date' ~ 'Submitted date', 
      inname == 'study_update' ~ 'Updated date', # not sure
      inname == 'status' ~ 'Overall Recruitment Status',
      inname == 'purpose' ~ 'Primary Purpose',
      inname == 'phase' ~ 'Study Phase',
      inname == 'assignment' ~ 'Interventional Study Model', 
      inname == 'n_funding' ~ 'Number of funders',
      inname == 'n_primary' ~ 'Number of primary outcomes',
      inname == 'n_secondary' ~ 'Number of secondary outcomes',
      inname == 'masking' ~ 'Masking',
      inname == 'allocation' ~ 'Allocation',
      inname == 'gender' ~ 'Sex',
      inname == 'age_limit' ~ 'Age limit',
      inname == 'min_age' ~ 'Minimum Age',
      inname == 'max_age' ~ 'Maximum Age',
      TRUE ~ inname
   )
   return(outname)
}

# nice variable name for clinical trials
# https://prsinfo.clinicaltrials.gov/definitions.html
nice_clinical_trials = function(inname){
   outname = case_when(
      inname == 'study_type' ~ 'Study type',
      inname == 'study_first_submitted' ~ 'Submitted date', # not sure
      inname == 'study_update' ~ 'Updated date', # not sure
      inname == 'status' ~ 'Overall Recruitment Status',
      inname == 'purpose' ~ 'Primary Purpose',
      inname == 'phase' ~ 'Study Phase',
      inname == 'assignment' ~ 'Interventional Study Model', 
      inname == 'n_arms' ~ 'Number of Arms',
      inname == 'n_primary' ~ 'Number of primary outcomes',
      inname == 'n_secondary' ~ 'Number of secondary outcomes',
      inname == 'masking' ~ 'Masking',
      inname == 'allocation' ~ 'Allocation',
      inname == 'gender' ~ 'Sex',
      inname == 'min_age' ~ 'Minimum Age',
      inname == 'max_age' ~ 'Maximum Age'
   )
   return(outname)
}

# convert date that sometimes has no day
my.as.Date = function(indate){
   indate[is.na(indate)] = ''
   spaces = str_count(string=indate, pattern=' ')
   dformat = ifelse(spaces==1, '%d %B %Y', '%B %d, %Y') # format depends on number of spaces
   indate[spaces==1] = paste('15 ', indate[spaces==1], sep='') # assume middle of month
   outdate = as.Date(indate, dformat)
   return(outdate)
}

# function for rounding numbers with zeros kept
roundz = function(x, digits){
   dformat = paste('%.', digits, 'f', sep='')
   x = sprintf(dformat, round(x, digits))
   return(x)
}

## create nice plot of sample size regression model estimates
# plot target and actual in same plot
plot_function = function(indata, 
                         table_names, 
                         xlabs = c('Decrease','Increase'), # labels for x-axis
                         label_location = 'right', # location within plot area for group labels
                         remove_this_size = 1, # default to remove groups that are just 1 result
                         lsize = 13, # size of labels text
                         label_side = NULL, # side for group labels
                         ljust = 0.5, # justification of legend at top
                         x_limits = 1:3, # major ticks for x-axis
                         minor_breaks=0 # minor ticks for x-axis
                         ){
   # add estimates (from elastic net models) to labels
   add_ests = full_join(table_names, indata, by='term') %>%
      filter(!term == '(Intercept)',
             !is.na(estimate)|reference==TRUE)  %>% # remove missing estimates, but keep reference categories
      select(-p.value, -std.error, -statistic) %>% # tidy up
      mutate(estimate = ifelse(reference==TRUE, 0, estimate), # for reference groups
             outcome_num = case_when( # outcome number for colouring and ordering in legend
                outcome == 'target' ~ 1,
                outcome == 'actual' ~ 2,
                is.na(outcome) ~ 3
             ))
   # only include groups that were in the final model
   add_ests  =group_by(add_ests, group_number) %>%
      summarise(any_non_ref = min(reference)) %>%
      right_join(add_ests, by='group_number') %>%
      filter(any_non_ref == 0) # only if there's at least one non-reference estimate
   
   # quick check that all estimates are in the reference table (should only be intercept); and vice versa
   check = function(){
      f = filter(add_ests, is.na(label)) %>% dplyr::select(term) # should be empty
   }
   
   # order results within group and make x-axis number
   est_rank = group_by(add_ests, group_number, term) %>%
      summarise(mean = mean(estimate)) %>%
      mutate(rank = rank(mean)) %>%
      ungroup() %>%
      select(-mean)
   add_x = left_join(add_ests, est_rank, by=c('group_number', 'term')) %>%
      mutate(final_number = (group_number*100) + rank, # *100 to split numbers
             xaxis = as.numeric(as.factor(final_number)))
   # reverse order to match table
   add_x = mutate(add_x, xaxis = max(xaxis) - xaxis + 1)
   
   # final
   all_res = mutate(add_x,
                    reference = as.numeric(reference) + 1, # to mark reference point
                    # back transform to relative change:
                    estimate = exp(estimate), 
                    conf.low = exp(conf.low),
                    conf.high = exp(conf.high),
                  # now make into percent change,
                  estimate = 100*(estimate-1),
                  conf.low = 100*(conf.low-1),
                  conf.high = 100*(conf.high-1)
                     
   ) 
   # get labels
   axis_labels = select(all_res, xaxis, label) %>%
      arrange(xaxis) %>%
      unique() %>%
      pull(label)

   # where to put labels    
   if(label_location == 'right'){
      put_labels = max(all_res$conf.high, na.rm=T) # put labels at highest CI (right side)
      h_adjustment = 1 # horizontal adjustment
   }
   if(label_location == 'left'){
      put_labels = min(all_res$conf.high, na.rm=T) # put labels at lowest CI (left side)
      h_adjustment = 0
   }
   # labels for groups inside plot area
   group_labels = group_by(all_res, group) %>%
      summarise(n=n(), meanx=mean(xaxis), maxx=max(xaxis)) %>%
      ungroup() %>%
      filter(n > remove_this_size) %>% # remove small labels?
      mutate(
         estimate = put_labels,
         conf.low=0, conf.high=0, reference=1,
         group = ifelse(group=='continuous', 'Continuous variables', group)) # 
   dotted.lines = group_labels$maxx + 0.5 # dotted lines to split groups
   # dodge target to avoid overlap of CIs with actual
   gap = 0.25
   all_res = mutate(all_res,
                    xaxis = ifelse(outcome_num==1, xaxis+gap, xaxis))
   # text for axis labels
   text1 = data.frame(xaxis=1, estimate=1, conf.low=0, conf.high=0, reference=1, label=xlabs[2])
   text2 = data.frame(xaxis=1, estimate=1, conf.low=0, conf.high=0, reference=1, label=xlabs[1])
   # plot
   star.wars.relative = ggplot(data=all_res, aes(x=xaxis, y=estimate, ymin=conf.low, ymax=conf.high, shape=factor(reference), col=factor(outcome_num)))+
      geom_hline(lty=2, yintercept=0)+ # reference line at zero
      geom_point(size=2, shape=19)+
      geom_errorbar(width=0, size=1.02)+
      scale_color_manual('Sample size:', values=c('dodgerblue','goldenrod1','grey'), labels=c('Target','Actual','Reference'))+
      geom_vline(lty=3, xintercept=dotted.lines)+ # breaks between groups of variables
      geom_text(data=text1, aes(x=xaxis, y=estimate, label =label), adj=-0.1, vjust=1, col=grey(0.5))+
      geom_text(data=text2, aes(x=xaxis, y=estimate, label =label), adj=1.1, vjust=1, col=grey(0.5))+
      geom_text(data=group_labels, aes(x=meanx, y=estimate, label =group), adj=h_adjustment, col=grey(0.5))+
      scale_x_continuous(expand=c(0.01,0.01), breaks=1:length(axis_labels), labels=axis_labels, limits=c(0.5, length(axis_labels)+gap))+ # plus 0.2 for dodge
      scale_y_continuous(breaks=x_limits, minor_breaks =minor_breaks )+ # 
      ylab('Percent change in sample size')+
      xlab('')+
      theme_bw()+
      theme(
         legend.position = 'top',
         legend.justification = c(ljust,0),
         legend.box.spacing = unit(0, 'mm'), # reduce space between plot and legend
         legend.box.margin	= margin(t=0, r=0, b=0, l=0), # reduce space around legend
         legend.margin = margin(t=0, r=0, b=0, l=0, unit='mm'), # reduce space around legend
         legend.title = element_text(size=10),
         legend.text = element_text(size=10),
         plot.margin = margin(t=0, r=1, b=1, l=0, unit='mm'), # small space around plot
         text = element_text(size=12), 
         axis.text.y = element_text(size=lsize), # size of labels, had to shorten for very large plots
         panel.grid.major.y = element_blank(),
         panel.grid.minor.y = element_blank(),
         panel.grid.minor.x = element_blank())+
      coord_flip() 
   star.wars.relative
   return(star.wars.relative)
}
