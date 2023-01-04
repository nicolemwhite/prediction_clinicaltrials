# time to study completion (as reported on clin-trials)
source("99_packages.R") #moved library(survival) to 99_packages.R

#load("data/all_studies.rda") #NW: should this be final_studies, not all_studies?
load("data/final_studies.rda")


dat <- dat_included %>%
  select(id, overall_status, submitted, updated, keyword=Keyword) %>%
  mutate(
    across(
      c(submitted, updated),
      \(x) as.Date.character(x, format = "%B %d, %Y")
    ),
    status = case_when(
      overall_status == "Completed" ~ "completed",
      overall_status %in% c("Withdrawn", "Terminated", "Suspended") ~ "stopped",
      TRUE ~ "ongoing",
    ),
    status = factor(status, levels=c("ongoing", "stopped", "completed"))) %>%
  mutate(
    submit_days_since_min = as.numeric(submitted - min(.$submitted)),
    days = as.numeric(updated - submitted)
  ) %>%
  rowwise() %>%
  mutate(study_type = paste0(unlist(keyword), collapse = ";")) %>%
  filter(days > 0)


mfit <- survfit(
  formula = Surv(days, status) ~ study_type, 
  data=filter(dat, study_type %in% c("Prognostic;Development", "Diagnostic;Development"))
)
# plot(mfit)
# summary(mfit)
survminer::ggcompetingrisks(mfit, conf.int = F)

# same thing but using cumulative incidence curves
dat_subset <- filter(dat, study_type %in% c("Prognostic;Development", "Diagnostic;Development"))
# dat_subset <- dat
cfit <- cuminc(ftime=(dat_subset$days)/365.25, fstatus=dat_subset$status, group=dat_subset$study_type)
survminer::ggcompetingrisks(
  cfit, 
  multiple_panels = F,
  conf.int=T,
  xlab="Years", 
  legend="right"
) +
  labs(linetype="Study type",
       colour = "Study status",
       fill = "Study status")

ggsave("output/figures/time-to-study-completion.png", height=5, width=7)
