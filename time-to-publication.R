# Time from registration to publication

source("99_packages.R")
library(survival)
library(ggsurvfit)

# load included studies and scores from matching algorithm
load("data/final_studies.rda")
scoredTrials <- readRDS("data/scoredTrials.rds")

prob_adj_threshold <- 0.5
date_of_score_matching <- as.Date.character("2022-11-01")


scoredTrials_top <-
  scoredTrials %>%
  filter(prob_adj > prob_adj_threshold) %>%
  group_by(NCT_num) %>%
  arrange(desc(prob_adj)) %>%
  slice(1:10) %>%
  ungroup()

scoredTrials_top %>%
  group_by(NCT_num) %>%
  arrange(desc(prob_adj)) %>%
  slice(1) %>% # only show top match %
  ungroup() %>%
  ggplot(aes(prob_adj)) +
  geom_histogram(bins = 100) +
  theme_bw()


f_extract_details_from_query <- function(l) {
  data.frame(
    pubdate = l$pubdate,
    epubdate = l$epubdate,
    title = l$title,
    pubstatus = l$pubstatus
  )
}

f_send_pmid_query <- function(pmids) {
  pmid_details <- rentrez::entrez_summary(db = "pubmed", id = pmids)
  lapply(pmid_details, f_extract_details_from_query) %>%
    do.call("rbind", .) %>%
    rownames_to_column("PMID") %>%
    mutate(PMID = as.integer(PMID))
}

f_get_pmid_details <- function(pmids, chunk_size) {
  chunks <- split(pmids, paste0("ch", ceiling(seq_along(pmids) / chunk_size)))
  map_dfr(chunks, f_send_pmid_query)
}

all_pmid_details <- f_get_pmid_details(pmids = scoredTrials_top$PMID, chunk_size = 250)
scoredTrials_top_with_dates <- left_join(all_pmid_details, scoredTrials_top, by = "PMID")

# pmid_details <- rentrez::entrez_summary(db="pubmed", id=scoredTrials_top$PMID)
# scoredTrials_top_with_dates <-
#   lapply(pmid_details, f_extract_details_from_query) %>%
#   do.call("rbind", .) %>%
#   rownames_to_column("PMID") %>%
#   mutate(PMID = as.integer(PMID)) %>%
#   inner_join(scoredTrials_top, by="PMID")

dat_included_with_pub <-
  dat_included %>%
  select(id, overall_status, submitted, posted, updated) %>%
  left_join(select(scoredTrials_top_with_dates, id = NCT_num, PMID, pubdate, epubdate), by = "id") %>%
  mutate(status = ifelse(is.na(PMID), 0, 1))


dat_included_with_pub2 <- # clean dates
  dat_included_with_pub %>%
  mutate(
    pubdate = ifelse(nchar(pubdate) == 4, epubdate, pubdate), # if the pubdate is just the year, use the epubdate
    across(
      c(submitted, posted, updated),
      \(x) as.Date.character(x, format = "%B %d, %Y")
    ),
    pubdate_old = pubdate,
    epubdate_old = epubdate,
    across(
      c(pubdate, epubdate),
      \(x) as.Date.character(x, format = "%Y %b %d")
    ),
    pubdate_YYYY_M = as.Date.character(paste(pubdate_old, "1"), format = "%Y %b %d"),
    pubdate = ifelse(is.na(pubdate), pubdate_YYYY_M, pubdate),
    pubdate = as.Date.numeric(pubdate, origin = "1970-01-01")
  ) %>%
  filter(!(status & is.na(pubdate))) %>% # remove the 6 records with ambiguous/missing pubdate for matched study
  select(-all_of(c("epubdate", "pubdate_old", "epubdate_old", "pubdate_YYYY_M"))) %>%
  mutate(
    days = ifelse(
      status == 1,
      as.numeric(difftime(pubdate, submitted, units = "days")),
      as.numeric(difftime(date_of_score_matching, submitted, units = "days"))
    )
  ) %>%
  filter(days > 0) %>% # only keep matched pubs with dates after the registration
  group_by(id) %>%
  arrange(days) %>% # keep the earliest matched pub
  slice(1) %>%
  ungroup()

saveRDS(dat_included_with_pub2, file = "data/time-to-pub.rds")



# run from here to save re-wrangling and pumed requests
dat_included_with_pub2 <- readRDS("data/time-to-pub.rds")

survfit2(Surv(days, status) ~ 1, data = dat_included_with_pub2) %>%
  ggsurvfit(type = "risk") +
  labs(
    x = "Days",
    y = "Publication probability",
    title = "Time to first publication from registration"
  ) +
  add_confidence_interval()

ggsave("output/figures/time-to-first-publication.png", height = 5, width = 7)

# add screening outcomes
dat_included_with_pub3 <- 
  screening_results %>%
  select(id=NCT, labels, Prognostic, Diagnostic, Development, Validation) %>%
  left_join(dat_included_with_pub2, ., by="id") %>%
  mutate(
    study_type = "",
    study_type = ifelse(Prognostic, paste(study_type, "Prognostic"), study_type),
    study_type = ifelse(Diagnostic, paste(study_type, "Diagnostic"), study_type),
    study_type = ifelse(Development, paste(study_type, "Development"), study_type),
    study_type = ifelse(Validation, paste(study_type, "Validation"), study_type),
    study_type = str_remove(study_type, "^ "),
    multiple_designs = str_detect(labels, "multiple"),
    submit_days_since_min = as.numeric(submitted - min(.$submitted))
  )


m1 <- coxph(Surv(days, status) ~ study_type, data=dat_included_with_pub3)
m2 <- coxph(Surv(days, status) ~ strata(study_type), data=dat_included_with_pub3)
m3 <- coxph(Surv(days, status) ~ strata(study_type) + multiple_designs, data=dat_included_with_pub3)
m4 <- coxph(Surv(days, status) ~ strata(study_type) + multiple_designs + submit_days_since_min, data=dat_included_with_pub3)
m5 <- coxph(Surv(days, status) ~ strata(study_type) + multiple_designs + pspline(submit_days_since_min, df=2), data=dat_included_with_pub3)
m6 <- coxph(Surv(days, status) ~ strata(study_type) + multiple_designs + pspline(submit_days_since_min, df=3), data=dat_included_with_pub3)
m7 <- coxph(Surv(days, status) ~ strata(study_type) + multiple_designs + pspline(submit_days_since_min, df=4), data=dat_included_with_pub3)

AIC(m1, m2, m3, m4, m5, m6, m7) # model 3 (excludes parameter for time) is best

summary(m3)
cox.zph(m3)


library(ggfortify)

autoplot(survfit(
  ggsurvfit::Surv(days/365.25, status) ~ strata(study_type), 
  # data=dat_included_with_pub3,
  data=filter(dat_included_with_pub3, study_type %in% c(
    "Prognostic Development",
    "Diagnostic Development"#,
    # "Prognostic Validation",
    # "Diagnostic Validation"
  ))
), fun="event") +
  theme_bw() +
  labs(
    x = "Years",
    y = "Publication probability",
    title = "Time to first publication from registration",
    fill = "Study type",
    col = "Study type"
  ) +
  coord_cartesian(xlim=c(0, 10)) # to focus on main area of data


ggsave("output/figures/time-to-first-publication-by-study-type.png", height = 5, width = 7)

m8 <- coxph(Surv(submit_days_since_min, days + submit_days_since_min, status) ~ strata(study_type) + multiple_designs, data=dat_included_with_pub3)

AIC(m3, m8) # incorporating submit_days_since_min by using at as tstart is better than tstart = 0
cox.zph(m8)
summary(m8)

autoplot(survfit(
  ggsurvfit::Surv(submit_days_since_min, days + submit_days_since_min, status) ~ strata(study_type), 
  data=dat_included_with_pub3,
  # data=filter(dat_included_with_pub3, study_type %in% c(
  #   "Prognostic Development",
  #   "Diagnostic Development"#,
  #   # "Prognostic Validation",
  #   # "Diagnostic Validation"
  # ))
), fun="event") +
  theme_bw() +
  labs(
    x = "Days",
    y = "Publication probability",
    title = "Time to first publication from registration",
    fill="Study type",
    col="Study type"
  )



