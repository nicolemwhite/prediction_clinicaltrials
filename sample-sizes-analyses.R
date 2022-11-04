source("99_packages.R")

sample_sizes <- readRDS("data/clintrials_sample_sizes.rds")

sample_sizes %>%
  group_by(id) %>%
  summarize(n=n())

sample_sizes2 <- 
  sample_sizes %>%
  mutate(
    sample_size_type = ifelse(!nzchar(sample_size_type) & status != "Completed", "Anticipated", sample_size_type),
    completed = (status == "Completed"),
    ss_updated_at_completion = (sample_size_type == "Anticipated" & completed),
    sample_size_type = ifelse(sample_size_type == "Ancitipated" & completed, "Actual", sample_size_type),
    sample_size_type = ifelse(!nzchar(sample_size_type), NA_character_, sample_size_type)
  ) %>%
  distinct()

# keep only the earliest anticipated sample_size
sample_sizes3 <- rbind(
  filter(sample_sizes2, sample_size_type != "Anticipated"),
  sample_sizes2 %>%
    filter(sample_size_type == "Anticipated") %>%
    group_by(id) %>%
    arrange(date) %>%
    slice(1) %>%
    ungroup()
) %>%
  arrange(id)


fx_keep_only_one_actual <- function(.data) {
  # keep only one row for the actual sample size
  # if there are two *DIFFERENT* sample sizes marked as "actual", then remove that ID altogether
  
  dat <- .data
  dat$row_num <- 1:nrow(dat)
  
  dat <- 
    dat %>%
    filter(sample_size_type == "Actual") %>%
    group_by(id) %>%
    mutate(n = n()) %>%
    ungroup() %>%
    arrange(id)
  
  keep_row_nums <-
    # keep the row if there's only one entry with type = "Actual"
    dat %>%
    filter(n==1) %>%
    pull(row_num)
  
  more_rows <-
    # also keep the row if there's more than 1 row but they all have the same sample_size
    dat %>%
    filter(n>1) %>%
    distinct(id, sample_size, .keep_all = T) %>%
    group_by(id) %>%
    mutate(n=n()) %>%
    filter(n==1) %>%
    pull(row_num)
  
  keep_row_nums <- c(keep_row_nums, more_rows)
  
  .data[keep_row_nums, ] 
}


sample_sizes4 <-
  rbind(
    fx_keep_only_one_actual(sample_sizes3),
    filter(sample_sizes3, sample_size_type != "Actual")
  ) %>%
  arrange(id)

sample_sizes5 <-
  sample_sizes4 %>%
  group_by(id) %>%
  mutate(n=n()) %>%
  filter(n==2) %>%
  select(id, sample_size_type, sample_size) %>%
  pivot_wider(names_from=sample_size_type, values_from=sample_size) %>%
  mutate(anticipated_minus_actual = Anticipated - Actual,
         actual_over_anticipated = Actual/Anticipated)

sample_sizes5 %>%
  ggplot(aes(x=1,actual_over_anticipated)) +
  geom_boxplot(outlier.alpha=0) +
  ggbeeswarm::geom_beeswarm(alpha=0.4) +
  scale_y_continuous(limits=c(0, 3), labels = scales::percent) +
  theme_bw() +
  labs(
    title = "Actual sample size as a percentage of anticipated",
    y = expression(paste(frac("Actual", "Anticipated"), "  (%)"))
    ) +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())

ggsave("output/figures/sample-size_actual-over-anticipated1.png", height=7, width=5.1)


sample_sizes5 %>%
  ggplot(aes(x=Anticipated, y=actual_over_anticipated, col=Actual < Anticipated)) +
  geom_point(alpha=0.6) +
  scale_y_continuous(labels = scales::percent, trans="log10") +
  scale_x_continuous(limits=c(0,10000)) +
  labs(
    title = "Actual sample size as a percentage of anticipated",
    y = expression(paste(frac("Actual", "Anticipated"), "  (%)")),
    x = "Anticipated (n)"
  ) +
  theme_bw()

ggsave("output/figures/sample-size_actual-over-anticipated2.png", height=5, width=7)

# find fraction of studies which include "Actual" sample size on study completion

sample_sizes2 %>% 
  filter(status == "Completed") %>%
  group_by(id) %>%
  arrange(desc(date)) %>%
  slice(1) %>%
  ungroup() 
