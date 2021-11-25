#2_assess_10percent_sample.R
#takes a random sample from studies to determine overall sensitivity & specificity of search terms

source('99_packages.R')
source('99_functions.R')

load('data/processed_studies.rda')

char2seed('clintrialsgov')
prop_sample = 0.1

check_studies = studies %>% sample_frac(prop_sample)

#write to workbook for offline use
write.xlsx(check_studies,file='data/processed_studies_random10percent.xlsx')

