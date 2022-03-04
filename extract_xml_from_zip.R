#extract_xml_from_zip.R
source('99_packages.R')
source('99_functions.R')

#setseed
TeachingDemos::char2seed('clinicaltrials')

home = getwd()

# review files in zipped folder
all_results = unzip('Z:/clinicaltrials/AllPublicXML.zip',list=TRUE) %>% data.frame() %>%
  filter(Name!='Contents.txt') %>% mutate(FileName = gsub(".*/","",Name))

#take random sample to downoad
n = 100
sample_results = sample_n(all_results,n)

# site_search('https://clinicaltrials.gov/ct2/show/NCT04757766',destfile = "web/result.html")
# page <- read_html('web/result.html') 

for (k in 1:n){
xml_data = getXMLrecord_zip(sample_results,FilePath = sample_results[k,"Name"])
}
