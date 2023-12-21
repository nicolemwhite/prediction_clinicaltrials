#1_find_duplicates.R
source('99_packages.R')
source('99_functions.R')
load('data/final_studies.rda')

#get subset of records that were manually reviewed for eligibility
ids = screening_results$NCT


#load all NCTs
load('Z:/clinicaltrials/data/analysis_ready/all_studies.rda')

##subset to ids to extract title and date info
record_info = select(dat,id,brief_title,official_title,posted) %>% filter(id %in% ids)

#find records with the same title
possible_duplicates = record_info[duplicated(record_info$official_title),] %>% filter(!is.na(official_title))

inner_join(record_info,possible_duplicates,by='official_title')

distinct(record_info,official_title) %>% nrow()


library(textreuse)
#define settings for textreuse -> local senstivity hashing
n.minhash = 200
n.bands = 50
random.seed = TeachingDemos::char2seed('jaccard',set=F)
minhash <- minhash_generator(n = n.minhash, seed = random.seed)
cutoff=0.5
nct_duplicates = list()

#brief title
record_info_bt = filter(record_info,!is.na(brief_title))
text.corpus = TextReuseCorpus(text=record_info_bt$brief_title,meta=list(id="id"),tokenizer = tokenize_words,minhash_func = minhash,skip_short = F)
buckets <- lsh(text.corpus, bands = n.bands)
candidates <- lsh_candidates(buckets)
jacsim = lsh_compare(candidates, text.corpus, jaccard_similarity) %>% mutate_at(c('a','b'),~as.numeric(str_remove_all(.,pattern='doc-')))
possible_duplicates= filter(jacsim,score>=cutoff)

nct_duplicates[['brief_title']]=tibble('a'=record_info_bt[possible_duplicates$a,][['id']],'b'=record_info_bt[possible_duplicates$b,][['id']],score=possible_duplicates$score) 

#official title
record_info_ot = filter(record_info,!is.na(official_title))
text.corpus = TextReuseCorpus(text=record_info_ot$official_title,meta=list(id="id"),tokenizer = tokenize_words,minhash_func = minhash,skip_short = F)
buckets <- lsh(text.corpus, bands = n.bands)
candidates <- lsh_candidates(buckets)
jacsim = lsh_compare(candidates, text.corpus, jaccard_similarity) %>% mutate_at(c('a','b'),~as.numeric(str_remove_all(.,pattern='doc-')))
possible_duplicates = filter(jacsim,score>=cutoff)
nct_duplicates[['official_title']]=tibble('a'=record_info_ot[possible_duplicates$a,][['id']],'b'=record_info_ot[possible_duplicates$b,][['id']],score=possible_duplicates$score) 


#bind_rows
nct_duplicates = bind_rows(nct_duplicates,.id='field')
#arrange by decreasing score
nct_duplicates = nct_duplicates %>% arrange(-score)
write.xlsx(nct_duplicates,file='data/manual checks/possible duplicates.xlsx')
