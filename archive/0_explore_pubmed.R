#0_explore_pubmed.R
library(easyPubMed)


me_on_pubmed <- get_pubmed_ids("Nicole White[AU]")
my_abstracts_txt <- fetch_pubmed_data(pubmed_id_list = me_on_pubmed, format = "abstract")
my_abstracts_xml <- fetch_pubmed_data(pubmed_id_list = me_on_pubmed, format = "xml")
titles <- custom_grep(my_abstracts_xml, "ArticleTitle", "char")


#title search string for clinical prediction models
