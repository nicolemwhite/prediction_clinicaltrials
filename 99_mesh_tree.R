#99_mesh_tree.R

source('99_packages.R')
source('99_functions.R')
mesh_tree <- xmlParse('data/mesh/desc2022.xml',useInternalNodes = T)
mesh_descriptor_tree = do.call(rbind, xpathApply(mesh_tree, "//DescriptorRecord", function(node) {
  date_created <- xmlValue(node[["DateCreated"]])
  date_revised <- xmlValue(node[["DateRevised"]])
  descriptor_ui <- xmlValue(node[["DescriptorUI"]])
  descriptor_name <- xmlValue(node[["DescriptorName"]])
  tree_number <- xpathSApply(node,"./TreeNumberList/TreeNumber",xmlValue)
  if (is.null(tree_number)) tree_number <- NA
  tibble(descriptor_ui, descriptor_name,tree_number,date_created,date_revised)
}))

mesh_concept_tree = do.call(rbind, xpathApply(mesh_tree, "//DescriptorRecord", function(node) {
  descriptor_ui <- xmlValue(node[["DescriptorUI"]])
  concept_ui <-   xpathSApply(node,"./ConceptList//ConceptUI",xmlValue)
  concept_name <-   xpathSApply(node,"./ConceptList//ConceptName",xmlValue)
  preferred_yn <- xpathSApply(node,"./ConceptList//Concept",xmlGetAttr,'PreferredConceptYN')
  tibble(descriptor_ui, concept_ui,concept_name,preferred_yn)
}))


save(mesh_descriptor_tree,mesh_concept_tree,file='data/mesh/mesh_term_info.rda')

mesh_tree_nodes = getNodeSet(mesh_tree,'//DescriptorRecord')

mesh_descriptor_tree = tibble(
    descriptor_class = sapply(mesh_tree_nodes,xpathSApply,".",xmlGetAttr,"DescriptorClass"),
    descriptor_ui = sapply(mesh_tree_nodes,xpathSApply,"./DescriptorUI",xmlValue),
    descriptor_name = sapply(mesh_tree_nodes,xpathSApply,"./DescriptorName",xmlValue),
    date_established = sapply(mesh_tree_nodes,xpathSApply,"./DateEstablished",function(x) unlist(lapply(xmlChildren(x),xmlValue)[c("Year","Month","Day")])) %>% apply(.,2,function(x) str_c(x,collapse='-')), 
    date_created = sapply(mesh_tree_nodes,xpathSApply,"./DateCreated",function(x) unlist(lapply(xmlChildren(x),xmlValue)[c("Year","Month","Day")])) %>% apply(.,2,function(x) str_c(x,collapse='-')),
    date_revised = sapply(mesh_tree_nodes,xpathSApply,"./DateRevised",function(x) unlist(lapply(xmlChildren(x),xmlValue)[c("Year","Month","Day")])) %>% sapply(function(x) str_c(x,collapse = '-')),
    tree_number = sapply(mesh_tree_nodes,xpathSApply,"./TreeNumberList/TreeNumber",xmlValue),
    previous_indexing = lapply(mesh_tree_nodes,xpathSApply,"./PreviousIndexingList",xmlValue),
    related_descriptor_ui = sapply(mesh_tree_nodes,xpathSApply,".//SeeRelatedDescriptor/DescriptorReferredTo/DescriptorUI",xmlValue))

save(mesh_descriptor_tree,file='data/mesh/mesh_term_info_v2.rda')

# load(file='data/mesh/mesh_term_info_v2.rda')

# create a dataframe with the descriptor and associated previous descriptors and their associated years for snowballing
previous_descriptors <-
  mesh_descriptor_tree %>% 
  rowwise() %>%
  mutate(pi_char = paste0(unlist(previous_indexing), collapse = ";")) %>%
  ungroup() %>%
  mutate(pi_char = str_split(pi_char, "(?<=[0-9]{4}\\))")) %>%
  unnest(pi_char) %>%
  mutate(pi_char = ifelse(pi_char == "", NA, pi_char)) %>% 
  group_by(descriptor_ui) %>%
  mutate(n_pi = n()) %>%
  ungroup() %>%
  filter(!(is.na(pi_char) & n_pi > 1)) %>%
  select(-n_pi) %>%
  mutate(
    pi_year_start = str_extract(pi_char, "(?<=\\()[0-9]{4}"),
    pi_year_end = str_extract(pi_char, "[0-9]{4}(?=\\))"),
    across(starts_with("pi_year"), as.numeric),
    pi_char = str_trim(str_remove(pi_char, "\\(.*\\)")) # remove years from pi string (OPTIONAL?)
  ) %>%
  select(
    descriptor_ui, 
    descriptor_name, 
    previous_idx_name = pi_char,
    previous_idx_year_min = pi_year_start,
    previous_idx_year_max = pi_year_end
  )


mesh_descriptors_all <-
  mesh_descriptor_tree %>%
  select(descriptor_ui, descriptor_name) %>%
  mutate(descriptor_name_lower = tolower(descriptor_name))


previous_descriptor_ui_lookup <-
  previous_descriptors %>%
  select(orig_descriptor_ui = descriptor_ui, orig_descriptor_name = descriptor_name, previous_idx_name) %>%
  mutate(previous_idx_name_lower = tolower(previous_idx_name)) %>%
  filter(!is.na(previous_idx_name)) %>%
  inner_join(
    mesh_descriptors_all, 
    by = c("previous_idx_name_lower" = "descriptor_name_lower")
  ) %>% 
  select(orig_descriptor_ui, previous_idx_name, previous_descriptor_ui = descriptor_ui)

previous_descriptors_with_ui <-
  previous_descriptors %>% 
  left_join(
    previous_descriptor_ui_lookup, 
    by = c("descriptor_ui" = "orig_descriptor_ui", "previous_idx_name" = "previous_idx_name")
  )

save(previous_descriptors_with_ui,file='data/mesh/mesh_descriptors_previous_indexing.rda')
