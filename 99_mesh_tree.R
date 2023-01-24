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
