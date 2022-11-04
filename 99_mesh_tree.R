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