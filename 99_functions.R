#99_functions.R
null_na<-function(input){
  return(ifelse(all(!is.null(input)),input,NA))
}