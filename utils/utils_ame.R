
##AME helper######


isFileUploaded <- function(label, input){
  inputFile <- input[[label]]
  
  if( is.null(inputFile) ){
    shinyFeedback::feedbackWarning(label, TRUE, "Please upload a file!")
    return (FALSE)
  }
  return( TRUE)
}

isExtensionPermitted <- function(label, input){
  
  permitted <- TRUE
  inputFile <- input[[label]]
  
  extStart <- tools::file_ext(inputFile$name)
  if( !(extStart %in% c("tsv", "bed", "narrowPeak", "broadPeak") )){
    shinyFeedback::feedbackWarning(label, TRUE, "Please use a tab separetd file with '.peak', '.bed' or '.tsv' extension ")
    permitted <-FALSE
  }
  
  permitted
}

checkStatus <- function(obj, msg){
  if(obj$status != 0){
    showNotification(msg, type = "error", duration = NULL,)
    req(FALSE, cancelOutput = FALSE)
  }
}