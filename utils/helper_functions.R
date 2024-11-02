checkNetworkCreated <- function() {
  if (!networkCreated) {
    shinyalert::shinyalert(
      "There are no Genes to Analyse",
      "Please input genes and press the RUN button before performing any analysis",
      type = "error"
    )
    return(FALSE)
  }
  return(TRUE)
}