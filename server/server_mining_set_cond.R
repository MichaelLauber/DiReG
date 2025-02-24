#All  performed analysis are hidden
hideAll <- function(){
  cond_ora(0)
  cond_gsea(0)
  cond_gtex(0)
  cond_tfcof(0)
  cond_tftf(0)
  cond_isoforms(0)
  cond_tfa(0)
}   


resetBtns <- function(){
  btn_ids <- c("btnORA", "btnGSEA", "btnGTEx", "btnTfcof", "btnTFTF", "btnIsoforms", "btnTFA")
  for (btn_id in btn_ids) {
    shinyjs::runjs(sprintf("document.getElementById('%s').classList.remove('bttn-success')", btn_id))
    shinyjs::runjs(sprintf("document.getElementById('%s').classList.add('bttn-primary')", btn_id))
  }
}

handleButtonClick <- function(buttonId, conditionSetter) {
  observeEvent(input[[buttonId]], {
    
    if (!networkCreated) {
      return()
    }
    
    hideAll()
    resetBtns()
    conditionSetter(1)
    shinyjs::runjs(sprintf("document.getElementById('%s').classList.remove('bttn-primary')", buttonId))
    shinyjs::runjs(sprintf("document.getElementById('%s').classList.add('bttn-success')", buttonId))
    
    shinyjs::toggle(id = 'networkContainer', condition = FALSE)
    shinyjs::toggle(id = 'expandButtonContainer', condition = TRUE) 
  })
}

observeEvent(input$expandButton, {
  shinyjs::toggle(id = "networkContainer", condition = TRUE)  # Show the network container
  shinyjs::toggle(id = "expandButtonContainer", condition = FALSE)  # Hide the button container
})


cond_ora <- reactiveVal(0)
cond_gsea <- reactiveVal(0)
cond_gtex <- reactiveVal(0)
cond_tfcof <- reactiveVal(0)
cond_tftf <- reactiveVal(0)
cond_isoforms <- reactiveVal(0)
cond_tfa <- reactiveVal(0)

handleButtonClick("btnORA", cond_ora)
handleButtonClick("btnGSEA", cond_gsea)
handleButtonClick("btnGTEx", cond_gtex)
handleButtonClick("btnTfcof", cond_tfcof)
handleButtonClick("btnTFTF", cond_tftf)
handleButtonClick("btnIsoforms", cond_isoforms)
handleButtonClick("btnTFA", cond_tfa)


output$cond_ora = renderText({cond_ora()})
output$cond_gsea = renderText({cond_gsea()})
output$cond_gtex = renderText({cond_gtex()})
output$cond_tfcof = renderText({cond_tfcof()})
output$cond_tftf = renderText({cond_tftf()})
output$cond_isoforms = renderText({cond_isoforms()})
output$cond_tfa = renderText({cond_tfa()})

observeEvent(input$ok, {
  cond_tfa(1)
})

outputOptions(output, 'cond_ora', suspendWhenHidden=FALSE)
outputOptions(output, 'cond_gsea', suspendWhenHidden=FALSE)
outputOptions(output, 'cond_gtex', suspendWhenHidden=FALSE)
outputOptions(output, 'cond_tfcof', suspendWhenHidden=FALSE)
outputOptions(output, 'cond_tftf', suspendWhenHidden=FALSE)
outputOptions(output, 'cond_isoforms', suspendWhenHidden=FALSE)
outputOptions(output, 'cond_tfa', suspendWhenHidden=FALSE)