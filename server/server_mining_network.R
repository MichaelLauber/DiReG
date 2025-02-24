library(decoupleR)

dorothea_hs <- readRDS("data/dorothea_hs.rds")
dorothea_mm <- readRDS("data/dorothea_mm.rds")

networkCreated <- FALSE
btnCreateDoroPressed <- reactiveVal(FALSE)
dropdownSelected <- reactiveVal(FALSE) # checks if the network should be ristricted to a selected TF

dorothea <- reactive({
  switch(input$radioOrgDorothea,
         "human" = dorothea_hs,
         "mouse" = dorothea_mm)
}) 

organism <- reactive({
  switch(input$radioOrgDorothea,
         "human" = "hsapiens",
         "mouse" = "mmusculus")
})


data <- reactive({
  dorothea() %>%
    filter(confidence %in% input$checkConfidence) %>%
    dplyr::rename("from" = "source", "to" = "target") %>%
    dplyr::select(from, to, mor, confidence)
})

#loads example TFs in the input field 
observeEvent(input$btnMiningExample, {
  value <- "HNF1A HNF4A ONECUT1 ATF5 PROX1 CEBPA"
  updateTextInput(session, "inputTextTFs", value=value)
})


inputTFs <- eventReactive(input$btnCreateDoro, {
  
  networkCreated <<- TRUE
  btnCreateDoroPressed(TRUE)
  cond_visnet(0)
  shinyjs::runjs(sprintf('window.cond_visnet = "%s"', cond_visnet()))
  raw_input <- input$inputTextTFs
  split_result <- stringr::str_split(raw_input, "[,;\\s]+") %>%  unlist()
  processed_input <- split_result[split_result != ""]
  
  gprofiler2::gconvert(query = processed_input,
                       organism = organism(),
                       target="ENSG", # ifelse(organism() == "mmusculus", "ENSMUSG", "ENSG")
                       mthreshold = Inf,
                       filter_na = FALSE) %>%
    dplyr::distinct(`input`, .keep_all = TRUE) %>%
    dplyr::mutate(output = case_when(
      is.na(name) ~ processed_input,
      TRUE ~ name
    )) %>%
    dplyr::pull(output)
})

allTFs <- reactive({
  unique(dorothea()$source)
})

observe({
  if (!is.null(input$selectTF)) {
    dropdownSelected(TRUE)
  }
})

observe({
  visNetworkProxy("visNet_dorothea") %>%
    visSetData(nodes=nodes(), edges=edges())
})

edges <- reactive({
  req(inputTFs())
  
  if(dropdownSelected()){
    select_edges <- data()$from %in% input$selectTF
    edges1 <- data()[select_edges,]
  } else {
    select_edges <- data()$from %in% inputTFs()
    edges1 <- data()[select_edges,]
    
    output$tfFilter <- renderUI({
      selectizeInput("selectTF",
                     "Select TF",
                     choices = createChoices(inputTFs()),
                     #, selected = inputTFs()[1]
                     multiple = TRUE,
                     options = list(maxItems = 1)
      )
    })
  }
  
  if(input$sliderDegDorothea %in% c(2,3)){
    
    secDegTfs <- edges1$to[edges1$to %in% allTFs()]
    select_edges2 <- data()$from %in% secDegTfs
    select_edges_comb <- (select_edges | select_edges2)
    edges <- data()[select_edges_comb,]
    
    if(input$sliderDegDorothea == 3){
      
      thirdDegTfs <- edges$to[edges$to %in% allTFs()]
      select_edges3 <- data()$from %in% thirdDegTfs
      select_edges_comb <- (select_edges | select_edges2 | select_edges3)
      edges <- data()[select_edges_comb,]
      
    }
  } else {
    edges <- edges1
  }

  
  nrEdges <- dim(edges)[1]
  if(nrEdges >100){
    shinyalert::shinyalert(glue::glue("The network contains {nrEdges} Interactions! The network might be too cluttered."),
                           "For an easier inspection of the results you can pick interactions based on a single input TF using the dropdown menu in the right corner. 
                           Increasing the confidence level could also make the network more readable.",
                           type = "warning")
  }
  
  
  edges$arrows <- "to"
  edges$title <- glue::glue("confidence score: {edges$confidence}")
  edges$label <- glue::glue("{edges$confidence}")
  edges$dashes <- !(edges$from %in% inputTFs())
  edges$width <- abs(edges$mor)*1.5
  edges$color <- sapply(edges$mor, function(x) {
    switch(as.character(x),
           "1" = "green",
           "-1" = "red")
  })
  edges
})

nodes <- reactive({
  req(edges())
  
  nodes_subset <- unique(c(edges()$from, edges()$to))
  isTF <- nodes_subset %in% allTFs()
  group <- ifelse(isTF, "TF", "Target")
  
  data.frame(
    id = nodes_subset,
    label = nodes_subset,
    group = group,
    title = glue::glue(
      "<p style=\"font-weight: bold;\"><b> {nodes_subset} </b><br><a href='https://pubmed.ncbi.nlm.nih.gov/?term={nodes_subset}' target='_blank'>More Informations</a></p>"
    )
  )
})

edgeLabel <- reactive({
  lables <- c("Activation", "Repression")
  colors <- c("green", "red")
  edgeLabelColor <- data.frame(label = lables,
                               dashes = FALSE,
                               color = colors)
  
  # Slider " Radius" which extends the network to further downstream targets
  if(input$sliderDegDorothea %in% c(2,3)){
    edgeLabelDeg <- data.frame(
      label = c("1st Degree", "2nd Degree"),
      dashes = c(FALSE, TRUE),
      color = "black"
    )
  } else {
    edgeLabelDeg <- data.frame()
  }
  
  rbind(edgeLabelDeg,edgeLabelColor)
})

output$visNet_dorothea <- renderVisNetwork({
  
  # for debugging TfsSelection can be replace by inputTF()
  TfsSelection <-  inputTFs()[inputTFs() %in% nodes()$id]
  
  TFsNotInDoro <- inputTFs()[!(inputTFs() %in% nodes()$id)]
  
  #warning if the selected TF is not part of the dorothea network
  if(!dropdownSelected()){
    
    if(length(TfsSelection) == 0  ){
      shinyalert::shinyalert("Please enter valid TFs to the input field before pressing the RUN button",
                             type = "warning")
      return()
    } 
    
    if(length(TFsNotInDoro) != 0){
      shinyalert::shinyalert(glue::glue(' Transcription factors "{TFsNotInDoro}" is not contained in the Dorothea network'),
                             "Please check spelling and use gene symbols",
                             type = "warning")
    } 
  }
  
  
  visNetwork(
    nodes(),
    edges(),
    width = "100%",
    main = list(text = "Predicted TFs with their Target Genes", style = "font-family:Arial; font-size:25px; text-align:center; font-weight:bold; color:black;"),
    submain = list(text = "Interactions based on DoRothEA",
                   style = "font-family:Arial; color:black; font-size:19px; text-align:center; margin-top:5px;"),
    footer = list(text = "For more information click on node or edge", style = "font-family:Arial;font-size:12px;text-align:center; color: black;")
  )  %>%
    visGroups(
      groupname = "TF",
      color = list(border = "blue"),
      shape = "triangle",
      shadow = list(enabled = TRUE)
    ) %>%
    visGroups(
      groupname = "Target",
      color = list(border = "orange", background = "orange"),
      shape = "ellipse",
      shadow = list(enabled = TRUE)
    ) %>%
    visLayout(randomSeed = 12) %>%
    visOptions(
      highlightNearest = list(
        enabled = T,
        degree = 1,
        hover = T
      ),
      nodesIdSelection = list(enabled = TRUE, style = "margin: 5px 0px; border:none; outline:none;background: #f8f8f8;")
    )  %>%
    visOptions(nodesIdSelection = list(enabled = TRUE,
                                       values = TfsSelection)) %>%
    visLegend(addEdges = edgeLabel()) %>%
    visPhysics(stabilization = FALSE)
  
})


# download dorothea network as csv file
output$btnDownloadDorothea <- downloadHandler(
  
  filename = function() {
    paste("data-", Sys.Date(), ".csv", sep="")
  },
  content = function(file) {
    write.csv(edges()[,c(1:4)], file)
  }
)
