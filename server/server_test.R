
## dorothea

library(decoupleR)
dorothea_hs <- readRDS("data/dorothea_hs.rds")
dorothea_mm <- readRDS("data/dorothea_mm.rds")

# tri_hs <- OmnipathR::collectri(organism = 9606)
# tri_mm <- OmnipathR::collectri(organism = 10090)
# 
# dorothea <- reactive({
#   if(input$radioOrgDorothea == "human"){
#     tri_hs
#   } else {
#     tri_mm
#   }
# })

dorothea <- reactive({
  if(input$radioOrgDorothea == "human"){
    dorothea_hs
  } else {
    dorothea_mm
  }
})

organism <- reactive({
  if(input$radioOrgDorothea == "human"){
    "hsapiens"
  } else {
    "mmusculus"
  }
})



data <- reactive({
  dorothea() %>%
    #filter(confidence %in% input$checkConfidence) %>%
    rename(from = source_genesymbol, to = target_genesymbol) %>%
    select(from, to, mor, confidence)
})


data <- reactive({
  dorothea() %>%
    filter(confidence %in% input$checkConfidence) %>%
    rename(from = source, to = target) %>%
    select(from, to, mor, confidence)
})


networkCreated <- FALSE

btnCreateDoroPressed <- reactiveVal(FALSE)

inputTFs <- eventReactive(input$btnCreateDoro, {
  networkCreated <<- TRUE
  btnCreateDoroPressed(TRUE)
  cond_visnet(0)
  shinyjs::runjs(sprintf('window.cond_visnet = "%s"', cond_visnet()))
  
  split_result <- stringr::str_split(input$inputTextTFs, "\\s+") %>%  unlist()
  input <- split_result[split_result != ""] 
  gprofiler2::gconvert(query = input,
                         organism = organism(), 
                         target="ENSG", 
                         mthreshold = Inf, 
                         filter_na = FALSE) %>% 
    mutate(output = case_when(
      is.na(name) ~ input,
      TRUE ~ name
    )) %>%
    pull(output)
  
})

  
observe({
  print("changed_observed")
  print(btnCreateDoroPressed)
  print(networkCreated)
  
  visNetworkProxy("visNet_dorothea") %>% 
    visSetData(nodes=nodes(), edges=edges())
})  

 
allTFs <- reactive({
  unique(dorothea()$source)
})

dropdownSelected <- reactiveVal(FALSE)

observe({
  if (!is.null(input$selectTF)) {
    dropdownSelected(TRUE)
  }
})


edges <- reactive({
  
  if(dropdownSelected()){
    select_edges <- data()$from %in% input$selectTF
    print(input$selectTF)
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
  edges$width <- 2
  edges$width <- abs(edges$mor)*1.5
  edges$color <- sapply(edges$mor, function(x) {
    switch(as.character(x),
           "1" = "green",
           "-1" = "red")
  })
  edges
})



nodes <- reactive({
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
  
  print(inputTFs())

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


previousNodes <- reactiveVal(NULL)
#OR analysis

observeEvent(input$btnORA,{
  
  if(!networkCreated){
    shinyalert::shinyalert("There are no Genes to Analyse",
                           "Please input genes and press the RUN button before performing any analysis",
                           type = "error")
    return()}
  
  currentNodes <- nodes()$id
  
  
  if (identical(currentNodes, previousNodes())) {
    
    return()
  } 
  
  waiter <- waiter::Waiter$new()
  waiter$show()
  on.exit(waiter$hide())
   
  notification <- showNotification(glue::glue("Overrepresentation Analysis running. This can take a few seconds."), type = "message", duration = NULL, closeButton = TRUE)
  on.exit(removeNotification(notification), add = TRUE)
  
  if(input$radioOrgDorothea == "human"){
    organism <- "hsapiens"
  } else {
    organism <- 'mmusculus'
  }
  
  enrichData <<-
    gprofiler2::gost(query = nodes()$id,
                     organism = organism, ordered_query = FALSE,
                     multi_query = FALSE, significant = TRUE, exclude_iea = FALSE,
                     measure_underrepresentation = FALSE, evcodes = FALSE,
                     user_threshold = 0.05, correction_method = "g_SCS",
                     domain_scope = "annotated", custom_bg = NULL,
                     numeric_ns = "", sources = c("GO:MF", "GO:CC", "GO:BP", "KEGG","REAC"), as_short_link = FALSE)
  
  
  
  output$enrichPlot <- renderPlotly({
    gprofiler2::gostplot(enrichData, capped = TRUE, interactive = TRUE)
  })
  
  output$tbl_enrich <- renderDT(
    enrichData$result[,c(11,3:6,9, 10)],
    options = list(pageLength = 5)
  )
  previousNodes(currentNodes)
})



output$downloadDataORA <- downloadHandler(
  filename = function() {
    paste("ORA-", Sys.Date(), ".csv", sep="")
  },
  content = function(file) {
    write.csv(enrichData$result[,c(11,3:6,9, 10)], file)
  }
)



##GSEA Analysis

source("utils/utils_enrichment.R")
library(fgsea)


previousInputTFs <- reactiveVal(NULL)


observeEvent(input$btnGSEA, {
  
  if(!networkCreated){
    shinyalert::shinyalert("There are no Genes to Analyse",
                           "Please input genes and press the RUN button before performing any analysis",
                           type = "error")
    return()}
  
  currentTFs <- inputTFs()

  
  if (identical(currentTFs, previousInputTFs())) {
    
    return()
  } 

  waiter <- waiter::Waiter$new()
  waiter$show()
  on.exit(waiter$hide())

  notification <- showNotification(glue::glue("Calculating GSEA. This can take up to a few minutes"), type = "message", duration = NULL, closeButton = TRUE)
  on.exit(removeNotification(notification), add = TRUE)

  if(!exists("migSDBmHallmark")) {migSDBmHallmark <- file.path("data", "mh.all.v2023.1.Mm.symbols.gmt")}
  if(!exists("migSDBm2Cp")) {migSDBm2Cp <- file.path("data", "m2.cp.v2023.1.Mm.symbols.gmt")}
  if(!exists("migSDBm5go")) {migSDBm5go <- file.path("data", "m5.go.v2023.1.Mm.symbols.gmt")}
  if(!exists("migSDBm8")) {migSDBm8 <- file.path("data", "m8.all.v2023.1.Mm.symbols.gmt")}
  

  if(!exists("migSDBhsHallmark")) {migSDBhsHallmark <- file.path("data", "h.all.v2023.1.Hs.symbols.gmt")}
  if(!exists("migSDBc2Cp")) {migSDBc2Cp <- file.path("data", "c2.cp.v2023.1.Hs.symbols.gmt")}
  if(!exists("migSDBc5go")) {migSDBc5go <- file.path("data", "c5.go.v2023.1.Hs.symbols.gmt")}
  if(!exists("migSDBc8")) {migSDBc8 <- file.path("data", "c8.all.v2023.1.Hs.symbols.gmt")}
  
  names_migsig_sets <- c("Hallmark Gene Sets", "Canonical Pathways Gene Sets", "Gene Ontology Gene Sets", "Cell Type Signature Gene Sets")
  murine_migsigDbs <- c(migSDBmHallmark, migSDBm2Cp, migSDBm5go, migSDBm8)
  human_migsigDbs <- c(migSDBhsHallmark, migSDBc2Cp, migSDBc5go, migSDBc8)


  #CHANGE !!!!!!!
  if(input$radioOrgDorothea == "human"){
    collection <- lapply(human_migsigDbs[1:2], function(x){fgsea::gmtPathways(x)})
  } else {
    collection <- lapply(murine_migsigDbs[1:2], function(x){fgsea::gmtPathways(x)})
  }

  gene_list <- getRanks(inputTFs(), data()) # change input TFs
  if(sum(gene_list != 0) < 500) {
    shinyalert::shinyalert("Not Enough Target Genes",
    "The selected TFs do not have enough downstream targets. Add a lower confidence level",
    type = "error")
  } else {

  fgsea_results <- lapply(collection, function(x){
    fgsea::fgsea(pathways = x,
                                       stats = gene_list,
                                       #minSize=15, ## minimum gene set size
                                       #maxSize=400, ## maximum gene set size
                                       ) %>%
                                       filter(padj <= 0.05) %>%
                                       arrange(desc(abs(NES)))
  })

  
  gsea_res_DTs <- lapply(fgsea_results, function(fgRes){
    fgRes %>%
      select("pathway","padj", "NES",  "leadingEdge" ) %>%
      mutate(padj = round(padj, 5)) %>%
      mutate(NES = round(NES, 2))
  })
  gsea_res_DT <<- gsea_res_DTs[[1]]
  
  gseaDTs <<- lapply(gsea_res_DTs, function(gsea_res_DT){
    gsea_res_DT %>% select("pathway","padj", "NES")} )


  plots <- lapply(fgsea_results, function(fgRes){
    fgRes %>%
      arrange(desc(abs(NES))) %>%
      slice(1:10) %>%
      mutate(pathway=factor(pathway, levels = pathway)) %>%
      ggplot(aes(x=NES,
                 y=pathway,
                 size=size,
                 #shape=Enrichment,
                 color=padj
      )) +
      geom_point() +
      scale_size(range=c(3, 6)) +
      scale_colour_gradient(low = "red", high = "blue")+
      #guides(size = TRUE) +
      theme_bw() +
      labs(y="Pathway", x="Normalized Enrichment Score",
           title="Top 10 Pathways")
  })
  
  #CHANGE !!!!!!!
  names(plots) <- names_migsig_sets[1:2]
  screens_plot <- lapply(seq_along(plots), function(i){
    
    shinyglide::screen(
      h3(names_migsig_sets[i], align="center"),
      renderPlot({plots[i]}),
      renderDT({
        gseaDTs[[i]]
      })
    )
  })
  
  output$plot_gsea <- renderUI({
    do.call(glide, screens_plot)
  })
  }
  previousInputTFs(currentTFs)
})

output$downloadDataGSEA<- downloadHandler(
  filename = function() {
    paste("GSEA-", Sys.Date(), ".csv", sep="")
  },
  content = function(file) {
    write.csv(do.call(rbind,gseaDTs), file)
  }
)




## GTEx expression

ensg_to_hgnc <- readRDS("data/ensg_to_hgnc.rds")

sample.df <- read.delim("data/GTEx_Analysis_v8_Annotations_SampleAttributesDS.txt", as.is=TRUE, header=TRUE, row.names=1)
rnaseq.sample.df <- sample.df[sample.df['SMAFRZE']=='RNASEQ', ]
mapping <- rnaseq.sample.df %>%
  select(SMTS, SMTSD)
mapping$id <- rownames(rnaseq.sample.df)

library(slickR)
# Create content for the carousel

previousGTEXInputTFs <- reactiveVal(NULL)

observeEvent(input$btnGTEx, ({
  
  if(!networkCreated){
    shinyalert::shinyalert("There are no Genes to Analyse",
                           "Please input genes and press the RUN button before performing any analysis",
                           type = "error")
    return()}
  
  
  currentTFs <- inputTFs()
  
  if (identical(currentTFs, previousGTEXInputTFs())) {
    
    return()
  } 
  
  symbols <- c(inputTFs())
  
  nrFigs <- length(symbols)
  
  #print(symbols)
  
  screens <- lapply(1:nrFigs, function(i){
    
    print(symbols[i])
    
    gene_name <- ensg_to_hgnc %>%
      filter(HGNC == symbols[i]) %>%
      select(ENSG)
    
    filename <- paste0(gene_name, ".rds")
    
    print(filename)
    
    gene <- readRDS(file.path("~/gtex_splitted",filename))
    
    df <- data.frame(id=names(gene), values= gene)
    df$tissue <- mapping$SMTSD[match(df$id, mapping$id)]
    #df$tissue2 <- mapping$SMTSD[match(df$id, mapping$id)]
    #df$tissue3  <- stringr::str_split_fixed(df$tissue2, "-", 2)[,1]
    
    shinyglide::screen(
      h3(symbols[i], align="center"),
      renderPlotly({
        
        
        df %>% plotly::plot_ly(
          y = ~values,
          #y = ~logval,
          x = ~tissue,
          type= 'violin',
          split = ~tissue,
          #split = ~tissue3,
          box = list(
            visible = T
          ),
          meanline = list(
            visible = T
          ),
          spanmode = "hard",
          marker = list(
            size = 3
          )
        ) %>%
          layout(
            xaxis = list(
              title = paste0(i,"/",nrFigs),
              tickangle = 45,
              spanmode = "hard"
            ),
            yaxis = list(
              title = "TPM",
              zeroline = F
            ),
            showlegend = FALSE
          )
        
      })
    )
  })
  
 
  output$carousel <- renderUI({
    
    do.call(glide, screens)
    
    
  })
  previousGTEXInputTFs(currentTFs)
})
)



##isoforms



observeEvent(input$btnIsoforms, {

  if(!networkCreated){
    shinyalert::shinyalert("There are no Genes to Analyse",
                           "Please input genes and press the RUN button before performing any analysis",
                           type = "error")
    return()}
  
  if(input$radioOrgDorothea == "mouse"){
    shinyalert::shinyalert("Not Supported for Mouse data",
                           "We are sorry, but the Differention Potential calculations are based on human data only",
                           type = "error")
  }

  if(!exists("isoformTFs_pseudotimes")){
  isoformTFs_pseudotimes <- vroom::vroom(file.path("data", "isoformTFs_pseudotimes.tsv"))
  }

  TFs <- inputTFs()

  output$isoforms_plot <- renderPlotly({

    # change here

    subset <- isoformTFs_pseudotimes %>%
      filter(TFName %in% TFs)

    nrGroups <- length(unique(subset$TFName))
    repsGroup <- as.numeric(table(subset$TFName))

    traces1 <- seq(1,nrGroups*2,by=2)
    traces2 <- seq(2,nrGroups*2,by=2)

    subset %>%
      group_by(TFName) %>%
      do(p=plot_ly(., x = ~IsoForm, y = ~diffusionZscore,  type = "bar", name = 'Diffusion', showlegend = F,hovertext = ~ENST) %>%
           add_trace(y = ~velocityZscore, name = 'Velocity', showlegend = F) #%>% layout(showlegend = FALSE)
      ) %>%
      subplot(nrows = 1, shareX = TRUE, shareY = TRUE, titleX = FALSE) %>%
      style(marker = list(color = c("#4b4896")), traces = traces1, name = 'Diffusion') %>%
      style(marker = list(color = c("#fc7f03")), traces = traces2, name = 'Velocity') %>%
      style(traces = c(1,2), showlegend=TRUE) %>%
      layout(
        title =  list(text = '<b>Differention Potential</b>'),
        yaxis = list(
          title = "Potential Z-Score"
        ),
        margin = list(l = 50, r = 50,
                      b = 50, t = 50,
                      pad = 20)
      )

  })
})


##TFA

if(!exists("gtexTPM")){
  gtexTPM <- readRDS(file.path("data","gtexTPM.rds"))
}
tissues <- colnames(gtexTPM)[-c(1:2)]

vals <- reactiveValues(data = NULL)

dataModal <- function(failed = FALSE) {
  modalDialog(
    selectizeInput("selectTissueTFAStart", h3("Start Tissue"),
                   choices = createChoices(tissues),
                   selected= "Cells - Cultured fibroblasts",
                   width = "230px"),
    
    selectizeInput("selectTissueTFATarget", h3("Target Tissue"),
                   choices = createChoices(tissues),
                   selected="Liver",
                   width = "230px"),
    
    span('Compare TF activities between two different tissues'),
    if (failed)
      div(tags$b("Invalid name of data object", style = "color: red;")),
    
    footer = tagList(
      modalButton("Cancel"),
      actionButton("ok", "OK")
    )
  )
  

  
}


observeEvent(input$selectTissueTFAStart, {
  if(input$selectTissueTFAStart == input$selectTissueTFATarget){
  updateSelectizeInput(session, "selectTissueTFATarget", choices = setdiff(createChoices(tissues), input$selectTissueTFAStart))
  }
})

observeEvent(input$selectTissueTFATarget, {
  if(input$selectTissueTFAStart == input$selectTissueTFATarget){
  updateSelectizeInput(session, "selectTissueTFAStart", choices = setdiff(createChoices(tissues), input$selectTissueTFATarget))
  }
})





observeEvent(input$btnTFA, {
  
  if(!networkCreated){
    shinyalert::shinyalert("There are no Genes to Analyse",
                           "Please input genes and press the RUN button before performing any analysis",
                           type = "error")
    return()}
  
  removeModal()

  showModal(dataModal())
  
})  
  
  observeEvent(input$ok, {
    
    removeModal()
    
    waiter <- waiter::Waiter$new()
    waiter$show()
    on.exit(waiter$hide())
    
    notification <- showNotification(glue::glue("Calcutlating TFAs"), type = "message", duration = NULL, closeButton = TRUE)
    on.exit(removeNotification(notification), add = TRUE)
    
    celltype1 <- input$selectTissueTFAStart #change to input$
    celltype2 <- input$selectTissueTFATarget #change to input$
    
    #for debugging
    # celltype1 <- "Bladder"
    # celltype2 <- "Liver" 
    
    dups <- duplicated(gtexTPM$genes)
    counts <- gtexTPM[!dups,] %>%
      select(celltype1, celltype2) %>%
      as.matrix()
    
    rownames(counts) <- gtexTPM[!dups,] %>% pull("genes")
    
    res <- decoupleR::decouple(mat=counts, #remove after debugging!
                    net=data(),
                    .source='from',
                    .target='to',
                    args = list(
                      # wsum = list(.mor = "mor"),
                      # ulm = list(.mor = "mor"),
                      mlm = list(.mor = "mor")
                    ),
                    minsize = 5) %>%
      mutate(score = round(score, 2)) %>%
      filter(statistic == "consensus") %>%
      select(-run_id)
    
    
    output$tbl_tfa <- renderDT({
      
      res %>%
        DT::datatable() %>%
        formatSignif(columns = c('p_value'), digits = 3)
      
    })
    
    
    output$tfa_plot <-  renderPlotly({
      
      sample_acts <- res %>% filter(statistic == "consensus")
      splitted_by_tissue <- split(sample_acts, sample_acts$condition)
      combined <- inner_join(splitted_by_tissue[[1]], splitted_by_tissue[[2]], by=c("source", "statistic"))
      combined$greaterPval <- 1/(ifelse(combined$p_value.x < combined$p_value.y, combined$p_value.y, combined$p_value.x))
      combined$symbol <- ifelse(combined$source %in% inputTFs(),  'diamond', 'circle')
      combined$lineColor <- ifelse(combined$source %in% c("inputTFs()"),  'red', 'white')
      
      max <- max(c(combined$score.x, combined$score.y))
      min <- min(c(combined$score.x, combined$score.y))
      
      xtitle <- unique(combined$condition.x)
      ytitle <- unique(combined$condition.y)
      plot_ly(combined,
              x = ~score.x,
              y = ~score.y,
              marker = list(size = 10,
                            symbol = ~symbol,
                            color = ~greaterPval,
                            #color = 'rgba(255, 182, 193, .9)',
                            line = list(color = ~lineColor, width = 2)
              ),
              text = ~paste(combined$source,
                            "<br>",
                            "p value in", combined$condition.x,": ", combined$p_value.x,
                            '<br>',
                            "p value in", combined$condition.y,": ", combined$p_value.y
              )
      ) %>%
        layout(shapes =
                 list(
                   list(
                     type = "line",
                     x0 = min,
                     x1 = max,
                     xref = "x",
                     y0 = min,
                     y1 = max,
                     yref = "y",
                     line = list(color = "black", width = 1, dash = 'dash')
                   )
                 )
               ,
               xaxis = list(title = paste0("TFA in ", xtitle)),
               yaxis = list(title = paste0("TFA in ", ytitle)),
               annotations = list(
                 list(x = 0.05, y = 1, xref = 'paper', yref = 'paper', text = 'Diamond: Input TF', showarrow = FALSE, align = 'left'),
                 list(x = 0.05, y = 0.95, xref = 'paper', yref = 'paper', text = 'Color: p-value', showarrow = FALSE, align = 'left')
               )
        )
      
      
    })
    
  })




