
## dorothea

library(decoupleR)
dorothea_hs <- readRDS("data/dorothea_hs.rds")
dorothea_mm <- readRDS("data/dorothea_mm.rds")

dorothea <- reactive({
  if(input$radioOrgDorothea == "human"){
    dorothea_hs
  } else {
    dorothea_mm
  }
})

data <- reactive({
  dorothea() %>%
    filter(confidence %in% input$checkConfidence) %>%
    rename(from = source, to = target) %>%
    select(from, to, mor, confidence)
})


networkCreated <- FALSE

inputTFs <- eventReactive(input$btnCreateDoro, {
  networkCreated <<- TRUE
  stringr::str_split(input$inputTextTFs, "\\s+") %>%  unlist() 
})

  
observe({
  print("changed_observed")
  visNetworkProxy("visNet_dorothea") %>% 
    visSetData(nodes=nodes(), edges=edges())
})  

 
allTFs <- reactive({
  unique(dorothea()$source)
})


edges <- reactive({
  
  select_edges <- data()$from %in% inputTFs()
  edges1 <- data()[select_edges,]
  
  # if(is.null(input$selectTF)){
  #   output$tfFilter <- renderUI({
  #     selectizeInput("selectTF", 
  #                    "Select TF", 
  #                    choices = createChoices(inputTFs()),
  #                    #, selected = inputTFs()[1]
  #                    multiple = TRUE, 
  #                    options = list(maxItems = 1)
  #     )
  #   })
  # }
  
  # if(is.null(input$selectTF)){
  #   print("is Null")
  # }
    

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
    # tfFilter is a small dropdown menu at the right corner that allows to restrict the network to a specific TF 
    # output$tfFilter <- renderUI({
    #   selectizeInput("selectTF", 
    #                  "Select TF", 
    #                   choices = createChoices(inputTFs()),
    #               #, selected = inputTFs()[1]
    #               multiple = TRUE, 
    #               options = list(maxItems = 1)
    #               )
    # })
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

    # for debugging TfsSelection can be replace by inputTF()
    TfsSelection <-  inputTFs()[inputTFs() %in% nodes()$id]
    
    TFsNotInDoro <- inputTFs()[!(inputTFs() %in% nodes()$id)]
    
    if(length(TfsSelection) == 0 || input$inputTextTFs == "Enter TFs..." ){
      shinyalert::shinyalert("Please enter TFs to the input field before pressing the RUN button",
                             type = "warning")
      return()
    } 
    
    if(length(TFsNotInDoro) != 0){
      shinyalert::shinyalert(glue::glue(' Transcription factors "{TFsNotInDoro}" is not contained in the Dorothea network'),
                             "Please check spelling and use gene symbols",
                             type = "warning")
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



#OR analysis

observeEvent(input$btnORA,{
  
  if(!networkCreated){
    shinyalert::shinyalert("There are no Genes to Analyse",
                           "Please input genes and press the RUN button before performing any analysis",
                           type = "error")
    return()}
  
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
  
})



##GSEA Analysis

source("utils/utils_enrichment.R")
library(fgsea)


#remove after debugging
# tbl <- vroom::vroom("~/R-Packages/DiReG/data/reprogramming_protocols.csv")
# prots <- tbl %>%
#   filter(Target == "Hepatocyte") %>%
#   select(Protocol)
# prot <- prots[4,1] %>%  unlist()
#inputTFs <- strsplit(prot, "\\|") %>%  unlist()

observeEvent(input$btnGSEA, {
  
  if(!networkCreated){
    shinyalert::shinyalert("There are no Genes to Analyse",
                           "Please input genes and press the RUN button before performing any analysis",
                           type = "error")
    return()}

  waiter <- waiter::Waiter$new()
  waiter$show()
  on.exit(waiter$hide())

  notification <- showNotification(glue::glue("Calculating GSEA. This can take up to a few minutes"), type = "message", duration = NULL, closeButton = TRUE)
  on.exit(removeNotification(notification), add = TRUE)

  if(!exists("migSDBm8")) {migSDBm8 <- file.path("data", "m8.all.v2023.1.Mm.symbols.gmt")}

  if(!exists("migSDBc8")) {migSDBc8 <- file.path("data", "c8.all.v2023.1.Hs.symbols.gmt")}

  if(input$radioOrgDorothea == "human"){
    collection <- fgsea::gmtPathways(migSDBc8)
  } else {
    collection <- fgsea::gmtPathways(migSDBm8)
  }

  gene_list <- getRanks(inputTFs(), data()) # change input TFs
  if(sum(gene_list != 0) < 500) {
    shinyalert::shinyalert("Not Enough Target Genes",
    "The selected TFs do not have enough downstream targets. Add a lower confidence level",
    type = "error")
  } else {



  fgRes <- fgsea::fgsea(pathways = collection,
                        stats = gene_list,
                        #minSize=15, ## minimum gene set size
                        #maxSize=400, ## maximum gene set size
                        ) %>%
                        filter(padj <= 0.05) %>%
                        arrange(desc(abs(NES)))


  output$tbl_gsea <- renderDT({
    fgRes %>%
      select("pathway","padj", "NES",  "leadingEdge" ) %>%
      mutate(padj = round(padj, 5)) %>%
      mutate(NES = round(NES, 2))

  })


  output$plot_gsea <- renderPlot({

    header = "Top 10 Pathways"

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
           title=header)
  })

  }

})


## GTEx expression

ensg_to_hgnc <- readRDS("data/ensg_to_hgnc.rds")

sample.df <- read.delim("data/GTEx_Analysis_v8_Annotations_SampleAttributesDS.txt", as.is=TRUE, header=TRUE, row.names=1)
rnaseq.sample.df <- sample.df[sample.df['SMAFRZE']=='RNASEQ', ]
mapping <- rnaseq.sample.df %>%
  select(SMTS, SMTSD)
mapping$id <- rownames(rnaseq.sample.df)

library(slickR)
# Create content for the carousel

observeEvent(input$btnGTEx, ({
  
  if(!networkCreated){
    shinyalert::shinyalert("There are no Genes to Analyse",
                           "Please input genes and press the RUN button before performing any analysis",
                           type = "error")
    return()}
  
  symbols <- c(inputTFs())
  
  nrFigs <- length(symbols)
  
  print(symbols)
  
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
    df$tissue2 <- mapping$SMTSD[match(df$id, mapping$id)]
    df$tissue3  <- stringr::str_split_fixed(df$tissue2, "-", 2)[,1]
    
    shinyglide::screen(
      h3(symbols[i], align="center"),
      renderPlotly({
        
        
        df %>% plotly::plot_ly(
          y = ~values,
          #y = ~logval,
          x = ~tissue,
          type= 'violin',
          split = ~tissue3,
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


if(!exists("gtexTPM")){
  gtexTPM <- readRDS(file.path("data","gtexTPM.rds"))
}
tissues <- colnames(gtexTPM)[-c(1:2)]

vals <- reactiveValues(data = NULL)

dataModal <- function(failed = FALSE) {
  modalDialog(
    selectizeInput("selectTissueTFAStart", h3("Start Tissue"),
                   choices = createChoices(tissues),
                   selected=NULL,
                   width = "230px"),
    
    selectizeInput("selectTissueTFATarget", h3("Target Tissue"),
                   choices = createChoices(tissues),
                   selected=NULL,
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

##TFA

observeEvent(input$btnTFA, {
  
  if(!networkCreated){
    shinyalert::shinyalert("There are no Genes to Analyse",
                           "Please input genes and press the RUN button before performing any analysis",
                           type = "error")
    return()}

  showModal(dataModal())
  
  observeEvent(input$ok, {
    
    removeModal()
    
    waiter <- waiter::Waiter$new()
    waiter$show()
    on.exit(waiter$hide())
    
    notification <- showNotification(glue::glue("Calcutlating TFAs"), type = "message", duration = NULL, closeButton = TRUE)
    on.exit(removeNotification(notification), add = TRUE)
    
    celltype1 <- input$selectTissueTFAStart #change to input$
    celltype2 <- input$selectTissueTFATarget #change to input$
    
    dups <- duplicated(gtexTPM$genes)
    counts <- gtexTPM[!dups,] %>%
      select(celltype1, celltype2) %>%
      as.matrix()
    
    rownames(counts) <- gtexTPM[!dups,] %>% pull("genes")
    
    res <- decouple(mat=counts, #remove after debugging!
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
      
      max <- max(c(combined$score.x, combined$score.y))
      min <- min(c(combined$score.x, combined$score.y))
      
      xtitle <- unique(combined$condition.x)
      ytitle <- unique(combined$condition.y)
      
      plot_ly(combined,
              x = ~score.x,
              y = ~score.y,
              marker = list(size = 10,
                            color = ~greaterPval
                            #color = 'rgba(255, 182, 193, .9)',
                            #line = list(color = 'rgba(152, 0, 0, .8)', width = 2)
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
               yaxis = list(title = paste0("TFA in ", ytitle))
        )
      
      
    })
    
  })
  

  
  
})






# output$btnDownloadEnrich <- downloadHandler(
#   print(enrichData$result[1:5,]),
#   filename = function() {
#     paste("data-", Sys.Date(), ".csv", sep="")
#   },
#   content = function(file) {
#     vroom::vroom_write(enrichData$result, file)
#   }
# )



# gostres <- gprofiler2::gost(query = c("X:1000:1000000", "rs17396340", "GO:0005005", "ENSG00000156103", "NLRP1"),
#                             organism = "hsapiens", ordered_query = FALSE,
#                             multi_query = FALSE, significant = TRUE, exclude_iea = FALSE,
#                             measure_underrepresentation = FALSE, evcodes = FALSE,
#                             user_threshold = 0.05, correction_method = "g_SCS",
#                             domain_scope = "annotated", custom_bg = NULL,
#                             numeric_ns = "", sources = c("GO:MF", "GO:CC", "GO:BP", "KEGG","REAC"), as_short_link = FALSE)





