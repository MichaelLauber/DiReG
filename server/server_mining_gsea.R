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