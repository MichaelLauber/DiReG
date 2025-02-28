library(fgsea)

previousNodesGSEA <- reactiveVal(NULL)
cachedGeneSets <- reactiveValues(human = NULL, murine = NULL)
cachedResGseaDT <- reactiveValues(gsea_res_DTs  = NULL)
#comb_gsea_res <- reactiveVal(NULL)

cache_gsea <- reactiveValues(
  comb_gsea_res = NULL
)

loadGeneSets <- function(organism) {
  if (organism == "human") {
    if (is.null(cachedGeneSets$human)) {
      humanFiles <- list(
        H_Hallmark_Genesets  = file.path("data", "h.all.v2023.1.Hs.symbols.gmt"),
        C2_Curated_Genesets = file.path("data", "c2.cp.v2023.1.Hs.symbols.gmt"),
        C5_Ontology_Genesets  = file.path("data", "c5.go.v2023.1.Hs.symbols.gmt"),
        C8_CellType_signature_Genesets = file.path("data", "c8.all.v2023.1.Hs.symbols.gmt")
      )
      cachedGeneSets$human <- lapply(humanFiles, fgsea::gmtPathways)
    }
    return(cachedGeneSets$human)
  } else {
    if (is.null(cachedGeneSets$murine)) {
      murineFiles <- list(
        M_Hallmark_Genesets = file.path("data", "mh.all.v2023.1.Mm.symbols.gmt"),
        M2_Curated_Genesets = file.path("data", "m2.cp.v2023.1.Mm.symbols.gmt"),
        M5_Ontology_Genesets = file.path("data", "m5.go.v2023.1.Mm.symbols.gmt"),
        M8_CellType_signature_Genesets = file.path("data", "m8.all.v2023.1.Mm.symbols.gmt")
      )
      cachedGeneSets$murine <- lapply(murineFiles, fgsea::gmtPathways)
    }
    return(cachedGeneSets$murine)
  }
}

observeEvent(input$btnGSEA, {
  
  if(!networkCreated){
    shinyalert::shinyalert("There are no Genes to Analyse",
                           "Please input genes and press the RUN button before performing any analysis",
                           type = "error")
    return()}
  
  currentNodesGSEA <- nodes()$id
  if (identical(currentNodesGSEA, previousNodesGSEA())) {
    return()
  }

  waiter <- waiter::Waiter$new()
  waiter$show()
  on.exit(waiter$hide())
  
  notification <- showNotification(glue::glue("Calculating GSEA. This can take up to a few minutes"), type = "message", duration = NULL, closeButton = TRUE)
  on.exit(removeNotification(notification), add = TRUE)
  
  names_migsig_sets <- c("Hallmark Gene Sets", "Canonical Pathways Gene Sets", "Gene Ontology Gene Sets", "Cell Type Signature Gene Sets")
  
  
  organism <- ifelse(input$radioOrgDorothea == "human", "human", "murine")
  collection <- loadGeneSets(organism)
  
  gene_list <- getRanks(inputTFs(), data()) # change input TFs
  
  
  if (sum(gene_list != 0) < 500) {
    shinyalert::shinyalert(
      "Not Enough Target Genes",
      "The selected TFs do not have enough downstream targets. Add a lower confidence level.",
      type = "error"
    )
    return()
  }
    
    fgsea_results <- lapply(collection[1:2], function(pathways) {
      fgsea::fgsea(pathways = pathways, stats = gene_list) %>%
        dplyr::filter(padj <= 0.05) %>%
        dplyr::arrange(desc(abs(NES)))
    })
    
    
    gsea_res_DTs <- lapply(fgsea_results, function(res) {
      res %>%
        dplyr::select("pathway", "padj", "NES", "leadingEdge") %>%
        dplyr::mutate(padj = round(padj, 5), NES = round(NES, 2))
    })
    
    ###!!!!
    cache_gsea$comb_gsea_res <- do.call(rbind,gsea_res_DTs)
    
    cachedResGseaDT$gsea_res_DTs <- gsea_res_DTs
    
    gseaDTs <- lapply(gsea_res_DTs, function(dt){
      dt %>% dplyr::select("pathway","padj", "NES", "leadingEdge")} )
    
    
    plots <- lapply(fgsea_results, function(res) {
      # Take top 10 pathways based on NES
      top_res <- res %>%
        dplyr::arrange(desc(abs(NES))) %>%
        dplyr::slice(1:10)
      
      size_range <- c(5, 25)
      min_size <- min(top_res$size, na.rm = TRUE)
      max_size <- max(top_res$size, na.rm = TRUE)
      scaled_size <- (top_res$size - min_size) / (max_size - min_size) * (size_range[2] - size_range[1]) + size_range[1]
      
      # Create plotly scatter plot
      plot_ly(
        data = top_res,
        x = ~NES,
        y = ~reorder(pathway, NES),  # Reorder pathways for better display
        type = "scatter",
        mode = "markers",
        marker = list(
          size = scaled_size,  # Set the marker size based on the size variable
          color = ~padj,  # Set the marker color based on padj
          colorscale = "RdBu",
          reversescale = TRUE,
          cmin = 0,
          cmax = max(top_res$padj, na.rm = TRUE),
          showscale = TRUE,
          colorbar = list(
            title = "Adjusted p-value",  # Clear legend title
            titleside = "right",  # Position the title to the right
            font = list(size = 12)  # Customize the font size
          )
        ),
        text = ~paste(
          "Pathway:", pathway,
          "<br>NES:", round(NES, 2),
          "<br>Adjusted p-value:", round(padj, 5),
          "<br>Leading Edge Size:", size 
        ),
        hoverinfo = "text"
      ) %>%
        layout(
          title = "Top 10 Pathways",
          xaxis = list(title = "Normalized Enrichment Score"),
          yaxis = list(title = "Pathway"),
          colorbar = list(title = "Adjusted p-value")
        )
    })
    
    
    names(plots) <- names_migsig_sets[1:2] #CHANGE !!!!!!!
    
    
    screens_plot <- lapply(seq_along(plots), function(i) {
      shinyglide::screen(
        h3(names_migsig_sets[i], align = "center"),
        renderPlotly({ plots[[i]] }),
        renderDT({
          datatable(
            gseaDTs[[i]],
            options = list(
              scrollX = TRUE,  # Enable horizontal scrolling
              autoWidth = TRUE,  # Adjust column widths automatically
              columnDefs = list(
                list(targets = 3, render = JS(
                  "function(data, type, row, meta) {",
                  "return type === 'display' && data.length > 50 ?",
                  "'<div style=\"white-space: normal;\">' + data + '</div>' : data;",
                  "}"
                ))
              )
            ),
            escape = FALSE  # Allow HTML for wrapping text
          )
        })
      )
    })
    
    output$plot_gsea <- renderUI({
      do.call(glide, screens_plot)
    })
  previousNodesGSEA(currentNodesGSEA)
})


output$downloadDataGSEA <- downloadHandler(
  filename = function() {
    paste0("GSEA-", Sys.Date(), ".csv")
  },
  content = function(file) {
    req(cachedResGseaDT$gsea_res_DTs)
    
    # Combine all GSEA data tables into one data frame using bind_rows
    combined_gseaDT <- dplyr::bind_rows(
      lapply(names(cachedResGseaDT$gsea_res_DTs), function(name) {
        df <- cachedResGseaDT$gsea_res_DTs[[name]]
        df$Collection <- name  # Add a column to indicate the gene set collection
        df
      })
    )
    
    # Convert 'leadingEdge' from list to concatenated string
    combined_gseaDT <- combined_gseaDT %>%
      dplyr::mutate(
        leadingEdge = sapply(leadingEdge, function(x) paste(x, collapse = ", "))
      )
    
    # Write to CSV
    write.csv(combined_gseaDT, file, row.names = FALSE)
  }
)


#### LLM

observeEvent(input$submit_prompt_gsea_btn, {
  
  if(!key_uploaded()){
    showModal(modalDialog("Please Upload an API Key [For test purposes an API Key is provided by us]", easyClose = TRUE))
  }
  
  user_prompt <- paste0("Do the enriched genesets show specifity for:", 
                        input$user_prompt_gsea, 
                        "-These are the enriched genesets: ", 
                        paste(cache_gsea$comb_gsea_res$pathway, collapse = ", "))
  output$llm_response_gsea <- renderText("Generating response, please wait...")
  shinyjs::runjs("$('#llm_response_gsea').text('Generating response, please wait...');")
  
  ####Using openaiAPI
  
  data <- jsonlite::toJSON(list(
    model = api_settings()$preferred_model,  # Specify the model you want to use (e.g., "gpt-3.5-turbo" or "davinci")
    messages = list(
      list(role = "user", content = user_prompt)
    ),
    max_tokens = 1000
  ), auto_unbox = TRUE)
  
  response <- httr::POST(
    url,
    httr::add_headers(
      Authorization = paste("Bearer", api_settings()$api_key),
      `Content-Type` = "application/json"
    ),
    body = data,
    encode = "json"
  )
  

  # Check the response status
  if (httr::status_code(response) == 200) {
    # Parse the response
    content <- httr::content(response, as = "text", encoding = "UTF-8")
    parsed_content <- jsonlite::fromJSON(content)
    
    response_to_display <- parsed_content$choices$message$content

  } else {
    # Print an error message if the request fails
    cat("Error: Unable to retrieve a response. Status code:", status_code(response), "\n")
    response_to_display <- paste0("Error: Unable to retrieve a response. Status code:", status_code(response), "\n")
  }
  
  #####
  output$llm_response_gsea <- renderText({
    response_to_display
  })
})