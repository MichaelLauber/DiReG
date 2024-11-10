  # Initialize reactive values for caching GSEA data
  gseaCache <- reactiveValues(
    previousNodesGSEA = NULL,
    cachedGeneSets = list(human = NULL, murine = NULL),
    gseaResults = NULL  # To store processed GSEA results
  )
  
  # Function to load gene sets based on organism
  loadGeneSets <- function(organism) {
    if (organism == "human") {
      if (is.null(gseaCache$cachedGeneSets$human)) {
        humanFiles <- list(
          Hallmark_Genesets = file.path("data", "h.all.v2023.1.Hs.symbols.gmt"),
          C2_Curated_Genesets = file.path("data", "c2.cp.v2023.1.Hs.symbols.gmt"),
          C5_Ontology_Genesets = file.path("data", "c5.go.v2023.1.Hs.symbols.gmt"),
          C8_CellType_signature_Genesets = file.path("data", "c8.all.v2023.1.Hs.symbols.gmt")
        )
        gseaCache$cachedGeneSets$human <- lapply(humanFiles, fgsea::gmtPathways)
      }
      return(gseaCache$cachedGeneSets$human)
    } else {
      if (is.null(gseaCache$cachedGeneSets$murine)) {
        murineFiles <- list(
          Hallmark_Genesets = file.path("data", "mh.all.v2023.1.Mm.symbols.gmt"),
          C2_Curated_Genesets = file.path("data", "m2.cp.v2023.1.Mm.symbols.gmt"),
          C5_Ontology_Genesets = file.path("data", "m5.go.v2023.1.Mm.symbols.gmt"),
          C8_CellType_signature_Genesets = file.path("data", "m8.all.v2023.1.Mm.symbols.gmt")
        )
        gseaCache$cachedGeneSets$murine <- lapply(murineFiles, fgsea::gmtPathways)
      }
      return(gseaCache$cachedGeneSets$murine)
    }
  }
  
  # Function to perform GSEA
  performGSEA <- function(selectedGeneSets, geneList, names_migsig_sets) {
    # Show loading indicator
    waiter::waiter_show()
    on.exit(waiter::waiter_hide(), add = TRUE)
    
    # Show notification
    notification <- showNotification(
      "Calculating GSEA. This can take up to a few minutes.",
      type = "message", duration = NULL, closeButton = TRUE
    )
    on.exit(removeNotification(notification), add = TRUE)
    
    print("Gsea run")
    # Perform fgsea for each selected gene set collection
    fgsea_results <- lapply(selectedGeneSets, function(pathways) {
      fgsea::fgsea(
        pathways = pathways,
        stats = geneList,
        nperm = 1000  # You can adjust the number of permutations
      ) %>%
        dplyr::filter(padj <= 0.05) %>%
        dplyr::arrange(desc(abs(NES)))
    })
    print("Gsea finished")
    #print(fgsea_results)
    
    # Process results into data tables
    gsea_res_DTs <- lapply(fgsea_results, function(res) {
      res %>%
        dplyr::select("pathway", "padj", "NES", "leadingEdge", "size") %>%
        dplyr::mutate(
          padj = round(padj, 5),
          NES = round(NES, 2)
        )
    })
    print("Gsea Table")
    print(gsea_res_DTs)
    
    # Assign names to the results based on the gene set collection
    names(gsea_res_DTs) <- names_migsig_sets
    
    # Store processed results in reactiveValues
    gseaCache$gseaResults <- list(
      fgsea_results = fgsea_results,
      gseaDTs = gsea_res_DTs
    )
  }
  
  # Observe Event for GSEA Button
  observeEvent(input$btnGSEA, {
    # Check if the network is created
    if (!networkCreated) {
      shinyalert::shinyalert(
        title = "No Genes to Analyze",
        text = "Please input genes and press the RUN button before performing any analysis.",
        type = "error"
      )
      return()
    }
    
    # Retrieve current nodes
    currentNodesGSEA <- nodes()$id  # Ensure 'nodes()$id' is defined and reactive
    
    # Check if the current nodes are identical to the previous ones
    if (identical(currentNodesGSEA, gseaCache$previousNodesGSEA)) {
      shinyalert::shinyalert(
        title = "No Changes Detected",
        text = "The current gene list is identical to the previous one. No new analysis is performed.",
        type = "info"
      )
      return()
    }
    
    # Load gene sets based on the selected organism
    organism <- ifelse(input$radioOrgDorothea == "human", "human", "murine")
    geneSetsCollection <- loadGeneSets(organism)
    
    # Define the names for gene set collections (adjust as needed)
    names_migsig_sets <- c(
      "Hallmark Gene Sets",
      "Canonical Pathways Gene Sets",
      "Gene Ontology Gene Sets",
      "Cell Type Signature Gene Sets"
    )
    
    # Retrieve gene list rankings
    gene_list <- getRanks(inputTFs(), data())  # Ensure 'getRanks', 'inputTFs()', and 'data()' are defined
    
    # Validate gene list
    if (sum(gene_list != 0, na.rm = TRUE) < 500) {
      shinyalert::shinyalert(
        title = "Not Enough Target Genes",
        text = "The selected TFs do not have enough downstream targets. Add a lower confidence level.",
        type = "error"
      )
      return()
    }
    
    # Select specific gene set collections for analysis (adjust indices as needed)
    selectedGeneSets <- geneSetsCollection[1:2]  # Example: selecting Hallmark and C2 Curated Gene Sets
    selectedNames <- names_migsig_sets[1:2]
    
    # Perform GSEA
    performGSEA(selectedGeneSets, gene_list, selectedNames)
    
    # Update previous nodes
    gseaCache$previousNodesGSEA <- currentNodesGSEA
  })
  
  # UI Output for GSEA Plots and Tables
  output$plot_gsea <- renderUI({
    req(gseaCache$gseaResults)
    
    screens_plot <- lapply(seq_along(gseaCache$gseaResults$gseaDTs), function(i) {
      shinyglide::screen(
        h3(names(gseaCache$gseaResults$gseaDTs)[i], align = "center"),
        plotlyOutput(paste0("plot_gsea_", i)),
        DTOutput(paste0("tbl_gsea_", i))
      )
    })
    
    # Render all screens within a glide
    shinyglide::glide(
      do.call(shinyglide::screen, screens_plot)
    )
  })
  
  # Render GSEA Plots and Tables Dynamically
  observe({
    req(gseaCache$gseaResults)
    
    for (i in seq_along(gseaCache$gseaResults$gseaDTs)) {
      local({
        my_i <- i  # Local copy for each iteration
        plot_id <- paste0("plot_gsea_", my_i)
        tbl_id <- paste0("tbl_gsea_", my_i)
        collection_name <- names(gseaCache$gseaResults$gseaDTs)[my_i]
        gseaDT <- gseaCache$gseaResults$gseaDTs[[my_i]]
        fgsea_res <- gseaCache$gseaResults$fgsea_results[[my_i]]
        
        # Render Plotly Plot
        output[[plot_id]] <- renderPlotly({
          req(gseaDT)
          
          # Take top 10 pathways based on absolute NES
          top_res <- gseaDT %>%
            dplyr::arrange(desc(abs(NES))) %>%
            slice(1:10)
          
          # Scale marker sizes
          size_range <- c(5, 25)
          min_size <- min(top_res$size, na.rm = TRUE)
          max_size <- max(top_res$size, na.rm = TRUE)
          scaled_size <- (top_res$size - min_size) / (max_size - min_size) * 
            (size_range[2] - size_range[1]) + size_range[1]
          
          # Create Plotly Scatter Plot
          plot_ly(
            data = top_res,
            x = ~NES,
            y = ~reorder(pathway, NES),  # Reorder pathways for better display
            type = "scatter",
            mode = "markers",
            marker = list(
              size = scaled_size,           # Set marker size
              color = ~padj,                # Set marker color based on adjusted p-value
              colorscale = "RdBu",
              reversescale = TRUE,
              cmin = 0,
              cmax = max(top_res$padj, na.rm = TRUE),
              showscale = TRUE,
              colorbar = list(
                title = "Adjusted p-value",
                titleside = "right",
                font = list(size = 12)
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
              title = paste("Top 10 Pathways -", collection_name),
              xaxis = list(title = "Normalized Enrichment Score"),
              yaxis = list(title = "Pathway")
            )
        })
        
        # Render DataTable
        output[[tbl_id]] <- renderDT({
          req(gseaDT)
          
          # Convert 'leadingEdge' from list to concatenated string
          gseaDT_processed <- gseaDT %>%
            dplyr::mutate(
              leadingEdge = sapply(leadingEdge, function(x) paste(x, collapse = ", "))
            )
          
          datatable(
            gseaDT_processed,
            options = list(
              pageLength = 10,
              scrollX = TRUE,
              autoWidth = TRUE,
              columnDefs = list(
                list(targets = 4, render = JS(
                  "function(data, type, row, meta) {",
                  "return type === 'display' && data.length > 100 ?",
                  "'<div style=\"white-space: normal;\">' + data + '</div>' : data;",
                  "}"
                ))
              )
            ),
            escape = FALSE  # Allow HTML for wrapping text
          )
        })
      })
    }
  })
  
  # Download Handler for GSEA Results
  output$downloadDataGSEA <- downloadHandler(
    filename = function() {
      paste0("GSEA-", Sys.Date(), ".csv")
    },
    content = function(file) {
      req(gseaCache$gseaResults)
      
      # Combine all GSEA data tables into one data frame
      combined_gseaDT <- do.call(rbind, lapply(names(gseaCache$gseaResults$gseaDTs), function(name) {
        df <- gseaCache$gseaResults$gseaDTs[[name]]
        df$Collection <- name  # Add a column to indicate the gene set collection
        df
      }))
      
      # Convert 'leadingEdge' from list to concatenated string
      combined_gseaDT <- combined_gseaDT %>%
        dplyr::mutate(
          leadingEdge = sapply(leadingEdge, function(x) paste(x, collapse = ", "))
        )
      
      # Write to CSV
      write.csv(combined_gseaDT, file, row.names = FALSE)
    }
  )

