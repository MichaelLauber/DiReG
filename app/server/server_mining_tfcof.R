library(org.Hs.eg.db)  
library(org.Mm.eg.db)  


tfcof_interaction_hs <- readRDS("data/gene_tf_interactions_human_annotated_small.rds")
tfcof_interaction_mm <- readRDS("data/gene_tf_interactions_mouse_annotated_small.rds")

selectedGeneTfcof <- reactiveVal(NULL)

# Single-cell folder logic (like your final code)
folderInfo_Tcof <- reactive({
  if (input$radioOrgDorothea == "human") {
    folder <- "hs_tcofs_rds_files"
    file_ending <- "_human_Tcofs.rds"
  } else {
    folder <- "mm_tcofs_rds_files"
    file_ending <- "_tmuris_tcofs.rds"
  }
  list(
    folder_path = file.path("data", folder),
    file_ending = file_ending
  )
})

files_tcof <- reactive({
  list.files(folderInfo_Tcof()$folder_path)
})

tissues_exp_tcof <- reactive({
  stringr::str_replace_all(files_tcof(), "_(human|tmuris)_(T|t)cofs.rds", "")
})

dataModalExpTcof <- function(failed = FALSE) {
  modalDialog(
    radioButtons("compareTissuesTcof", "Display Option", 
                 choices = c("Single Tissue" = "single", "Compare Two Tissues" = "compare"), 
                 selected = "compare", inline = TRUE),
    
    selectizeInput("selectTissueExpStartTcof", 
                   "Tissue",
                   choices = tissues_exp_tcof(),  # placeholder choices; replace with your reactive list if needed
                   selected = tissues_exp_tcof()[1],
                   width = "230px"),
    
    conditionalPanel(
      condition = "input.compareTissuesTcof == 'compare'",
      selectizeInput("selectTissueExpTargetTcof",
                     "Target Tissue",
                     choices = tissues_exp_tcof(),  # placeholder choices
                     selected = tissues_exp_tcof()[2],
                     width = "230px")
    ),
    
    span("Analyze TF Expression. Please be patient, might take some time till plots are rendered"),
    
    if (failed) {
      div(tags$b("Invalid name of data object", style = "color: red;"))
    },
    
    footer = tagList(
      modalButton("Cancel"),
      actionButton("okTissueTcof", "OK")    )
  )
}

previousTfcofInputTFs <- reactiveVal(NULL)

observeEvent(input$btnTfcof, {
  
  if(!networkCreated){
    shinyalert::shinyalert(
      "There are no Genes to Analyse",
      "Please input genes and press the RUN button before performing any analysis",
      type = "error")
    return()}
  
  organism <- reactive({
    switch(input$radioOrgDorothea,
           "human" = "hsapiens",
           "mouse" = "mmusculus")
  })
  
  
  if(organism() == "hsapiens"){
    tfcof_df <- tfcof_interaction_hs
    orgDB <- org.Hs.eg.db
  } else {
    tfcof_df <- tfcof_interaction_mm
    orgDB <- org.Mm.eg.db
  }
  
  
  currentTFs <- inputTFs()

  if (identical(currentTFs, previousTfcofInputTFs())) {
    print("Same input: TFCof wont be changed")
    return()
  }
  
  waiter <- waiter::Waiter$new()
  waiter$show()
  on.exit(waiter$hide())
  
  notification <- showNotification(glue::glue("Retrieving interacting TF Cofactors. This might take a few seconds"), type = "message", duration = NULL, closeButton = TRUE)
  on.exit(removeNotification(notification), add = TRUE)

  symbols <- c(inputTFs())
  #symbols <- c("CREBBP")
  nrFigs <- length(symbols)
  
  tfcof_df <- tfcof_df %>%
    mutate(TF_list = if_else(isTF_A, uniprot_A_list, uniprot_B_list))
  
  tfcof_df <- tfcof_df %>%
    mutate(TcoF_list = if_else(isTF_A, uniprot_B_list, uniprot_A_list))
  
  uniprot_ids <- AnnotationDbi::mapIds(orgDB,
                        keys = symbols,
                        keytype = "SYMBOL",
                        column = "UNIPROT",
                        #multiVals = "first"
                        multiVals = "list"
  )
  
  result_list <- list()
  
  for (s in symbols) {
    # Get the UniProt IDs for the current symbol
    current_ids <- uniprot_ids[[s]]
    
    # Subset tfcof_df rows where any element of TF_list matches current_ids
    subset_df <- tfcof_df %>%
      filter(purrr::map_lgl(TF_list, ~ any(.x %in% current_ids)))
    
    # If any rows found, add a column for the current symbol
    if(nrow(subset_df) > 0) {
      
      subset_df <- subset_df %>% mutate(TF_symbol = s)%>%
        filter(!is.na(tcof_gene_id)) %>%
        distinct(tcof_gene_id, .keep_all = TRUE) %>%
        dplyr::select(TF_symbol, tcof_gene_id, tcof_class) %>%
        dplyr::rename(TF = TF_symbol, TFcof = tcof_gene_id, Class = tcof_class)
      
      
      result_list[[s]] <- subset_df
    }
  }
  
  combined_df <- bind_rows(result_list)
  
  
  # If there's nothing to show, end here
  if(nrow(combined_df) == 0) {
    shinyalert::shinyalert(
      "No Results Found",
      "No TF Co-Factor Interactions Found for the Current Inputs",
      type = "info"
    )
    previousTfcofInputTFs(currentTFs)
    output$carousel_tfcof <- renderUI({ NULL })
    return()
  }
  
 

  screens <- lapply(symbols, function(s) {
    # Create a unique output ID for each symbol.
    slideId <- paste0("dt_tfcof_", s)
    shinyglide::screen(
      h3(s, align = "center"),
      DT::dataTableOutput(slideId)
    )
  })
 

  output$carousel_tfcof <- renderUI({
    div(id = paste0("carousel_", as.integer(Sys.time())),
        do.call(glide, screens)
    )
  })
  
  
  
  table_data_list <- setNames(lapply(symbols, function(s) {
    df_tmp <- combined_df %>% dplyr::filter(TF == s)
    
    class_tooltips <- list(
      "HC" = "HC: TcoFs with direct experimental support for both their role in transcriptional regulation and their presence in the nucleus.",
      "Class 1" = "Class 1: TcoFs with experimental evidence for transcriptional regulation, but only inferred evidence for being present in the nucleus.",
      "Class 2" = "Class 2: TcoFs with experimental evidence for nuclear localization, yet only inferred support for their role in transcriptional regulation.",
      "Class 3" = "Class 3: TcoFs where both the regulatory function and nuclear presence are supported solely by inferred, non-experimental evidence."
    )
    
    # Wrap Class column with tooltip text
    df_tmp$Class <- sapply(df_tmp$Class, function(cl) {
      tooltip <- class_tooltips[[cl]]
      paste0("<span title='", tooltip, "'>", cl, "</span>")
    })
    
    df_tmp$TF <- sapply(df_tmp$TF, function(val) {
      paste0("<span class='clickable-cell' title='Click to show per tissue expression'>", val, "</span>")
    })
    df_tmp$TFcof <- sapply(df_tmp$TFcof, function(val) {
      paste0("<span class='clickable-cell' title='Click to show per tissue expression'>", val, "</span>")
    })
    
    # Reorder columns so hidden columns (Ensembl IDs) come last
    df_tmp <- df_tmp[, c("TF", "TFcof", "Class")]
    
    # Reorder columns so hidden columns (Ensembl IDs) come last
    df_tmp <- df_tmp[, c("TF", "TFcof", "Class")]
    
    df_tmp
  }), symbols)
  

  
  lapply(symbols, function(s) {
    slideId <- paste0("dt_tfcof_", s)
    output[[slideId]] <- DT::renderDT({
      # Build the DT
      DT::datatable(
        table_data_list[[s]],
        selection = 'single',
        escape = FALSE,
        class = "display clickableDT", 
        options = list(
          pageLength = 10,
          autoWidth = TRUE,
          columnDefs = list(
            list(targets = c(2), className = "clickableCell")
          )
        )
      )
    })
  })
  
 
  

  
  output$popupPlot <- renderPlotly({ NULL })  # Initialize empty
  
  lapply(symbols, function(s) {
    observeEvent(input[[paste0("dt_tfcof_", s, "_cell_clicked")]], {
      info <- input[[paste0("dt_tfcof_", s, "_cell_clicked")]]
      if (is.null(info$row) || is.null(info$col)) return()
      
      # Get the corresponding row from our stored data
      df_clicked <- table_data_list[[s]]
      selectedRow <- df_clicked[info$row, ]
      


      gene_name <- NULL
      
      if (info$col == 1) {
        # User clicked on TF
        #gene_symbol <- gsub("<[^>]+>", "", selectedRow$TF)
        return() 
      } else if (info$col == 2) {
        # User clicked on TFcof
        gene_symbol <- gsub("<[^>]+>", "", selectedRow$TFcof)
      } else {
        return()  # Ignore clicks on the Class column or hidden columns
      }
      

      # Store the gene symbol in selectedGeneTfcof
      selectedGeneTfcof(gene_symbol)
      
      # Show the same tissue-selection modal as your final code
      showModal(dataModalExpTcof())
    })
  })
  
  previousTfcofInputTFs(currentTFs)
})
   
observeEvent(input$okTissueTcof, {
  removeModal()
  
  # The gene symbol clicked in TFCof table
  gene_symbol <- unname(selectedGeneTfcof())
  print("gene symbol")
  message(gene_symbol)
  if (is.null(gene_symbol) || gene_symbol == "") {
    print("No gene symbol selected for TFCof. Aborting.")
    return()
  }
  
  # Single or compare mode
  tissue_start <- input$selectTissueExpStartTcof
  compare_mode <- (input$compareTissuesTcof == "compare")
  tissue_target <- if (compare_mode) input$selectTissueExpTargetTcof else NA
  
  
  expr_data_start <- readRDS(
    file.path(folderInfo_Tcof()$folder_path, paste0(tissue_start, folderInfo_Tcof()$file_ending))
  )
  if (compare_mode) {
    expr_data_target <- readRDS(
      file.path(folderInfo_Tcof()$folder_path, paste0(tissue_target, folderInfo_Tcof()$file_ending))
    )
  }
  
  
  # If human, filter by annotation
  if (input$radioOrgDorothea == "human") {
    annotation_counts_start <- table(expr_data_start$free_annotation)
    valid_annotations_start <- names(annotation_counts_start[annotation_counts_start >= 40])
    expr_data_start <- expr_data_start[expr_data_start$free_annotation %in% valid_annotations_start, ]
    
    if (compare_mode) {
      annotation_counts_target <- table(expr_data_target$free_annotation)
      valid_annotations_target <- names(annotation_counts_target[annotation_counts_target >= 40])
      expr_data_target <- expr_data_target[expr_data_target$free_annotation %in% valid_annotations_target, ]
    }
  }
  
  if (!(gene_symbol %in% colnames(expr_data_start))) {
    shinyalert::shinyalert(
      "Missing Data",
      paste("No expression data found for gene", gene_symbol),
      type = "warning"
    )
    return()
  }
  
  
  columns_to_keep <- c("cell_ontology_class", "free_annotation")
  
  if (compare_mode && tissue_start != tissue_target) {
    # Build two violin plots
    p_start <- plotly::plot_ly(
      data = dplyr::select(expr_data_start, dplyr::all_of(c(gene_symbol, columns_to_keep))),
      y = as.formula(paste0("~`", gene_symbol, "`")),
      x = ~cell_ontology_class,
      type = "violin",
      split = ~cell_ontology_class,
      box = list(visible = TRUE),
      meanline = list(visible = TRUE),
      spanmode = "hard",
      marker = list(size = 3)
    ) %>%
      plotly::layout(
        xaxis = list(title = paste0("Tissue: ", tissue_start), tickangle = 45),
        yaxis = list(title = "Expression", zeroline = FALSE),
        showlegend = FALSE
      )
    
    p_target <- plotly::plot_ly(
      data = dplyr::select(expr_data_target, dplyr::all_of(c(gene_symbol, columns_to_keep))),
      y = as.formula(paste0("~`", gene_symbol, "`")),
      x = ~cell_ontology_class,
      type = "violin",
      split = ~cell_ontology_class,
      box = list(visible = TRUE),
      meanline = list(visible = TRUE),
      spanmode = "hard",
      marker = list(size = 3)
    ) %>%
      plotly::layout(
        xaxis = list(title = paste0("Tissue: ", tissue_target), tickangle = 45),
        yaxis = list(title = "Expression", zeroline = FALSE),
        showlegend = FALSE
      )
    
    # Show a modal with 2 columns
    showModal(modalDialog(
      title = paste("Single-Cell Plots for", gene_symbol),
      fluidRow(
        column(6, plotlyOutput("plotlyStartTFCof")),
        column(6, plotlyOutput("plotlyTargetTFCof"))
      ),
      easyClose = TRUE,
      footer = modalButton("Close")
    ))
    
    output$plotlyStartTFCof <- plotly::renderPlotly({ p_start })
    output$plotlyTargetTFCof <- plotly::renderPlotly({ p_target })
    
  } else {
    # Single mode
    p_single <- plotly::plot_ly(
      data = dplyr::select(expr_data_start, dplyr::all_of(c(gene_symbol, columns_to_keep))),
      y = as.formula(paste0("~`", gene_symbol, "`")),
      x = ~cell_ontology_class,
      type = "violin",
      split = ~cell_ontology_class,
      box = list(visible = TRUE),
      meanline = list(visible = TRUE),
      spanmode = "hard",
      marker = list(size = 3)
    ) %>%
      plotly::layout(
        xaxis = list(title = paste0("Tissue: ", tissue_start), tickangle = 45),
        yaxis = list(title = "Expression", zeroline = FALSE),
        showlegend = FALSE
      )
    
    showModal(modalDialog(
      title = paste("Single-Cell Plot for", gene_symbol),
      plotlyOutput("plotlySingleTFCof"),
      easyClose = TRUE,
      footer = modalButton("Close")
    ))
    
    output$plotlySingleTFCof <- plotly::renderPlotly({ p_single })
  }
})      