library(org.Hs.eg.db)  
library(org.Mm.eg.db)  


sub_cols <- c("detection_method", "pub_id", "uniprot_A_list", "uniprot_B_list")

tftf_interacton_hs <- readRDS("data/human_tf_tf_interactions.rds") %>% dplyr::select(sub_cols)
tftf_interaction_mm <- readRDS("data/mouse_tf_tf_interactions.rds") %>% dplyr::select(sub_cols)

selectedGeneTFTF <- reactiveVal(NULL)

# Create content for the carousel

folderInfo <- reactive({
  if (input$radioOrgDorothea == "human") {
    folder <- "hs_tfs_rds_files"
    file_ending <- "_human_TFs.rds"
  } else {
    folder <- "mm_tfs_rds_files"
    file_ending <- "_tmuris_tfs.rds"
  }
  list(
    folder_path = file.path("data", folder),
    file_ending = file_ending
  )
})

files <- reactive({
  list.files(folderInfo()$folder_path)
})

tissues_exp <- reactive({
  stringr::str_replace_all(files(), "_(human|tmuris)_TFs.rds", "")
})

dataModalExpTFTF <- function(failed = FALSE) {
  modalDialog(
    radioButtons("compareTissuesMining", "Display Option", 
                 choices = c("Single Tissue" = "single", "Compare Two Tissues" = "compare"), 
                 selected = "compare", inline = TRUE),
    
    selectizeInput("selectTissueExpStartTFTF", 
                   "Tissue",
                   choices = tissues_exp(),  # placeholder choices; replace with your reactive list if needed
                   selected = tissues_exp()[1],
                   width = "230px"),
    
    conditionalPanel(
      condition = "input.compareTissuesMining == 'compare'",
      selectizeInput("selectTissueExpTargetTFTF",
                     "Target Tissue",
                     choices = tissues_exp(),  # placeholder choices
                     selected = tissues_exp()[2],
                     width = "230px")
    ),
    
    span("Analyze TF Expression. Please be patient, might take some time till plots are rendered"),
    
    if (failed) {
      div(tags$b("Invalid name of data object", style = "color: red;"))
    },
    
    footer = tagList(
      modalButton("Cancel"),
      actionButton("okTissueTFTF", "OK")    )
  )
}

previousTftfInputTFs <- reactiveVal(NULL)

observeEvent(input$btnTFTF, ({
  
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
    tftf_df <- tftf_interacton_hs
    orgDB <- org.Hs.eg.db
  } else {
    tftf_df <- tftf_interaction_mm
    orgDB <- org.Mm.eg.db
  }
  
  
  currentTFs <- inputTFs()
  
  if (identical(currentTFs, previousTftfInputTFs())) {
    print("same input TFs")
    return()
  }
  
  waiter <- waiter::Waiter$new()
  waiter$show()
  on.exit(waiter$hide())
  
  notification <- showNotification(glue::glue("Retrieving interacting TFs. This might take a few seconds"), type = "message", duration = NULL, closeButton = TRUE)
  on.exit(removeNotification(notification), add = TRUE)
  
  symbols <- c(inputTFs())
  #symbols <- c("HNF1A", "FOXP3")
  nrFigs <- length(symbols)
  
 
  get_symbol_from_uniprot <- function(uniprot_ids, org_db = orgDB) {
    # 1) Coerce 'uniprot_ids' to a character vector
    if (is.list(uniprot_ids)) {
      uniprot_ids <- unlist(uniprot_ids, use.names = FALSE)
    }
    uniprot_ids <- as.character(uniprot_ids)
    
    # 2) If 'uniprot_ids' is empty, return NA
    if (length(uniprot_ids) == 0) {
      return(NA_character_)
    }
    
    # 3) Check each UniProt ID in order, returning the first match
    for (uid in uniprot_ids) {
      symbol <- suppressMessages(
        AnnotationDbi::mapIds(
        org_db,
        keys = uid,
        keytype = "UNIPROT",
        column = "SYMBOL",
        multiVals = "first"
      )
      )
      # If 'symbol' is not NULL or NA, it's a successful match
      if (!is.null(symbol) && !is.na(symbol)) {
        return(symbol)
      }
    }
    
    # 4) If no matches were found, return NA
    return(NA_character_)
  }
  

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
    
    # Subset tftf_df rows where any element of TF_list matches current_ids
    subset_df <- tftf_df %>%
      dplyr::mutate(
        matched_A = purrr::map_lgl(uniprot_A_list, ~ any(.x %in% current_ids)),
        matched_B = purrr::map_lgl(uniprot_B_list, ~ any(.x %in% current_ids))
      ) %>%
      filter(matched_A | matched_B) %>%
      dplyr::mutate(match = case_when(
        matched_A & matched_B ~ "Both",
        matched_A ~ "A",
        matched_B ~ "B"
      ))
    
    # If any rows found, add a column for the current symbol
    if(nrow(subset_df) > 0) {
      
      subset_df <- subset_df %>% mutate(TF_symbol_1 = s, Uniprot_1 = list(current_ids))
      
      
      subset_df <- subset_df %>% 
        rowwise() %>% 
        mutate(TF_symbol_2 = get_symbol_from_uniprot(
          if (matched_A == "A") uniprot_B_list else uniprot_A_list
        ))
      
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
    previousTftfInputTFs(currentTFs)
    output$carousel_tftf <- renderUI({ NULL })
    return()
  }
  
 
  
  screens <- lapply(symbols, function(s) {
    # Create a unique output ID for each symbol.
    slideId <- paste0("dt_tftf", s)
    shinyglide::screen(
      h3(s, align = "center"),
      DT::dataTableOutput(slideId)
    )
  })
  
  
  output$carousel_tftf <- renderUI({
    div(id = paste0("carousel_tftf_", round(runif(1)*1e8)),
        do.call(shinyglide::glide, screens)
    )
  })
  
  
  
  table_data_list <- setNames(lapply(symbols, function(s) {
    df_tmp <- combined_df %>% dplyr::filter(TF_symbol_1 == s)
    
    # Wrap Class column with tooltip text
    
    df_tmp$pub_id <- sapply(df_tmp$pub_id, function(val) {
      id <- stringr::str_remove_all(val, "pubmed:")
      paste0("<a href='https://pubmed.ncbi.nlm.nih.gov/", id, 
             "' target='_blank' title='View publication on PubMed'>", id, "</a>")
    })
    
    df_tmp$TF_symbol_2 <- sapply(df_tmp$TF_symbol_2, function(val) {
      paste0("<span class='clickable-cell' title='Click to show per tissue expression'>", val, "</span>")
    })
    
    # Reorder columns so hidden columns (Ensembl IDs) come last
    
    df_tmp <- df_tmp[, c("TF_symbol_2", "pub_id", "detection_method")]
    colnames(df_tmp) <- c("TF", "Pubmed", "Method")
    
    df_tmp %>% distinct()
  }), symbols)
  
  
  lapply(symbols, function(s) {
    slideId <- paste0("dt_tftf", s)
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
            list(targets = c(1,2), className = "clickableCell")
          )
        )
      )
    })
  })
  
  
  # OBSERVERS FOR CELL CLICK
 
  output$popupPlot <- renderPlotly({ NULL })  # Initialize empty
  
  lapply(symbols, function(s) {
    observeEvent(input[[paste0("dt_tftf", s, "_cell_clicked")]], {
      info <- input[[paste0("dt_tftf", s, "_cell_clicked")]]
      if (is.null(info$row) || is.null(info$col)) return()
      
      # Get the corresponding row from our stored data
      df_clicked <- table_data_list[[s]]
      selectedRow <- df_clicked[info$row, ]
      

      gene_name <- NULL
      
      if (info$col == 1) {
        # User clicked on TF
        #gene_name <- selectedRow$TF_ensembl
        gene_symbol <- gsub("<[^>]+>", "", selectedRow$TF)
        selectedGeneTFTF(gene_symbol)
      } else {
        return()  # Ignore clicks on the Class column or hidden columns
      }
      showModal(dataModalExpTFTF())
     
      

    })
  })
  
   
  
  previousTftfInputTFs(currentTFs)
})
)

observeEvent(input$okTissueTFTF, {
  # Close the tissue-selection modal first
  removeModal()
  
  # Retrieve the clicked gene symbol
  gene_symbol <- unname(selectedGeneTFTF())
  
  
  
  
  if (is.null(gene_symbol) || gene_symbol == "") {
    print("No gene symbol selected. Calculation aborted.")
    return()
  }
  
  # Get selected tissues and determine compare mode
  tissue_start <- input$selectTissueExpStartTFTF
  compare_mode <- (input$compareTissuesMining == "compare")
  tissue_target <- if (compare_mode) input$selectTissueExpTargetTFTF else NA
  

  # Load expression data for the start tissue (and for target if needed)
  expr_data_start <- readRDS(
    file.path(folderInfo()$folder_path, paste0(tissue_start, folderInfo()$file_ending))
  )
  if (compare_mode) {
    expr_data_target <- readRDS(
      file.path(folderInfo()$folder_path, paste0(tissue_target, folderInfo()$file_ending))
    )
  }
 
  
  # If organism is human, filter expression data by free_annotation counts (>=40)
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
  
  # Define columns to keep for plotting
  columns_to_keep <- c("cell_ontology_class", "free_annotation", "broad_cell_class")
  
  if (compare_mode && tissue_start != tissue_target) {
 
    # Calculate the plot for the start tissue
    p_start <- plotly::plot_ly(
      data = dplyr::select(expr_data_start, dplyr::all_of(c(gene_symbol, columns_to_keep))),
      y = ~.data[[gene_symbol]],
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
    
    # Calculate the plot for the target tissue
    p_target <- plotly::plot_ly(
      data = dplyr::select(expr_data_target, dplyr::all_of(c(gene_symbol, columns_to_keep))),
      y = ~.data[[gene_symbol]],
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
    
    
    # Show modal with two columns
    showModal(modalDialog(
      title = "Calculated Plots",
      fluidRow(
        column(6, plotlyOutput("plotlyStartTFTF")),
        column(6, plotlyOutput("plotlyTargetTFTF"))
      ),
      easyClose = TRUE,
      footer = modalButton("Close")
    ))
    
    output$plotlyStartTFTF <- plotly::renderPlotly({ p_start })
    output$plotlyTargetTFTF <- plotly::renderPlotly({ p_target })
    
  } else {
    # Calculate the plot for single tissue mode

    
    p_single <- plotly::plot_ly(
      data = dplyr::select(expr_data_start, dplyr::all_of(c(gene_symbol, columns_to_keep))),
      y = ~.data[[gene_symbol]],
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
    
    
    
    # Show modal with single plot output
    showModal(modalDialog(
      title = "Calculated Plot",
      plotlyOutput("plotlySingleTFTF"),
      easyClose = TRUE,
      footer = modalButton("Close")
    ))
    
    output$plotlySingleTFTF <- plotly::renderPlotly({ p_single })
  }
})

