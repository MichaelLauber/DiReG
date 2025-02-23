library(slickR)
library(org.Hs.eg.db)  
library(org.Mm.eg.db)  
library(AnnotationDbi)

ensg_to_hgnc <- readRDS("data/ensg_to_hgnc.rds")
mapping <- readRDS("data/gtex_mapping.rds")

sub_cols <- c("detection_method", "pub_id", "uniprot_A_list", "uniprot_B_list")

tftf_interacton_hs <- readRDS("data/human_tf_tf_interactions.rds") %>% dplyr::select(sub_cols)
tftf_interaction_mm <- readRDS("data/mouse_tf_tf_interactions.rds") %>% dplyr::select(sub_cols)

# Create content for the carousel

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
  

  uniprot_ids <- mapIds(orgDB,
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
      
      #adding ensmbl ids of the TF
      subset_df$TF1_ensembl <- ensg_to_hgnc %>%
        filter(HGNC == s) %>%
        dplyr::select(ENSG) %>% 
        .[[1]]
      
      subset_df <- subset_df %>% 
        rowwise() %>% 
        mutate(TF_symbol_2 = get_symbol_from_uniprot(
          if (matched_A == "A") uniprot_B_list else uniprot_A_list
        ))
      
      result_list[[s]] <- subset_df
    }
  }
  
  combined_df <- bind_rows(result_list)
  
  #adding ensmbl ids 
  combined_df <- combined_df %>%
    rowwise() %>%
    dplyr::mutate(TF2_ensembl = paste(
      ensg_to_hgnc %>%
        filter(HGNC == TF_symbol_2) %>%
        dplyr::pull(ENSG),
      collapse = "|"
    )) %>%
    ungroup()
  
  
  
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
  
   
  # output$carousel_tftf <- renderUI({
  #   div(id = paste0("carousel_tftf", as.integer(Sys.time())),
  #       do.call(glide, screens)
  #   )
  # })
  
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
    
    df_tmp <- df_tmp[, c("TF_symbol_2", "pub_id", "detection_method", "TF2_ensembl")]
    colnames(df_tmp) <- c("TF", "Pubmed", "Method", "TF_ensembl")
    
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
            list(targets = c(1,2), className = "clickableCell"),
            list(targets = c(4), visible = FALSE)
          )
        )
      )
    })
  })
  
  print("fuckin table")
  print(table_data_list)
  
  
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
        gene_name <- selectedRow$TF_ensembl
        gene_symbol <- gsub("<[^>]+>", "", selectedRow$TF)
      } else {
        return()  # Ignore clicks on the Class column or hidden columns
      }
      
      # If gene_name is empty or missing, do nothing
      if (is.null(gene_name) || is.na(gene_name) || gene_name == "") return()
      
      # 'gene_name' might contain multiple IDs separated by "|"
      # We'll take the first one for demonstration
      gene_name_vec <- stringr::str_split(gene_name, "\\|")[[1]]
      gene_to_load <- gene_name_vec[1]
      
      # Show a modal with the Plotly plot for the selected gene
      showModal(modalDialog(
        title = paste("Expression plot for", gene_symbol),
        plotlyOutput("popupPlot"),
        easyClose = TRUE,
        footer = modalButton("Close")
      ))
      
      # Render the Plotly plot in the modal
      output$popupPlot <- renderPlotly({
        # Build the filename and read the data
         filename <- paste0(gene_to_load, ".rds")
        # # Path to your expression data
         fullPath <- file.path("~/gtex_splitted", filename)
        
        if (!file.exists(fullPath)) {
          showModal(modalDialog(
            title = paste("Warning for", gene_symbol),
            "No expression data found for the selected gene.",
            easyClose = TRUE,
            footer = modalButton("Close")
          ))
          return()
        }
        
        # If file doesn't exist, show a simple plot or message
        if (!file.exists(fullPath)) {
          return(plot_ly() %>%
                   layout(title = "Expression file not found"))
        }
        
        gene <- readRDS(fullPath)
        
        df_gene <- data.frame(id = names(gene), values = gene)
        df_gene$tissue <- mapping$SMTSD[match(df_gene$id, mapping$id)]
        
        plot_ly(df_gene,
                y = ~values,
                x = ~tissue,
                type = 'violin',
                split = ~tissue,
                box = list(visible = TRUE),
                meanline = list(visible = TRUE),
                spanmode = "hard",
                marker = list(size = 3)) %>%
          layout(
            xaxis = list(
              title = paste("Expression plot for", gene_to_load),
              tickangle = 45,
              spanmode = "hard"
            ),
            yaxis = list(
              title = "TPM",
              zeroline = FALSE
            ),
            showlegend = FALSE
          )
      })
    })
  })
  
  
  
  previousTftfInputTFs(currentTFs)
})
)
    