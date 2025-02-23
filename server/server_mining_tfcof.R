library(slickR)
library(org.Hs.eg.db)  
library(org.Mm.eg.db)  
library(AnnotationDbi)

ensg_to_hgnc <- readRDS("data/ensg_to_hgnc.rds")
mapping <- readRDS("data/gtex_mapping.rds")

tfcof_interaction_hs <- readRDS("data/gene_tf_interactions_human_annotated_small.rds")
tfcof_interaction_mm <- readRDS("data/gene_tf_interactions_mouse_annotated_small.rds")


# Create content for the carousel

previousTfcofInputTFs <- reactiveVal(NULL)

observeEvent(input$btnTfcof, ({
  
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

  symbols <- c(inputTFs())
  #symbols <- c("HNF1A", "FOXP3")
  nrFigs <- length(symbols)
  
  tfcof_df <- tfcof_df %>%
    mutate(TF_list = if_else(isTF_A, uniprot_A_list, uniprot_B_list))
  
  tfcof_df <- tfcof_df %>%
    mutate(TcoF_list = if_else(isTF_A, uniprot_B_list, uniprot_A_list))
  
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
      
      #adding ensmbl ids of the TF
      subset_df$TF_ensembl <- ensg_to_hgnc %>%
        filter(HGNC == s) %>%
        dplyr::select(ENSG)
      
      
      
      result_list[[s]] <- subset_df
    }
  }
  
  combined_df <- bind_rows(result_list)
  
  #adding ensmbl ids for TFcof
  combined_df <- combined_df %>%
    rowwise() %>%
    dplyr::mutate(TFcof_ensembl = paste(
      ensg_to_hgnc %>%
        filter(HGNC == TFcof) %>%
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
    previousTfcofInputTFs(currentTFs)
    output$carousel_tfcof <- renderUI({ NULL })
    return()
  }
  
  fix_entry <- function(symbol, current_ensembl, organism) {
    # If the current value is non-empty, return it.
    if (!is.na(current_ensembl) && current_ensembl != "") {
      return(current_ensembl)
    } else {
      # Otherwise, use gprofiler2 to convert the symbol.
      res <- tryCatch({
        gprofiler2::gconvert(query = symbol,
                             organism = organism,
                             target = "ENSG", 
                             mthreshold = Inf,
                             filter_na = FALSE) %>%
          dplyr::distinct(`input`, .keep_all = TRUE) %>%
          dplyr::pull(target)
      }, error = function(e) NA_character_)
      if(length(res) > 0) return(res[1])
      return(NA_character_)
    }
  }
  
  
  combined_df <- combined_df %>%
    rowwise() %>%
    mutate(
      TF_ensembl = fix_entry(TF, TF_ensembl, organism()),
      TFcof_ensembl = fix_entry(TFcof, TFcof_ensembl, organism())
    ) %>%
    ungroup()
  
  

  screens <- lapply(symbols, function(s) {
    # Create a unique output ID for each symbol.
    slideId <- paste0("dt_", s)
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
  
  
  
  class_tooltips <- list(
    "HC" = "HC: TcoFs with direct experimental support for both their role in transcriptional regulation and their presence in the nucleus.",
    "Class 1" = "Class 1: TcoFs with experimental evidence for transcriptional regulation, but only inferred evidence for being present in the nucleus.",
    "Class 2" = "Class 2: TcoFs with experimental evidence for nuclear localization, yet only inferred support for their role in transcriptional regulation.",
    "Class 3" = "Class 3: TcoFs where both the regulatory function and nuclear presence are supported solely by inferred, non-experimental evidence."
  )
  
  table_data_list <- setNames(lapply(symbols, function(s) {
    df_tmp <- combined_df %>% dplyr::filter(TF == s)
    
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
    df_tmp <- df_tmp[, c("TF", "TFcof", "Class", "TF_ensembl", "TFcof_ensembl")]
    
    # Reorder columns so hidden columns (Ensembl IDs) come last
    df_tmp <- df_tmp[, c("TF", "TFcof", "Class", "TF_ensembl", "TFcof_ensembl")]
    
    df_tmp
  }), symbols)
  
  print("TFcof table")
  print(table_data_list)
  
  lapply(symbols, function(s) {
    slideId <- paste0("dt_", s)
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
            list(targets = c(4,5), visible = FALSE)
          )
        )
      )
    })
  })
  
  search_gene_version <- function(gene_to_load, folder = "~/gtex_splitted") {
    # Construct a regex pattern: gene_to_load followed by a dot and then digits (the version)
    pattern <- paste0("^", gene_to_load, "\\.[0-9]+\\.rds$")
    files <- list.files(folder, pattern = pattern, full.names = TRUE)
    if (length(files) > 0) {
      return(files[1])
    } else {
      return(NULL)
    }
  }
  
  get_versioned_file_path <- function(gene_to_load, folder = "~/gtex_splitted") {
    # Check if gene_to_load already has a version suffix (e.g. ".17")
    if (grepl("\\.[0-9]+$", gene_to_load)) {
      # Construct the expected file path
      file_path <- file.path(folder, paste0(gene_to_load, ".rds"))
      if (file.exists(file_path)) {
        return(file_path)
      } else {
        # Fallback: attempt to search for a matching file even if gene_to_load is versioned.
        return(search_gene_version(gene_to_load, folder))
      }
    } else {
      # gene_to_load does not have a version; search for a versioned file.
      file_path <- search_gene_version(gene_to_load, folder)
      if (!is.null(file_path)) {
        return(file_path)
      } else {
        # Optionally, try a default file name (without version) if present.
        file_path <- file.path(folder, paste0(gene_to_load, ".rds"))
        if (file.exists(file_path)) return(file_path)
        return(NULL)
      }
    }
  }
  
  #
  # OBSERVERS FOR CELL CLICK
  # We'll create one observer per table so that when a user clicks,
  # we pick up the row/col, figure out if they clicked TF or TFcof,
  # then show the violin plot in a modal.
  #
  
  # A single plot output ID for the modal's Plotly
  # (We will reuse the same ID "popupPlot" for every click)
  output$popupPlot <- renderPlotly({ NULL })  # Initialize empty
  
  lapply(symbols, function(s) {
    observeEvent(input[[paste0("dt_", s, "_cell_clicked")]], {
      info <- input[[paste0("dt_", s, "_cell_clicked")]]
      if (is.null(info$row) || is.null(info$col)) return()
      
      # Get the corresponding row from our stored data
      df_clicked <- table_data_list[[s]]
      selectedRow <- df_clicked[info$row, ]
      
      # Decide which column was clicked
      # 'TF' -> column index 0
      # 'TFcof' -> column index 1
      # In 0-based indexing, columns are: 
      #   0: TF
      #   1: TFcof
      #   2: Class
      #   3: TF_ensembl (hidden)
      #   4: TFcof_ensembl (hidden)
      gene_name <- NULL
      
      if (info$col == 1) {
        # User clicked on TF
        gene_name <- selectedRow$TF_ensembl
        gene_symbol <- gsub("<[^>]+>", "", selectedRow$TF)
      } else if (info$col == 2) {
        # User clicked on TFcof
        gene_name <- selectedRow$TFcof_ensembl
        gene_symbol <- gsub("<[^>]+>", "", selectedRow$TFcof)
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
        # filename <- paste0(gene_to_load, ".rds")
        # # Path to your expression data
        # fullPath <- file.path("~/gtex_splitted", filename)
        
        fullPath <- get_versioned_file_path(gene_to_load) 
        
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
  
  

  previousTfcofInputTFs(currentTFs)
})
)