# Update data type selection when radio button changes (for human)
observeEvent(input$dataTypeRadio, {
  dataTypeSelection(input$dataTypeRadio)
})

# Function to get cell types for a specific tissue in single cell data
getCellTypesForTissue <- function(tissue, organism) {
  if(is.null(tissue) || tissue == "") return(character(0))
  
  # Select the appropriate folder based on organism
  folder <- if(organism == "human") {
    "hs_pseudobulk_by_tissue"
  } else {
    "mm_pseudobulk_by_tissue" 
  }
  
  celltypes <- list.files(file.path("data", folder, tissue))
  return(celltypes)
}

data_list <- reactive({
  
  if (input$radioOrgDorothea == "human") {
    expression <- readRDS(file.path("data","gtexTPM.rds"))
    tissues <- colnames(expression)[-c(1:2)]
    
    # Get single cell tissue options for human
    hs_ss_tissues <- list.files(file.path("data", "hs_pseudobulk_by_tissue/"))
    
    return(list(
      expression = expression,  
      tissues = tissues,
      ss_tissues = hs_ss_tissues,
      organism = "human",
      has_bulk = TRUE
    ))
  } else {
    # For mouse - NO BULK DATA, only single cell
    
    # Get single cell tissue options for mouse
    mm_ss_tissues <- list.files(file.path("data", "mm_pseudobulk_by_tissue/"))
    
    return(list(
      expression = NULL,  # No bulk expression data
      tissues = NULL,     # No bulk tissues
      ss_tissues = mm_ss_tissues,
      organism = "mouse",
      has_bulk = FALSE
    ))
  }
})

# Function to format tissue names for display
formatTissueNameForDisplay <- function(tissue_name) {
  # Remove "_aggregated.csv" suffix if present
  clean_name <- gsub("_aggregated\\.csv$", "", tissue_name)
  
  # Replace underscores with spaces
  clean_name <- gsub("_", " ", clean_name)
  
  return(clean_name)
}

# Function to create choices for select inputs with formatted display names
createFormattedChoices <- function(items) {
  if(length(items) == 0) return(NULL)
  
  # Create a named vector where names are formatted for display
  # and values are the original values
  display_names <- sapply(items, formatTissueNameForDisplay)
  names(items) <- display_names
  
  return(as.list(items))
}

# Store the current data type selection (bulk or single_cell)
dataTypeSelection <- reactiveVal("bulk")

# Reactive to store cell types for selected start tissue
cellTypesForSelectedStartTissue <- reactive({
  req(input$selectTissueSCStart)
  getCellTypesForTissue(input$selectTissueSCStart, data_list()$organism)
})

# Reactive to store cell types for selected target tissue
cellTypesForSelectedTargetTissue <- reactive({
  req(input$selectTissueSCTarget)
  getCellTypesForTissue(input$selectTissueSCTarget, data_list()$organism)
})

vals <- reactiveValues(data = NULL)

# Create reactive values to store previous state
prevState <- reactiveValues(
  celltype1 = NULL,
  celltype2 = NULL,
  data = NULL,
  res = NULL,  # Store previously computed results
  dataType = NULL,  # Store data type (bulk or single_cell)
  start_tissue_sc = NULL,  # Store start tissue for single cell
  target_tissue_sc = NULL,  # Store target tissue for single cell
  celltype_sc_start = NULL,  # Store start cell type for single cell
  celltype_sc_target = NULL   # Store target cell type for single cell
)

dataModal <- function(failed = FALSE) {
  # Determine if we're working with human or mouse data
  is_human <- input$radioOrgDorothea == "human"
  
  # For mouse, we should automatically select single cell since bulk isn't available
  if (!is_human) {
    dataTypeSelection("single_cell")
  }
  
  modalDialog(
    # Only show data type selection for human (mouse only has single cell)
    if(is_human) {
      div(
        radioButtons("dataTypeRadio", "Data Type:",
                     choices = c("Bulk" = "bulk", "Single Cell (Pseudo Bulk)" = "single_cell"),
                     selected = dataTypeSelection(),
                     inline = TRUE)
      )
    } else {
      # For mouse, just show an informational message
      div(
        h5("Based on Pseudo Bulk Data"),
        shinyjs::hidden(textInput("dataTypeRadio", "", value = "single_cell"))
      )
    },
    
    # Bulk data UI - shown when bulk is selected (human only)
    conditionalPanel(
      condition = "input.dataTypeRadio === 'bulk'",
      selectizeInput("selectTissueTFAStart", h3("Start Tissue"),
                     choices = createChoices(data_list()$tissues),
                     selected = "Cells - Cultured fibroblasts",
                     width = "230px"),
      
      selectizeInput("selectTissueTFATarget", h3("Target Tissue"),
                     choices = createChoices(data_list()$tissues),
                     selected = "Liver",
                     width = "230px")
    ),
    
    # Single cell data UI - shown when single_cell is selected (both human and mouse)
    conditionalPanel(
      condition = "input.dataTypeRadio === 'single_cell'",
      # Start tissue and cell type selection
      h3("Start Condition:"),
      selectizeInput("selectTissueSCStart", "Tissue",
                     choices = createFormattedChoices(data_list()$ss_tissues),
                     selected = if(length(data_list()$ss_tissues) > 0) data_list()$ss_tissues[1] else NULL,
                     width = "230px"),
      
      uiOutput("cellTypeSelectionUIStart"),
      
      # Target tissue and cell type selection
      h3("Target Condition:"),
      selectizeInput("selectTissueSCTarget", "Tissue",
                     choices = createFormattedChoices(data_list()$ss_tissues),
                     selected = if(length(data_list()$ss_tissues) > 0 && length(data_list()$ss_tissues) > 1) 
                       data_list()$ss_tissues[2] else data_list()$ss_tissues[1],
                     width = "230px"),
      
      uiOutput("cellTypeSelectionUITarget")
    ),
    
    span(ifelse(dataTypeSelection() == "single_cell", 
                'Compare TF activities between different cell types', 
                'Compare TF activities between different tissues')),
    
    if (failed)
      div(tags$b("Invalid name of data object", style = "color: red;")),
    
    footer = tagList(
      modalButton("Cancel"),
      actionButton("ok", "OK")
    )
  )
}

# Output to tell UI if single cell data is available - no longer needed but kept for backward compatibility
output$hasSingleCell <- reactive({
  return(TRUE)
})
outputOptions(output, "hasSingleCell", suspendWhenHidden = FALSE)

# Set data type based on organism when radio button changes
observeEvent(input$radioOrgDorothea, {
  if(input$radioOrgDorothea == "mouse") {
    # Mouse only has single cell data
    dataTypeSelection("single_cell")
  } else {
    # For human, default to bulk data if switching from mouse
    if(dataTypeSelection() == "single_cell" && !is.null(prevState$dataType) && prevState$dataType == "single_cell") {
      # Don't change if user explicitly chose single cell for human
    } else {
      dataTypeSelection("bulk")
    }
  }
}, priority = 20) # High priority to ensure this runs before other event handlers

# Render the cell type selection UI for start tissue
output$cellTypeSelectionUIStart <- renderUI({
  req(input$selectTissueSCStart)
  cellTypes <- cellTypesForSelectedStartTissue()
  
  if(length(cellTypes) < 1) {
    return(div(
      h4("No cell types available for this tissue", style="color:red"),
      "Please select a different tissue with available cell types."
    ))
  }
  
  selectizeInput("selectCellTypeSCStart", "Cell Type",
                 choices = createFormattedChoices(cellTypes),
                 selected = cellTypes[1],
                 width = "230px")
})

# Render the cell type selection UI for target tissue
output$cellTypeSelectionUITarget <- renderUI({
  req(input$selectTissueSCTarget)
  cellTypes <- cellTypesForSelectedTargetTissue()
  
  if(length(cellTypes) < 1) {
    return(div(
      h4("No cell types available for this tissue", style="color:red"),
      "Please select a different tissue with available cell types."
    ))
  }
  
  selectizeInput("selectCellTypeSCTarget", "Cell Type",
                 choices = createFormattedChoices(cellTypes),
                 selected = cellTypes[1],
                 width = "230px")
})

# Prevent same tissue selection for bulk data
observeEvent(input$selectTissueTFAStart, {
  if(input$selectTissueTFAStart == input$selectTissueTFATarget){
    updateSelectizeInput(session, "selectTissueTFATarget", 
                         choices = setdiff(createChoices(data_list()$tissues), input$selectTissueTFAStart))
  }
})

observeEvent(input$selectTissueTFATarget, {
  if(input$selectTissueTFAStart == input$selectTissueTFATarget){
    updateSelectizeInput(session, "selectTissueTFAStart", 
                         choices = setdiff(createChoices(data_list()$tissues), input$selectTissueTFATarget))
  }
})

# Prevent same tissue selection for single cell data
observeEvent(input$selectTissueSCStart, {
  if(!is.null(input$selectTissueSCStart) && 
     !is.null(input$selectTissueSCTarget) && 
     input$selectTissueSCStart == input$selectTissueSCTarget &&
     length(data_list()$ss_tissues) > 1) {
    
    updateSelectizeInput(session, "selectTissueSCTarget", 
                         choices = createFormattedChoices(data_list()$ss_tissues),
                         selected = setdiff(data_list()$ss_tissues, input$selectTissueSCStart)[1])
  }
})

observeEvent(input$selectTissueSCTarget, {
  if(!is.null(input$selectTissueSCStart) && 
     !is.null(input$selectTissueSCTarget) && 
     input$selectTissueSCStart == input$selectTissueSCTarget &&
     length(data_list()$ss_tissues) > 1) {
    
    updateSelectizeInput(session, "selectTissueSCStart", 
                         choices = createFormattedChoices(data_list()$ss_tissues),
                         selected = setdiff(data_list()$ss_tissues, input$selectTissueSCTarget)[1])
  }
})

observeEvent(input$btnTFA, {
  
  if(!networkCreated){
    shinyalert::shinyalert("There are no Genes to Analyse",
                           "Please input genes and press the RUN button before performing any analysis",
                           type = "error")
    return()
  }
  
  removeModal()
  showModal(dataModal())
})  

observeEvent(input$ok, {
  
  removeModal() 
  
  # Determine if we're using single cell data
  using_single_cell <- dataTypeSelection() == "single_cell"
  
  # Check if everything is the same as before to use cached results
  if (identical(dataTypeSelection(), prevState$dataType) &&
      identical(data(), prevState$data) &&
      ((using_single_cell && 
        identical(input$selectTissueSCStart, prevState$start_tissue_sc) &&
        identical(input$selectCellTypeSCStart, prevState$celltype_sc_start) &&
        identical(input$selectTissueSCTarget, prevState$target_tissue_sc) &&
        identical(input$selectCellTypeSCTarget, prevState$celltype_sc_target)) ||
       (!using_single_cell && 
        identical(input$selectTissueTFAStart, prevState$celltype1) &&
        identical(input$selectTissueTFATarget, prevState$celltype2)))) {
    
    # If everything is the same, reuse the cached result
    res <- prevState$res
  } else {
    waiter <- waiter::Waiter$new()
    waiter$show()
    on.exit(waiter$hide())
    
    notification <- showNotification(glue::glue("Calculating TFAs"), type = "message", duration = NULL, closeButton = TRUE)
    on.exit(removeNotification(notification), add = TRUE)
    
    # Cache the data type
    prevState$dataType <- dataTypeSelection()
    prevState$data <- data()
    
    if(using_single_cell) {
      # Single cell data processing
      start_tissue <- input$selectTissueSCStart
      start_celltype <- input$selectCellTypeSCStart
      target_tissue <- input$selectTissueSCTarget
      target_celltype <- input$selectCellTypeSCTarget
      organism <- data_list()$organism
      
      # Cache single cell selections
      prevState$start_tissue_sc <- start_tissue
      prevState$target_tissue_sc <- target_tissue
      prevState$celltype_sc_start <- start_celltype
      prevState$celltype_sc_target <- target_celltype
      
      # Select the appropriate folder based on organism
      folder <- if(organism == "human") {
        "hs_pseudobulk_by_tissue"
      } else {
        "mm_pseudobulk_by_tissue"
      }
      
      # Read expression data for start cell type
      start_expression <- read.csv(file.path("data", folder, start_tissue, start_celltype), 
                                   col.names = c("genes", "expression1"))
      
      # Read expression data for target cell type
      target_expression <- read.csv(file.path("data", folder, target_tissue, target_celltype), 
                                    col.names = c("genes", "expression2"))
      
      # Merge the two expression datasets
      merged_expr <- dplyr::inner_join(start_expression, target_expression, by = "genes")
      
      # Convert to matrix format for decoupleR
      counts <- merged_expr %>%
        dplyr::select(expression1, expression2) %>%
        as.matrix()
      
      colnames(counts) <- c(start_celltype, target_celltype)
      rownames(counts) <- merged_expr$genes
      
    } else {
      # Bulk data processing (original functionality)
      celltype1 <- input$selectTissueTFAStart
      celltype2 <- input$selectTissueTFATarget
      
      # Cache bulk selections
      prevState$celltype1 <- celltype1
      prevState$celltype2 <- celltype2
      
      counts <- data_list()$expression %>%
        dplyr::filter(!duplicated(genes)) %>%
        dplyr::select(all_of(c(celltype1, celltype2))) %>%
        as.matrix()
      
      rownames(counts) <- data_list()$expression %>% 
        dplyr::filter(!duplicated(genes)) %>% 
        dplyr::pull("genes")
    } 
    
    # Run decoupleR analysis (same for both data types)
    res <- decoupleR::decouple(mat=counts,
                               net=data(),
                               .source='from',
                               .target='to',
                               args = list(
                                  wsum = list(.mor = "mor"),
                                  ulm = list(.mor = "mor"),
                                 mlm = list(.mor = "mor")
                               ),
                               minsize = 3) %>%
      dplyr::mutate(score = round(score, 2)) %>%
      dplyr::filter(statistic == "consensus") %>%
      dplyr::select(-run_id)
    
    prevState$res <- res
  }
  
  # Render results (same for both data types)
  output$tbl_tfa <- renderDT({
    # Format the condition names in the results data
    formatted_res <- res %>%
      dplyr::mutate(
        condition = sapply(condition, formatTissueNameForDisplay)
      ) %>%
      dplyr::select(-statistic) 
    
    DT::datatable(formatted_res) %>%
      formatSignif(columns = c('p_value'), digits = 3)
  })
  
  output$tfa_plot <-  renderPlotly({
    
    combined <- inner_join(
      res %>% dplyr::filter(statistic == "consensus") %>% split(.$condition) %>% `[[`(1),
      res %>% dplyr::filter(statistic == "consensus") %>% split(.$condition) %>% `[[`(2),
      by = c("source", "statistic")
    )
    
    # Format the condition names for display
    condition_x_formatted <- formatTissueNameForDisplay(unique(combined$condition.x))
    condition_y_formatted <- formatTissueNameForDisplay(unique(combined$condition.y))
    
    combined$greaterPval <- 1 / pmax(combined$p_value.x, combined$p_value.y)
    combined$symbol <- ifelse(combined$source %in% inputTFs(), "diamond", "circle")
    combined$lineColor <- ifelse(combined$source %in% inputTFs(), "black", "white")
    
    max_val <- max(c(combined$score.x, combined$score.y))
    min_val <- min(c(combined$score.x, combined$score.y))
    
    # Update hover text with formatted condition names
    hover_text <- paste(
      combined$source, 
      "<br>", "p value in", condition_x_formatted, ": ", combined$p_value.x,
      "<br>", "p value in", condition_y_formatted, ": ", combined$p_value.y
    )
    
    plot_ly(combined,
            x = ~score.x, y = ~score.y,
            type = 'scatter',  
            mode = 'markers', 
            marker = list(size = 10, symbol = ~symbol, color = ~greaterPval,
                          line = list(color = ~lineColor, width = 2)),
            text = hover_text) %>%
      layout(shapes = list(
        list(type = "line", x0 = min_val, x1 = max_val, xref = "x",
             y0 = min_val, y1 = max_val, yref = "y",
             line = list(color = "black", width = 1, dash = "dash"))
      ),
      xaxis = list(title = paste0("TFA in ", condition_x_formatted)),
      yaxis = list(title = paste0("TFA in ", condition_y_formatted)),
      annotations = list(
        list(x = 0.05, y = 1, xref = "paper", yref = "paper",
             text = "Diamond: Input TF", showarrow = FALSE, align = "left"),
        list(x = 0.05, y = 0.95, xref = "paper", yref = "paper",
             text = "Color: p-value", showarrow = FALSE, align = "left")
      ))
  })
})