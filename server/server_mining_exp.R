previousExpInputTFs <- reactiveVal(NULL)
# Reactive values to store plots and navigation state
plot_storage <- reactiveValues(
  plots = list(),
  ready = FALSE,
  current_index = 1
)

folderInfo_exp <- reactive({
  if (input$radioOrgDorothea == "human") {
    folder <- "hs_tfs_rds_files"
    file_ending <- "_human_TFs.rds"
  } else {
    folder <- "mm_tfs_rds_files"
    file_ending <- "_tmuris_tfs.rds"
  }
  res <- list(
    folder_path = file.path("data", folder),
    file_ending = file_ending
  )
  message("folder in mining exp")
  print(paste("For expression, using folder:", res$folder_path, "with ending:", res$file_ending))
  res
})

files_exp <- reactive({
  list.files(folderInfo_exp()$folder_path)
})

tissues_exp <- reactive({
  print("tissue_exp")
  print("in exp")
  message(stringr::str_replace_all(files_exp(), "_(human|tmuris)_(T|t)(F|f)s.rds", ""))
  stringr::str_replace_all(files_exp(), "_(human|tmuris)_(T|t)(F|f)s.rds", "")
})

dataModal_exp <- function(failed = FALSE) {
  modalDialog(
    # Radio button to choose display option.
    radioButtons("compareTissues", "Display Option", 
                 choices = c("Single Tissue" = "single", "Compare Two Tissues" = "compare"), 
                 selected = "compare", inline = TRUE),
    
    # Tissue selection: always show one.
    selectizeInput("selectTissueExpStart", 
                   h3("Tissue"),
                   choices = tissues_exp(),
                   selected = tissues_exp()[1],
                   width = "230px"),
    
    # Conditionally show the second tissue if comparing.
    conditionalPanel(
      condition = "input.compareTissues == 'compare'",
      selectizeInput("selectTissueExpTarget", 
                     h3("Target Tissue"),
                     choices = tissues_exp(),
                     selected = tissues_exp()[2],
                     width = "230px")
    ),
    
    span('Analyze TF Expression'),
    if (failed)
      div(tags$b("Invalid name of data object", style = "color: red;")),
    
    footer = tagList(
      modalButton("Cancel"),
      actionButton("okExpBtn", "OK")
    )
  )
}

observeEvent(input$btnGTEx, {
  if (!networkCreated) {
    shinyalert::shinyalert(
      "There are no Genes to Analyse",
      "Please input genes and press the RUN button before performing any analysis",
      type = "error"
    )
    return()
  }
  
  message("lets show the modal")
  showModal(dataModal_exp())
})

# Create a better function to generate plots for a gene
generatePlots <- function(gene, expression_data_start, expression_data_target, 
                          selected_tissue_start, selected_tissue_target, 
                          columns_to_keep, compare_tissues) {
  
  result <- list()
  
  if (compare_tissues) {
    # Create plotly objects for both tissues
    if (gene %in% names(expression_data_start)) {
      gene_data_start <- dplyr::select(
        expression_data_start,
        dplyr::all_of(c(gene, columns_to_keep))
      )
      # Create a formula for plotly to correctly reference the gene column
      y_formula <- eval(parse(text = paste0("~`", gene, "`")))
      start_plot <- plotly::plot_ly(
        data = gene_data_start,
        y = y_formula,
        x = ~ cell_ontology_class,
        type = "violin",
        split = ~ cell_ontology_class,
        box = list(visible = TRUE),
        meanline = list(visible = TRUE),
        spanmode = "hard",
        marker = list(size = 3)
      ) %>%
        plotly::layout(
          xaxis = list(
            title = paste0("Start Tissue: ", selected_tissue_start),
            tickangle = 45
          ),
          yaxis = list(
            title = "",
            zeroline = FALSE
          ),
          showlegend = FALSE
        )
    } else {
      start_plot <- plotly::plot_ly() %>% 
        plotly::layout(
          annotations = list(
            x = 0.5, y = 0.5,
            text = paste("No expression data found for", gene, 
                         ". Check for spelling errors and if you used the official gene symbol"),
            showarrow = FALSE,
            xref = 'paper', yref = 'paper'
          )
        )
    }
    
    if (gene %in% names(expression_data_target)) {
      gene_data_target <- dplyr::select(
        expression_data_target,
        dplyr::all_of(c(gene, columns_to_keep))
      )
      # Create a formula for plotly to correctly reference the gene column
      y_formula <- eval(parse(text = paste0("~`", gene, "`")))
      target_plot <- plotly::plot_ly(
        data = gene_data_target,
        y = y_formula,
        x = ~ cell_ontology_class,
        type = "violin",
        split = ~ cell_ontology_class,
        box = list(visible = TRUE),
        meanline = list(visible = TRUE),
        spanmode = "hard",
        marker = list(size = 3)
      ) %>%
        plotly::layout(
          xaxis = list(
            title = paste0("Target Tissue: ", selected_tissue_target),
            tickangle = 45
          ),
          yaxis = list(
            title = "",
            zeroline = FALSE
          ),
          showlegend = FALSE
        )
    } else {
      target_plot <- plotly::plot_ly() %>% 
        plotly::layout(
          annotations = list(
            x = 0.5, y = 0.5,
            text = paste("No expression data found for", gene, 
                         ". Check for spelling errors and if you used the official gene symbol"),
            showarrow = FALSE,
            xref = 'paper', yref = 'paper'
          )
        )
    }
    
    result$start_plot <- start_plot
    result$target_plot <- target_plot
    
  } else {
    # Single tissue
    if (gene %in% names(expression_data_start)) {
      gene_data <- dplyr::select(
        expression_data_start,
        dplyr::all_of(c(gene, columns_to_keep))
      )
      
      # Create a formula for plotly to correctly reference the gene column
      y_formula <- eval(parse(text = paste0("~`", gene, "`")))
      single_plot <- plotly::plot_ly(
        data = gene_data,
        y = y_formula,
        x = ~ cell_ontology_class,
        type = "violin",
        split = ~ cell_ontology_class,
        box = list(visible = TRUE),
        meanline = list(visible = TRUE),
        spanmode = "hard",
        marker = list(size = 3)
      ) %>%
        plotly::layout(
          xaxis = list(
            title = paste0("Tissue: ", selected_tissue_start),
            tickangle = 45
          ),
          yaxis = list(
            title = "",
            zeroline = FALSE
          ),
          showlegend = FALSE
        )
    } else {
      single_plot <- plotly::plot_ly() %>% 
        plotly::layout(
          annotations = list(
            x = 0.5, y = 0.5,
            text = paste("No expression data found for", gene, 
                         ". Check for spelling errors and if you used the official gene symbol"),
            showarrow = FALSE,
            xref = 'paper', yref = 'paper'
          )
        )
    }
    
    result$single_plot <- single_plot
  }
  
  return(result)
}

observeEvent(input$okExpBtn, {
  message("ok btn pressed")
  message(Sys.time())
  removeModal()
  
  symbols <- c(inputTFs())
  message("creating plots for the following genes:", symbols)
  nrFigs <- length(symbols)
  
  # Reset the current index and mark as not ready while processing
  plot_storage$current_index <- 1
  plot_storage$ready <- FALSE
  
  waiter <- waiter::Waiter$new()
  waiter$show()
  on.exit(waiter$hide())
  
  notification_exp <- showNotification(
    glue::glue("Loading data and calculating plots. This might take a few seconds."), 
    type = "message", duration = NULL, closeButton = TRUE
  )
  on.exit(removeNotification(notification_exp), add = TRUE)
  
  selected_tissue_start <- input$selectTissueExpStart
  # If comparing two tissues, use the second input; otherwise, set target equal to start.
  compare_tissues <- input$compareTissues == "compare"
  if (compare_tissues) {
    selected_tissue_target <- input$selectTissueExpTarget
  } else {
    selected_tissue_target <- selected_tissue_start
  }
  
  print("the following files will be loaded")
  message(file.path(folderInfo_exp()$folder_path, paste0(selected_tissue_start, folderInfo_exp()$file_ending)))
  message(file.path(folderInfo_exp()$folder_path, paste0(selected_tissue_target, folderInfo_exp()$file_ending)))
  
  expression_data_start <- readRDS(
    file.path(folderInfo_exp()$folder_path, paste0(selected_tissue_start, folderInfo_exp()$file_ending))
  )
  expression_data_target <- readRDS(
    file.path(folderInfo_exp()$folder_path, paste0(selected_tissue_target, folderInfo_exp()$file_ending))
  )
  
  # Apply filtering only if the organism is human.
  if (input$radioOrgDorothea == "human") {
    annotation_counts_start <- table(expression_data_start$free_annotation)
    valid_annotations_start <- names(annotation_counts_start[annotation_counts_start >= 40])
    expression_data_start <- expression_data_start[
      expression_data_start$free_annotation %in% valid_annotations_start, 
    ]
    
    annotation_counts_target <- table(expression_data_target$free_annotation)
    valid_annotations_target <- names(annotation_counts_target[annotation_counts_target >= 40])
    expression_data_target <- expression_data_target[
      expression_data_target$free_annotation %in% valid_annotations_target, 
    ]
  }
  
  columns_to_keep <- c("cell_ontology_class", "free_annotation")
  
  # Generate plots for each gene
  plotObjects <- list()
  geneInfoList <- list()
  
  withProgress(message = "Generating plots...", value = 0, {
    for (i in seq_len(nrFigs)) {
      gene <- symbols[i]
      geneInfoList[[i]] <- list(gene = gene, index = i)
      
      # Generate plots for this gene
      plots <- generatePlots(
        gene = gene,
        expression_data_start = expression_data_start,
        expression_data_target = expression_data_target,
        selected_tissue_start = selected_tissue_start,
        selected_tissue_target = selected_tissue_target,
        columns_to_keep = columns_to_keep,
        compare_tissues = compare_tissues
      )
      
      # Store plots with consistent naming
      if (compare_tissues) {
        plotObjects[[paste0("start_", i)]] <- plots$start_plot
        plotObjects[[paste0("target_", i)]] <- plots$target_plot
      } else {
        plotObjects[[paste0("single_", i)]] <- plots$single_plot
      }
      
      incProgress(1/nrFigs)
    }
  })
  
  # Store all plot objects and info in reactive values
  plot_storage$plots <- plotObjects
  plot_storage$genes <- geneInfoList
  plot_storage$nrFigs <- nrFigs
  plot_storage$compareTissues <- compare_tissues
  
  # Mark as ready now that all data is available
  plot_storage$ready <- TRUE
  
  previousExpInputTFs(inputTFs())
})

# Define the output renderers once globally
output$plot_single_current <- plotly::renderPlotly({
  req(plot_storage$ready)
  current_index <- plot_storage$current_index
  plot_storage$plots[[paste0("single_", current_index)]]
})

output$plot_start_current <- plotly::renderPlotly({
  req(plot_storage$ready)
  current_index <- plot_storage$current_index
  plot_storage$plots[[paste0("start_", current_index)]]
})

output$plot_target_current <- plotly::renderPlotly({
  req(plot_storage$ready)
  current_index <- plot_storage$current_index
  plot_storage$plots[[paste0("target_", current_index)]]
})

# Render custom UI with navigation
output$carousel <- renderUI({
  req(plot_storage$ready)
  
  current_index <- plot_storage$current_index
  gene_info <- plot_storage$genes[[current_index]]
  gene <- gene_info$gene
  
  # Create navigation buttons
  prev_button <- tags$button(
    id = "prev_gene_btn",
    class = "btn btn-primary btn-lg",
    style = "position: absolute; left: 20px; top: 50%; transform: translateY(-50%); z-index: 1000;",
    HTML("&laquo; Previous")
  )
  
  next_button <- tags$button(
    id = "next_gene_btn",
    class = "btn btn-primary btn-lg",
    style = "position: absolute; right: 20px; top: 50%; transform: translateY(-50%); z-index: 1000;",
    HTML("Next &raquo;")
  )
  
  # Only show prev button if not at first item
  if (current_index == 1) {
    prev_button <- NULL
  }
  
  # Only show next button if not at last item
  if (current_index == plot_storage$nrFigs) {
    next_button <- NULL
  }
  
  # Content based on compare/single mode
  if (plot_storage$compareTissues) {
    content <- div(
      style = "position: relative; padding: 40px 70px;",
      h3(gene, align = "center"),
      div(
        style = "display: flex; flex-direction: column; gap: 20px;",
        plotly::plotlyOutput("plot_start_current", height = "350px"),
        plotly::plotlyOutput("plot_target_current", height = "350px")
      ),
      prev_button,
      next_button,
      # Caption with counter
      div(
        style = "text-align: center; margin-top: 20px; font-size: 16px;",
        paste0("Gene ", current_index, " of ", plot_storage$nrFigs)
      )
    )
  } else {
    content <- div(
      style = "position: relative; padding: 40px 70px;",
      h3(gene, align = "center"),
      div(
        style = "display: flex; flex-direction: column;",
        plotly::plotlyOutput("plot_single_current", height = "500px")
      ),
      prev_button,
      next_button,
      # Caption with counter
      div(
        style = "text-align: center; margin-top: 20px; font-size: 16px;",
        paste0("Gene ", current_index, " of ", plot_storage$nrFigs)
      )
    )
  }
  
  # Add JS for button click handling
  content <- tagList(
    content,
    tags$script(HTML("
      $(document).ready(function() {
        $('#prev_gene_btn').on('click', function() {
          Shiny.setInputValue('prev_gene_clicked', Math.random());
        });
        
        $('#next_gene_btn').on('click', function() {
          Shiny.setInputValue('next_gene_clicked', Math.random());
        });
      });
    "))
  )
  
  return(content)
})

# Handle previous button click
observeEvent(input$prev_gene_clicked, {
  if (plot_storage$current_index > 1) {
    plot_storage$current_index <- plot_storage$current_index - 1
  }
})

# Handle next button click
observeEvent(input$next_gene_clicked, {
  if (plot_storage$current_index < plot_storage$nrFigs) {
    plot_storage$current_index <- plot_storage$current_index + 1
  }
})

observeEvent(input$changeTissues, {
  showModal(dataModal_exp())
})