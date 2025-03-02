previousExpInputTFs <- reactiveVal(NULL)

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
  
  
  if (identical(inputTFs(), previousExpInputTFs())) {
    return()
  }
  message("lets show the modal")
  showModal(dataModal_exp())
})

observeEvent(input$okExpBtn, {
  message("ok btn pressed")
  message(Sys.time())
  removeModal()
  
  symbols <- c(inputTFs())
  message("creating plots for the following genes:", symbols)
  nrFigs <- length(symbols)
  
  
  waiter <- waiter::Waiter$new()
  waiter$show()
  on.exit(waiter$hide())
  
  notification_exp <- showNotification(
    glue::glue("Loading data and calculating plots. This might take a few seconds. With many input genes, it might take additional time until all slides are rendered after the calculation."), 
    type = "message", duration = NULL, closeButton = TRUE
  )
  on.exit(removeNotification(notification_exp), add = TRUE)
  
  selected_tissue_start <- input$selectTissueExpStart
  # If comparing two tissues, use the second input; otherwise, set target equal to start.
  if (input$compareTissues == "compare") {
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
  
  # Build carousel screens with a progress indicator.
  screens <- vector("list", nrFigs)
  withProgress(message = "Generating plots...", value = 0, {
    for (i in seq_len(nrFigs)) {
      gene <- symbols[i]
      if (input$compareTissues == "compare") {
        # Two plots per gene: one for start and one for target.
        plot_start_id  <- paste0("plotly_start_", gene)
        plot_target_id <- paste0("plotly_target_", gene)
        screens[[i]] <- shinyglide::screen(
          h3(gene, align = "center"),
          plotly::plotlyOutput(plot_start_id),
          plotly::plotlyOutput(plot_target_id)
        )
      } else {
        # Single tissue: one plot per gene.
        plot_id  <- paste0("plotly_", gene)
        screens[[i]] <- shinyglide::screen(
          h3(gene, align = "center"),
          plotly::plotlyOutput(plot_id)
        )
      }
      incProgress(1/nrFigs)
    }
  })
  
  # Generate the plotly outputs for each gene.
  lapply(seq_len(nrFigs), function(i) {
    gene <- symbols[i]
    if (input$compareTissues == "compare") {
      start_id  <- paste0("plotly_start_", gene)
      target_id <- paste0("plotly_target_", gene)
      
      output[[start_id]] <- plotly::renderPlotly({
        
        if (!gene %in% names(expression_data_start)) {
          return(
            plotly::plot_ly() %>% 
              plotly::layout(
                annotations = list(
                  x = 0.5, y = 0.5,
                  text = paste("No expression data found for", gene, 
                               ". Check for spelling errors and if you used the official gene symbol"),
                  showarrow = FALSE,
                  xref = 'paper', yref = 'paper'
                )
              )
          )
          }
        
        gene_data_start <- dplyr::select(
          expression_data_start,
          dplyr::all_of(c(gene, columns_to_keep))
        )
        plotly::plot_ly(
          data = gene_data_start,
          y = ~ .data[[gene]],
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
      })
      
      output[[target_id]] <- plotly::renderPlotly({
        
        if (!gene %in% names(expression_data_target)) {
          return()
        }
        
        gene_data_target <- dplyr::select(
          expression_data_target,
          dplyr::all_of(c(gene, columns_to_keep))
        )
        plotly::plot_ly(
          data = gene_data_target,
          y = ~ .data[[gene]],
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
      })
    } else {
      # In single tissue mode, render one plot.
      plot_id  <- paste0("plotly_", gene)
      output[[plot_id]] <- plotly::renderPlotly({
        
        
        if (!gene %in% names(expression_data_start)) {
          return(
            plotly::plot_ly() %>% 
              plotly::layout(
                annotations = list(
                  x = 0.5, y = 0.5,
                  text = paste("No expression data found for", gene, 
                               ". Check for spelling errors and if you used the official gene symbol"),
                  showarrow = FALSE,
                  xref = 'paper', yref = 'paper'
                )
              )
          )
        }
        
        gene_data <- dplyr::select(
          expression_data_start,  # same tissue used for start and target
          dplyr::all_of(c(gene, columns_to_keep))
        )
        plotly::plot_ly(
          data = gene_data,
          y = ~ .data[[gene]],
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
      })
    }
  })
  
  # Render the carousel after all screens are ready.
  output$carousel <- renderUI({
    do.call(glide, screens)
  })
  
  previousExpInputTFs(inputTFs())
})

observeEvent(input$changeTissues, {
  showModal(dataModal_exp())
})
