previousExpInputTFs <- reactiveVal(NULL)

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
  
  showModal(dataModal_exp())
})

observeEvent(input$okExpBtn, {
  removeModal()
  
  symbols <- c(inputTFs())
  message("creating plots for the following genes:", symbols)
  nrFigs <- length(symbols)
  
  # Create a spinner overlay.
  spinner <- waiter::Waiter$new(html = waiter::spin_folding_cube())
  spinner$show()
  
  notification_exp <- showNotification(
    glue::glue("Loading Expression data. This might take a few seconds. If you have many input genes, it might take additional time until all slides are rendered"), 
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
  
  expression_data_start <- readRDS(
    file.path(folderInfo()$folder_path, paste0(selected_tissue_start, folderInfo()$file_ending))
  )
  expression_data_target <- readRDS(
    file.path(folderInfo()$folder_path, paste0(selected_tissue_target, folderInfo()$file_ending))
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
  
  columns_to_keep <- c("cell_ontology_class", "free_annotation", "broad_cell_class")
  
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
  spinner$hide()
})

observeEvent(input$changeTissues, {
  showModal(dataModal_exp())
})
