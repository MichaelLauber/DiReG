source("utils/utils_ame.R")

shinyjs::hide(id="download_ame")

pathStart <- c()
pathTarget <- c()
exampleDataLoaded <- FALSE

genomeCompatibility <- list(
  # Mouse genomes compatible with mouse TFBS databases
  mouse = c("mm10", "mm39", "GRCm38", "GRCm39"),
  # Human genomes compatible with human TFBS databases
  human = c("GRCh37", "GRCh38", "hg19", "hg38")
)

# Define TFBS database compatibility
tfbsCompatibility <- list(
  # Mouse-specific TFBS databases
  mouse = c("hocoMouse"),
  # Human-specific TFBS databases
  human = c("hocoHuman"),
  # TFBS databases compatible with any genome
  any = c("hocoCoreV13", "jaspar")
)

observe({
  # When either reference genome or TFBS database changes
  refGenomeKey <- input$radioRefG
  tfbsKey <- input$radioTFBSDB
  
  # Skip validation if either is not selected yet
  if(is.null(refGenomeKey) || is.null(tfbsKey)) {
    return(NULL)
  }
  
  # Determine genome type (mouse or human)
  genomeType <- NULL
  if(refGenomeKey %in% genomeCompatibility$mouse) {
    genomeType <- "mouse"
  } else if(refGenomeKey %in% genomeCompatibility$human) {
    genomeType <- "human"
  } else {
    # Unknown genome type
    return(NULL)
  }
  
  # Check if selected TFBS database is compatible with genome
  isCompatible <- (tfbsKey %in% tfbsCompatibility$any) || 
    (genomeType == "mouse" && tfbsKey %in% tfbsCompatibility$mouse) ||
    (genomeType == "human" && tfbsKey %in% tfbsCompatibility$human)
  
  if(!isCompatible) {
    # Show warning notification
    showNotification(
      paste0("Warning: The selected TFBS database (", tfbsKey, 
             ") may not be compatible with the ", genomeType, " reference genome (", refGenomeKey, ")."),
      type = "warning",
      duration = 10
    )
  }
})

# Helper function to run external processes safely
safeRunProcess <- function(script, args, errorMsg, session) {
  tryCatch({
    res <- processx::run(script, args)
    if(res$status != 0) {
      showModal(modalDialog(
        title = "Process Error",
        HTML(paste0("<p><strong>", errorMsg, "</strong></p>",
                    "<p>Command failed with exit code: ", res$status, "</p>",
                    "<p>Error output: <pre>", res$stderr, "</pre></p>",
                    "<p>Please check if your input data and chosen reference genome are compatible.</p>")),
        easyClose = TRUE,
        footer = modalButton("OK")
      ))
      return(NULL)
    }
    return(res)
  }, error = function(e) {
    showModal(modalDialog(
      title = "Process Error",
      HTML(paste0("<p><strong>", errorMsg, "</strong></p>",
                  "<p>R error message: <pre>", e$message, "</pre></p>",
                  "<p>Please check if your input data and chosen reference genome are compatible.</p>")),
      easyClose = TRUE,
      footer = modalButton("OK")
    ))
    return(NULL)
  })
}

# Helper function to run system commands safely
safeRunSystem <- function(command, errorMsg, session) {
  tryCatch({
    status <- system(command)
    if(status != 0) {
      showModal(modalDialog(
        title = "System Command Error",
        HTML(paste0("<p><strong>", errorMsg, "</strong></p>",
                    "<p>Command failed with exit code: ", status, "</p>",
                    "<p>Please check if your input data and chosen reference genome are compatible.</p>")),
        easyClose = TRUE,
        footer = modalButton("OK")
      ))
      return(FALSE)
    }
    return(TRUE)
  }, error = function(e) {
    showModal(modalDialog(
      title = "System Command Error",
      HTML(paste0("<p><strong>", errorMsg, "</strong></p>",
                  "<p>R error message: <pre>", e$message, "</pre></p>",
                  "<p>Please check if your input data and chosen reference genome are compatible.</p>")),
      easyClose = TRUE,
      footer = modalButton("OK")
    ))
    return(FALSE)
  })
}

observeEvent(input$loadExamplAMEbtn, {
  shinyjs::reset("fileAmeStart")
  shinyjs::reset("fileAmeTarget")
  session$sendCustomMessage(type = "updateFileInputHandler", "fileAmeStart")
  session$sendCustomMessage(type = "updateFileInputHandler", "fileAmeTarget")
  exampleDataLoaded <<- TRUE
})

observe({
  input$fileAmeStart
  if(!is.null(input$fileAmeStart$name)){
    isExtensionPermitted("fileAmeStart", input)
  }
})

observe({
  input$fileAmeTarget
  if(!is.null(input$fileAmeTarget$name)){
    isExtensionPermitted("fileAmeTarget", input)
  }
})

observeEvent(input$btnRunAME, {
  # Start a tryCatch block for the entire process
  tryCatch({
    if(exampleDataLoaded){
      pathStart <- "data/test1.narrowPeak"
      pathTarget <- "data/test2.narrowPeak"
    } else {
      req(isFileUploaded("fileAmeStart", input) & isFileUploaded("fileAmeTarget", input), cancelOutput = TRUE)
      req(isExtensionPermitted("fileAmeStart", input) & isExtensionPermitted("fileAmeTarget", input), cancelOutput = TRUE)
      pathStart <- input$fileAmeStart$datapath
      pathTarget <- input$fileAmeTarget$datapath
    }
    
    refGenome <- switch(input$radioRefG,
                        "mm10" = "refGenome/mm10.fa",
                        "mm39" ="refGenome/mm39.fa",
                        "GRCm38" = "refGenome/GRCm38.fa", #Still missing
                        "GRCm39"= "refGenome/GRCm39.fa",
                        "GRCh37" = "refGenome/GRCh37.fa",
                        "GRCh38" = "refGenome/GRCh38.fa",
                        "hg19" = "refGenome/hg19.fa",
                        "hg38" = "refGenome/hg38.fa"
    )
    
    motifFile <- switch(input$radioTFBSDB,
                        "hocoMouse" = "data/motiffiles/HOCOMOCOv11_core_MOUSE_mono_meme_format.meme",
                        "hocoHuman" = "data/motiffiles/H11_HUMAN_mono_meme_format.meme",
                        "hocoCoreV13" = "data/motiffiles/H13CORE_meme_format.meme",
                        "jaspar" = "data/motiffiles/JASPAR2024_CORE_vertebrates_non-redundant_pfms_meme.txt"
    )
    
    annotationFile <- switch(input$radioTFBSDB,
                             "hocoMouse" = "data/motiffiles/HOCOMOCOv11_full_annotation_MOUSE_mono.tsv",
                             "hocoHuman" = "data/motiffiles/HOCOMOCOv11_full_annotation_HUMAN_mono.tsv",
                             "hocoCoreV13" = "data/motiffiles/HOCOMOCOv13_annotation.tsv",
                             "jaspar" = ""
    )
    
    if(dir.exists("homer_tmp_bg")) {
      unlink("homer_tmp_bg", recursive = TRUE)
    }
    tempdir_path <- tempdir()
    temp_files <- list.files(tempdir_path, full.names = TRUE, pattern = "^file")
    if(length(temp_files) > 0) {
      sapply(temp_files, unlink)
    }
    
    waiter <- waiter::Waiter$new()
    waiter$show()
    on.exit(waiter$hide(), add = TRUE)
    
    # Check if files exist
    if(!file.exists(pathStart)) {
      showModal(modalDialog(
        title = "File Error",
        "Start file does not exist. Please check your inputs.",
        easyClose = TRUE,
        footer = modalButton("OK")
      ))
      return(NULL)
    }
    
    if(!file.exists(pathTarget)) {
      showModal(modalDialog(
        title = "File Error",
        "Target file does not exist. Please check your inputs.",
        easyClose = TRUE,
        footer = modalButton("OK")
      ))
      return(NULL)
    }
    
    if(!file.exists(refGenome)) {
      showModal(modalDialog(
        title = "Reference Genome Error",
        "Selected reference genome file does not exist. Please check your installation.",
        easyClose = TRUE,
        footer = modalButton("OK")
      ))
      return(NULL)
    }
    
    # Get file extension
    ext <- tools::file_ext(input$fileAmeTarget$name)
    if(exampleDataLoaded) ext <- "narrowPeak"
    
    # Filter regions in narrow and brod Peak File with FDR > 0.05
    stepNr <- 1
    if(ext %in% c("narrowPeak", "broadPeak")){
      id_filter1 <- showNotification(glue::glue("Step {stepNr}: Filter out regions with FDR > 0.05"), type = "message", duration = NULL, closeButton = TRUE)
      Sys.sleep(1)
      
      filteredStartFile.tmp <- tempfile()
      res <- safeRunProcess("scripts/filterByQvalue.sh", c(pathStart, filteredStartFile.tmp), 
                            "Error: Filtering by FDR value failed for start file", session)
      if(is.null(res)) return(NULL)
      pathStart <- filteredStartFile.tmp
      
      filteredTargetFile.tmp <- tempfile()
      res <- safeRunProcess("scripts/filterByQvalue.sh", c(pathTarget, filteredTargetFile.tmp),
                            "Error: Filtering by FDR value failed for target file", session)
      if(is.null(res)) return(NULL)
      pathTarget <- filteredTargetFile.tmp
      
      on.exit(removeNotification(id_filter1), add = TRUE)
    }
    
    # Find DAR Regions
    stepNr <- stepNr + 1
    id_dag <- showNotification(glue::glue("Step {stepNr}: Using bedtools to find differentially accessible regions"), type = "message", duration = NULL, closeButton = TRUE)
    Sys.sleep(1)
    dagFile.tmp <- tempfile()
    res <- safeRunProcess("scripts/runBedToolsIS.sh", c(pathStart, pathTarget, dagFile.tmp),
                          "Error: Calculating differentially accessible regions failed", session)
    if(is.null(res)) return(NULL)
    on.exit(removeNotification(id_dag), add = TRUE)
    
    # Filter for Top10% Regions in narrow peak file
    if(ext == "narrowPeak"){
      stepNr <- stepNr + 1
      id_select <- showNotification(glue::glue("Step {stepNr}: Selection of Top 10% Regions"), type = "message", duration = NULL, closeButton = TRUE)
      Sys.sleep(1)
      filteredDAGFile.tmp <- tempfile()
      res <- safeRunProcess("scripts/selectTop10perc.sh", c(dagFile.tmp, filteredDAGFile.tmp),
                            "Error: Selection of Top 10% Regions failed", session)
      if(is.null(res)) return(NULL)
      dagFile.tmp <- filteredDAGFile.tmp
      on.exit(removeNotification(id_select), add = TRUE)
    }
    
    # Sort bed files by coordiantes
    stepNr <- stepNr + 1
    id_sort <- showNotification(glue::glue("Step {stepNr}: Sorting File by coordinates"), type = "message", duration = NULL, closeButton = TRUE)
    Sys.sleep(2)
    sortedFile.tmp <- tempfile()
    res <- safeRunProcess("scripts/sortFile.sh", c(dagFile.tmp, sortedFile.tmp),
                          "Error: Sorting by coordinates failed", session)
    if(is.null(res)) return(NULL)
    on.exit(removeNotification(id_sort), add = TRUE)
    
    # Generate Fasta File from sorted bed file
    stepNr <- stepNr + 1
    id_genFasta <- showNotification(glue::glue("Step {stepNr}: Generating Fasta Files"), type = "message", duration = NULL, closeButton = TRUE)
    Sys.sleep(2)
    fastaFile.tmp <- tempfile()
    res <- safeRunProcess("scripts/makeFastaFile.sh", c(sortedFile.tmp, refGenome, fastaFile.tmp),
                          "Error: Generating Fasta Files failed. Check if your input data and reference genome are compatible.", session)
    if(is.null(res)) return(NULL)
    on.exit(removeNotification(id_genFasta), add = TRUE)
    
    # Run AME Calculation
    if(input$radioBg == "shuffled") {
      stepNr <- stepNr + 1
      id_shuffle <- showNotification(glue::glue("Step {stepNr}: Running AME. This can take up to a few minutes."), type = "message", duration = NULL, closeButton = TRUE)
      Sys.sleep(1)
      result.tmpdir <- tempdir()
      res <- safeRunProcess("scripts/runAME-shuffled.sh", c(fastaFile.tmp, motifFile, result.tmpdir),
                            "Error: Running AME with shuffled background failed", session)
      if(is.null(res)) return(NULL)
      on.exit(removeNotification(id_shuffle), add = TRUE)
    } else {
      stepNr <- stepNr + 1
      id_homer <- showNotification(glue::glue("Step {stepNr}: Generating Background FastaFile. This can take up to a few minutes."), type = "message", duration = NULL, closeButton = TRUE)
      Sys.sleep(2)
      
      bgDir.tmp <- "homer_tmp_bg"
      if (!dir.exists(bgDir.tmp)) {
        dir.create(bgDir.tmp, recursive = TRUE)
      }
      # Ensure the directory gets deleted on exit.
      on.exit(unlink(bgDir.tmp, recursive = TRUE), add = TRUE)
      
      resizedPeakfile.tmp <- tempfile()
      
      # Set PATH for homer
      oldPath <- Sys.getenv("PATH")
      Sys.setenv(PATH = paste(oldPath, "/nfs/home/users/michaell2/homer/bin", sep=":"))
      on.exit(Sys.setenv(PATH = oldPath), add = TRUE)
      
      # Safely run resize_bed.pl
      resize_command <- paste("scripts/resize_bed.pl", sortedFile.tmp, "200", ">", resizedPeakfile.tmp)
      success <- safeRunSystem(resize_command, 
                               "Error: Failed to resize bed file", session)
      if(!success) return(NULL)
      
      # Safely run homer2 bg
      homer_command <- paste("homer2 bg",
                             "-p", resizedPeakfile.tmp,
                             "-g", refGenome,
                             "-pkmer 2 -N 1000 -NN 1000000",
                             "-o", paste(bgDir.tmp, "/bg", sep=""),
                             "-allowTargetOverlaps -allowBgOverlaps")
      success <- safeRunSystem(homer_command, 
                               "Error: Failed to generate Homer background", session)
      if(!success) return(NULL)
      
      on.exit(removeNotification(id_homer), add = TRUE)
      
      stepNr <- stepNr + 1
      id_ame <- showNotification(glue::glue("Step {stepNr}: Running AME"), type = "message", duration = NULL, closeButton = TRUE)
      Sys.sleep(1)
      
      # Check if background file exists
      bgFile <- file.path(bgDir.tmp, "bg.bg.sequences.fasta")
      if(!file.exists(bgFile)) {
        showModal(modalDialog(
          title = "Background File Error",
          "Background fasta file was not generated correctly. Please check Homer installation.",
          easyClose = TRUE,
          footer = modalButton("OK")
        ))
        return(NULL)
      }
      
      result.tmpdir <- tempdir()
      res <- safeRunProcess("scripts/runAME.sh", c(fastaFile.tmp, motifFile, bgFile, result.tmpdir),
                            "Error: Running AME failed", session)
      if(is.null(res)) return(NULL)
      on.exit(removeNotification(id_ame), add = TRUE)
    }
    
    # Check if result file exists and is readable
    resultFile <- file.path(result.tmpdir, "ame.tsv")
    if(!file.exists(resultFile)) {
      showModal(modalDialog(
        title = "Results File Error",
        "AME did not produce the expected results file. Please check your inputs and try again.",
        easyClose = TRUE,
        footer = modalButton("OK")
      ))
      return(NULL)
    }
    
    # Read the results file safely
    tryCatch({
      res_df <- vroom::vroom(resultFile, delim = "\t")
      
      # Check if the file has content
      if(nrow(res_df) <= 3) {
        showModal(modalDialog(
          title = "Results Error",
          "AME produced an empty or invalid results file. This typically happens when no motifs match your data or there's an issue with the input sequences.",
          easyClose = TRUE,
          footer = modalButton("OK")
        ))
        return(NULL)
      }
      
      res_df <- res_df[1:(dim(res_df)[1]-3),]
      
      if(input$radioTFBSDB == "jaspar" || input$radioTFBSDB == "hocoCoreV13"){
        res_df$TF <- toupper(res_df$motif_alt_ID)
      } else {
        # Safely read annotation file
        if(!file.exists(annotationFile)) {
          showModal(modalDialog(
            title = "Annotation File Error",
            paste("Could not find annotation file:", annotationFile),
            easyClose = TRUE,
            footer = modalButton("OK")
          ))
          return(NULL)
        }
        
        annotation <- vroom::vroom(annotationFile)
        hoco_mapping <- annotation[,c(1:2)]
        ids <- match(res_df$motif_ID, hoco_mapping$Model)
        TFs <- hoco_mapping$`Transcription factor`[ids]
        TFs <- toupper(TFs)
        res_df$TF <- TFs
      }
      
      res_df <- res_df %>%
        dplyr::select(rank, TF, motif_ID, 'adj_p-value')
      
      ame_results <<- res_df
      
      output$ame_res <- DT::renderDataTable({
        DT::datatable(ame_results, options = list(pageLength = 10))
      })
      shinyjs::show(id="download_ame")
      
    }, error = function(e) {
      showModal(modalDialog(
        title = "Results Processing Error",
        HTML(paste0("<p>Failed to process AME results: <pre>", e$message, "</pre></p>",
                    "<p>Please check your input data and try again.</p>")),
        easyClose = TRUE,
        footer = modalButton("OK")
      ))
    })
    
  }, error = function(e) {
    # Catch-all error handler
    waiter <- waiter::Waiter$new()
    waiter$hide()
    showModal(modalDialog(
      title = "Unexpected Error",
      HTML(paste0("<p>An unexpected error occurred: <pre>", e$message, "</pre></p>",
                  "<p>Please check if your input data and chosen reference genome are compatible.</p>")),
      easyClose = TRUE,
      footer = modalButton("OK")
    ))
  })
})

output$download_ame <- downloadHandler(
  filename = function() {paste("AME-Run_", Sys.Date(), ".tsv", sep = "")},
  content = function(file){
    tryCatch({
      vroom::vroom_write(ame_results, file)
    }, error = function(e) {
      showModal(modalDialog(
        title = "Download Error",
        HTML(paste0("<p>Failed to save results: <pre>", e$message, "</pre></p>")),
        easyClose = TRUE,
        footer = modalButton("OK")
      ))
    })
  }
)