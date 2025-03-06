source("utils/utils_ame.R")


shinyjs::hide(id="download_ame")


pathStart <- c()
pathTarget <- c()
exampleDataLoaded <- FALSE


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
  if(exampleDataLoaded){
    pathStart <- "data/test1.narrowPeak"
    pathTarget <- "data/test2.narrowPeak"
  } else {
    req(isFileUploaded("fileAmeStart", input) & isFileUploaded("fileAmeTarget", input), cancelOutput = TRUE)
    req(isExtensionPermitted("fileAmeStart", input) & isExtensionPermitted("fileAmeTarget", input), cancelOutput = TRUE)
    pathStart <- input$fileAmeStart$datapath
    pathTarget <- input$fileAmeTarget$datapath
  }
  
  
  #refGenome <- "/nfs/home/users/michaell/programms/ReprogrammingRecovery/data/mm10.fa"
  refGenome <- switch(input$radioRefG,
                      
                      "mm10" = "/nfs/home/users/michaell/programms/ReprogrammingRecovery/data/mm10.fa",
                      "mm39" ="",
                      "GRCm38" = "",
                      "GRCm39"= "",
                      "GRCh37" = "GRCh37",
                      "GRCh38" = "GRCh38",
                      "hg19" = "hg19",
                      "hg38" = "hg38"
  )
  
  #motifFile <- "data/motiffiles/JASPAR2024_CORE_vertebrates_non-redundant_pfms_meme.txt"
  motifFile <- switch(input$radioTFBSDB,
                       #"hocoMouse" = "data/motiffiles/HOCOMOCOv11_core_MOUSE_mono_meme_format.meme",
                       "hocoHuman" = "data/motiffiles/H11_HUMAN_mono_meme_format.meme",
                       "hocoCoreV13" = "data/motiffiles/H13CORE_meme_format.meme",
                       "jaspar" = "data/motiffiles/JASPAR2024_CORE_vertebrates_non-redundant_pfms_meme.txt",
                      "hocoMouse" = "/nfs/home/users/michaell/DiReG/hocoTest.meme",
                      #"hocoHuman" = "/nfs/home/users/michaell/DiReG/hocoHTest.meme",
                      #"jaspar" = "/nfs/home/users/michaell/DiReG/jasparTest.meme"
  )
  
  #annotationFile <- "data/motiffiles/HOCOMOCOv13_annotation.tsv"
  annotationFile <- switch(input$radioTFBSDB,
                           "hocoMouse" = "data/motiffiles/HOCOMOCOv11_full_annotation_MOUSE_mono.tsv",
                           "hocoHuman" = "data/motiffiles/HOCOMOCOv11_full_annotation_HUMAN_mono.tsv",
                           "hocoCoreV13" = "data/motiffiles/HOCOMOCOv13_annotation.tsv",
                           "jaspar" = "",
  )
  
  
  ##to do check if input correct
  ## add advanced filter option for p value
  ## add advanced filter option for top values
  
  waiter <- waiter::Waiter$new()
  waiter$show()
  on.exit(waiter$hide())
  
  
  # refUCSC <- (refGenome %in% c("mm10", "mm39","hg19", "hg38") )
  # res <- processx::run("/usr/bin/grep", c("chr[1-9]", pathStart ))
  # if(res$status != 0 & refUCSC){
  #   showNotification("input file with 'chr' prefix only works with UCSC Ref Genome", type = "error", duration = NULL,)
  #   canRun <- FALSE
  # }
  # if(res$status == 0 & !refUCSC){
  #   showNotification("input file with 'chr' prefix only works with UCSC Ref Genome", type = "error", duration = NULL,)
  #   canRun <- FALSE
  # }
  # req(canRun, cancelOutput = TRUE)
  
  
  
  ext <- tools::file_ext(input$fileAmeTarget$name)
  if(exampleDataLoaded) ext <-"narrowPeak"
  
  # Filter regions in narrow and brod Peak File with FDR > 0.05
  stepNr <- 1
  if(ext %in% c("narrowPeak", "broadPeak")){
    id_filter1 <- showNotification(glue::glue("Step {stepNr}: Filter out regions with FDR > 0.05"), type = "message", duration = NULL, closeButton = TRUE)
    Sys.sleep(1)
    
    filteredStartFile.tmp <- tempfile()
    res <- processx::run("scripts/filterByQvalue.sh", c(pathStart, filteredStartFile.tmp ))
    checkStatus(res, "Error: Filtering by FDR value failed")
    pathStart <- filteredStartFile.tmp
    
    filteredTargetFile.tmp <- tempfile()
    res <- processx::run("scripts/filterByQvalue.sh", c(pathTarget, filteredTargetFile.tmp ))
    checkStatus(res, "Error: Filtering by FDR value failed")
    pathTarget <- filteredTargetFile.tmp
    
    on.exit(removeNotification(id_filter1), add = TRUE)
  }
  
  # Find DAR Regions
  stepNr <- stepNr + 1
  id_dag <- showNotification(glue::glue("Step {stepNr}: Using bedtools to find differentially accessible regions"), type = "message", duration = NULL, closeButton = TRUE)
  Sys.sleep(1)
  dagFile.tmp <- tempfile()
  res <- processx::run("scripts/runBedToolsIS.sh", c(pathStart, pathTarget, dagFile.tmp ))
  checkStatus(res, "Error: Calculating differentially accessible regions failed")
  on.exit(removeNotification(id_dag), add = TRUE)
  
  # Filter for Top10% Regions in narrow peak file
  if(ext == "narrowPeak"){
    stepNr <- stepNr + 1
    id_select <- showNotification(glue::glue("Step {stepNr}: Selection of Top 10% Regions"), type = "message", duration = NULL, closeButton = TRUE)
    Sys.sleep(1)
    filteredDAGFile.tmp <- tempfile()
    res <- processx::run("scripts/selectTop10perc.sh", c(dagFile.tmp,  filteredDAGFile.tmp ))
    checkStatus(res, "Error: Selection of Top 10% Regions failed")
    dagFile.tmp <- filteredDAGFile.tmp
    on.exit(removeNotification(id_select), add = TRUE)
  }
  
  # Sort bed files by coordiantes
  stepNr <- stepNr + 1
  id_sort <- showNotification(glue::glue("Step {stepNr}: Sorting File by coordinates"), type = "message", duration = NULL, closeButton = TRUE)
  Sys.sleep(2)
  sortedFile.tmp <- tempfile()
  res <- processx::run("scripts/sortFile.sh", c(dagFile.tmp, sortedFile.tmp ))
  checkStatus(res, "Error: Sorting by coordinates failed")
  on.exit(removeNotification(id_sort), add = TRUE)
  
  # Generate Fasta File from sorted bed file
  stepNr <- stepNr + 1
  id_genFasta <- showNotification(glue::glue("Step {stepNr}: Generating Fasta Files"), type = "message", duration = NULL, closeButton = TRUE)
  Sys.sleep(2)
  fastaFile.tmp <- tempfile()
  res <- processx::run("scripts/makeFastaFile.sh", c(sortedFile.tmp, refGenome, fastaFile.tmp ))
  checkStatus(res, "Error: Generating Fasta Files failed")
  on.exit(removeNotification(id_genFasta), add = TRUE)
  
  # Run AME Calculation
  if(input$radioBg == "shuffled") {
    stepNr <- stepNr + 1
    id_shuffle <- showNotification(glue::glue("Step {stepNr}: Running AME"), type = "message", duration = NULL, closeButton = TRUE)
    Sys.sleep(1)
    result.tmpdir <- tempdir()
    res <- processx::run("scripts/runAME-shuffled.sh", c(fastaFile.tmp, motifFile, result.tmpdir))
    checkStatus(res, "Error: Running AME failed")
    on.exit(removeNotification(id_shuffle), add = TRUE)
  } else {
    stepNr <- stepNr + 1
    id_homer <- showNotification(glue::glue("Step {stepNr}: Generating Background FastaFile"), type = "message", duration = NULL, closeButton = TRUE)
    Sys.sleep(2)
    
    bgDir.tmp <- "homer_tmp_bg"
    if (!dir.exists(bgDir.tmp)) {
      dir.create(bgDir.tmp, recursive = TRUE)
    }
    # Ensure the directory gets deleted on exit.
    on.exit(unlink(bgDir.tmp, recursive = TRUE), add = TRUE)
    
    resizedPeakfile.tmp <- tempfile()
  
    Sys.setenv(PATH = paste(Sys.getenv("PATH"), "/nfs/home/users/michaell2/homer/bin", sep=":"))
    
    system(paste("scripts/resize_bed.pl", sortedFile.tmp, "200", ">", resizedPeakfile.tmp))
    
    system(paste("homer2 bg",
                 "-p",resizedPeakfile.tmp,
                 "-g",refGenome,
                 "-pkmer 2 -N 1000 -NN 1000000",
                 "-o", paste(bgDir.tmp, "/bg", sep=""),
                 "-allowTargetOverlaps -allowBgOverlaps"
                 ))
    

    on.exit(removeNotification(id_homer), add = TRUE)
     
    stepNr <- stepNr + 1
    id_ame <- showNotification(glue::glue("Step {stepNr}: Running AME"), type = "message", duration = NULL, closeButton = TRUE)
    Sys.sleep(1)
    bgFile <- file.path(bgDir.tmp, "bg.bg.sequences.fasta")
    result.tmpdir <- tempdir()
    res <- processx::run("scripts/runAME.sh", c(fastaFile.tmp, motifFile, bgFile, result.tmpdir ))
    #res <- processx::run("scripts/runAME.sh", c(fastaFile.tmp, motifFile, "homer_tmp_bg.bg.sequences.fasta", result.tmpdir ))
    checkStatus(res, "Error: Running AME failed")
    #removeNotification(id)
    on.exit(removeNotification(id_ame), add = TRUE)
  }
  
  
  res_df <- vroom::vroom(file.path(result.tmpdir, "ame.tsv"), delim = "\t")
  res_df <- res_df[1:(dim(res_df)[1]-3),]
  
  if(input$radioTFBSDB == "jaspar" || input$radioTFBSDB == "hocoCoreV13"){
    res_df$TF <- toupper(res_df$motif_alt_ID)
  } else {
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
  
})
 

output$download_ame <- downloadHandler(
  
  filename = function() {paste("AME-Run_",  Sys.Date(), ".tsv", sep = "")},
  content = function(file){
    vroom::vroom_write(ame_results, file)
    
  }
)
