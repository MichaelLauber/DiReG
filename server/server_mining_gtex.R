ensg_to_hgnc <- readRDS("data/ensg_to_hgnc.rds")

sample.df <- read.delim("data/GTEx_Analysis_v8_Annotations_SampleAttributesDS.txt", as.is=TRUE, header=TRUE, row.names=1)
rnaseq.sample.df <- sample.df[sample.df['SMAFRZE']=='RNASEQ', ]
mapping <- rnaseq.sample.df %>%
  select(SMTS, SMTSD)
mapping$id <- rownames(rnaseq.sample.df)

library(slickR)
# Create content for the carousel

previousGTEXInputTFs <- reactiveVal(NULL)

observeEvent(input$btnGTEx, ({
  
  if(!networkCreated){
    shinyalert::shinyalert("There are no Genes to Analyse",
                           "Please input genes and press the RUN button before performing any analysis",
                           type = "error")
    return()}
  
  
  currentTFs <- inputTFs()
  
  if (identical(currentTFs, previousGTEXInputTFs())) {
    
    return()
  } 
  
  symbols <- c(inputTFs())
  
  nrFigs <- length(symbols)
  
  #print(symbols)
  
  screens <- lapply(1:nrFigs, function(i){
    
    print(symbols[i])
    
    gene_name <- ensg_to_hgnc %>%
      filter(HGNC == symbols[i]) %>%
      select(ENSG)
    
    filename <- paste0(gene_name, ".rds")
    
    print(filename)
    
    gene <- readRDS(file.path("~/gtex_splitted",filename))
    
    df <- data.frame(id=names(gene), values= gene)
    df$tissue <- mapping$SMTSD[match(df$id, mapping$id)]
    #df$tissue2 <- mapping$SMTSD[match(df$id, mapping$id)]
    #df$tissue3  <- stringr::str_split_fixed(df$tissue2, "-", 2)[,1]
    
    shinyglide::screen(
      h3(symbols[i], align="center"),
      renderPlotly({
        
        
        df %>% plotly::plot_ly(
          y = ~values,
          #y = ~logval,
          x = ~tissue,
          type= 'violin',
          split = ~tissue,
          #split = ~tissue3,
          box = list(
            visible = T
          ),
          meanline = list(
            visible = T
          ),
          spanmode = "hard",
          marker = list(
            size = 3
          )
        ) %>%
          layout(
            xaxis = list(
              title = paste0(i,"/",nrFigs),
              tickangle = 45,
              spanmode = "hard"
            ),
            yaxis = list(
              title = "TPM",
              zeroline = F
            ),
            showlegend = FALSE
          )
        
      })
    )
  })
  
  
  output$carousel <- renderUI({
    
    do.call(glide, screens)
    
    
  })
  previousGTEXInputTFs(currentTFs)
})
)