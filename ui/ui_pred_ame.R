tabPanel(
  "Discovery",
  h1(class = "center", "Make a prediction with AME"),
  div(
    p("All directed prediction tools up to date are trained on a large dataset and
                               the models are limited to the cell types in the training data.", a("Hammelman et al.", href="https://www.nature.com/articles/s41592-022-01522-2#Abs1") ,"evaluated different approaches
                               which are not dependend on a pretrained models and allow to rank transcription factors for their potential for directed differentiaion.
                               Here we allow the user to easily use the best performing approach based on AME from the", a("MEME suite.", href="https://meme-suite.org/meme/")),
    
    p("The user needs only
                               to upload chromatin data of the start and target cell type of the desired directed differentiation.
                               Then the following steps are performed in the background.",
      
      tags$ul(
        tags$li("If a ", a("narrowPeak", href="https://genome.ucsc.edu/FAQ/FAQformat.html#format12") ,"file is provided (which is recommend) regions will be filterd based on a q-value threshold of 0.05.
                                 Same goes for a broadPeak file.  Bed and tsv files are also supported, but filtering will be skipped) "),
        
        tags$li("Regions from the target cell which do
                                 not overlap any regions of the start cell will be selected"),
        
        tags$li("If a narrowpeak file was provided the top 10 % of regions with the lowest pvalue will be selected"),
        
        tags$li("Fasta files of this regions will be generated. It is important to choose the appropriate reference genome because
                                 if the prefix of the chromosome in the input file do not match the ref genome, this procedure will fail"),
        
        tags$li("GC match background sequences will be generated which migth take a couple of minutes.
                                         This step might be skipped to fasten up the procedure but comes with loss of accuracy"),
        
        tags$li("Lastly, AME will find differentially accessible between the input and background sequences.
                                 Here the user can choose transcription factors motifs either from JASPAR or Hocomoco database"),
      )
    )
  ),
  wellPanel(
            p(
              "Upload chromatin data from DNA-, Ataq-seq or other chromatin assays. Max size per file is 10 MB", align="center"
            ),
            
            
            div(
              id = "container-input-ame",
              fileInput("fileAmeStart", h3("Select Chromatin Data Start Cell")),
              fileInput("fileAmeTarget", h3("Select Chromatin Data Target Cell"))
            ),
            div(id="container-ame-radios",
                div(radioButtons("radioRefG", h3("Reference Genome"), inline = TRUE,
                                 choices = list(
                                   "mm10" = "mm10",
                                   "mm39" ="mm39",
                                   "hg19" = "hg19",
                                   "hg38" = "hg38"
                                   
                                 ), selected = "mm10"),
                    radioButtons("radioRefG", "", inline = TRUE,
                                 choices = list(
                                   "GRCm38" = "GRCm38",
                                   "GRCm39"= "GRCm39",
                                   "GRCh37" = "GRCh37",
                                   "GRCh38" = "GRCh37"
                                 ), selected = "mm10"),
                ),
                
                radioButtons("radioTFBSDB", h3("TFBS Database"), inline = FALSE,
                             choices = list("Hocomoco Mouse v11" = "hocoMouse", "Hocomoco Human v11" = "hocoHuman","JASPAR Vertebrate ’22 " = "jaspar"),selected = "hocoMouse"),
                radioButtons("radioBg", h3("Background"), inline = FALSE,
                             choices = list("GC Matched (more accurate)" = "gcmatched", "shuffled (faster)" = "shuffled"),selected = "shuffled")
            ),
            div(class="center-flex",
                actionButton("btnRunAME", "Run Prediction with AME", icon = icon("cocktail")),
                actionButton("loadExamplAMEbtn", "Load example Data")
            )
  ),
  
  div(id="container-ame-bottom",
      tableOutput("ame-res"),
      downloadButton("download-ame")
  ),
)