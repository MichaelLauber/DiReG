div(
  h2("Discovery"),
  p("The majority of the existing computational methods for the prediction of TF sets for transdifferentiation 
    rely on pretrained models based on huge repositories sequencing data. 
    For researches who have generated their owm data set which the want to user to
    leverage TF prediction, these methods are very ofthen unsuitable. Either the 
    wet lab lack the skill set the retrain the model, the model's source code is not published for example like Mogrify,
    the own data might not be easily incorporated due to batch effect or the 
    scientist simply lacks the capcaity to perform such a time consuming task.
    "),
  p("While their primary objectives differ, techniques designed for motif discovery and differential 
    expression analysis can also serve the purpose of ranking transcription factors or transcription 
    factor motifs as potential candidates for reprogramming protocols.
    When employing these approaches, careful consideration should be given to selecting parameters 
    like the type of accessibility or histone mark genomic data, the number and selection of genomic regions 
    from target cells, and the choice of background sequences, whether shuffled 
    or derived from natural genomic sequences. These choices are crucial for producing high-quality results.
    "),
  p("Hammelman et al. conducted a systematic comparison of nine computational 
    techniques to determine the most reliable approaches for identifying potential 
    reprogramming factors using gene expression and chromatin accessibility data. 
    They conducted a comprehensive and consistent assessment of nine methods: 
    CellNet, GarNet, EBseq, AME, DREME, HOMER, KMAC, DeepAccess, and diffTF."),
  p("In summary, their analysis showed that techniques leveraging chromatin accessibility 
    outperform gene expression-based methods in the context of reprogramming factor discovery. 
    They pinpointed the most effective strategies for selecting accessible 
    regions in sequence-based methods. Employing these optimized strategies, 
    the authors determined that AME and diffTF exhibit the most consistent 
    and robust performance in recovering transcription factors."),
  img(src="discovery.png"),
  br(),
  p("We provide users with the capability to upload ATAC-seq data from both a 
    starting and target cell, allowing them to select the relevant reference genome 
    and a TFBS database of their preference. Additionally, users have the flexibility 
    to choose between two different background generation methods."),
  img(src="ame.png"),
)