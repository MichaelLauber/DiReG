gtexTPM <-  vroom::vroom(file.path("data", "GTEx_Analysis_2017-06-05_v8_RNASeQCv1.1.9_gene_median_tpm.gct.gz"), delim = "\t", skip = 2)

gtexTPM <- gtexTPM %>% 
  rename(genes = Description )

normalize_minmax <- function(x) {
  maxdif <- (max(x, na.rm = TRUE) - min(x, na.rm = TRUE))
  if( maxdif == 0) return(x)
  (x - min(x, na.rm = TRUE)) / (max(x, na.rm = TRUE) - min(x, na.rm = TRUE))
}
gtexTPMnorm <-t(apply(gtexTPM[,-c(1:2)], 1,  normalize_minmax))
gtexTPMnorm <-as.data.frame(gtexTPMnorm)
gtexTPMnorm$genes <- gtexTPM[,2, drop=T]

gtexTPMscaled <-t(apply(gtexTPM[,-c(1:2)], 1,  scale))
gtexTPMscaled <-as.data.frame(gtexTPMscaled)
colnames(gtexTPMscaled) <- colnames(gtexTPM[,-c(1:2)])
gtexTPMscaled$genes <- gtexTPM[,2, drop=T]

saveRDS(gtexTPM,"data/gtexTPM.rds")
saveRDS(gtexTPMnorm,"data/gtexTPMnorm.rds")
saveRDS(gtexTPMscaled,"data/gtexTPMscaled.rds")


sample.df <- read.delim("data/GTEx_Analysis_v8_Annotations_SampleAttributesDS.txt", as.is=TRUE, header=TRUE, row.names=1)
rnaseq.sample.df <- sample.df[sample.df['SMAFRZE']=='RNASEQ', ]
mapping <- rnaseq.sample.df %>%
  select(SMTS, SMTSD)
mapping$id <- rownames(rnaseq.sample.df)
saveRDS(mapping, "data/gtex_mapping.rds")