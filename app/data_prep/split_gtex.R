print("loading data")
gtex <- readRDS("/nfs/home/users/michaell2/R-Packages/DiReG/data/gtex_gene_tpm.rds")


gtex_mat <- as.matrix(gtex[,-1])  
gene_names_hgnc <- gtex[,1]
gene_names_ensmbl <- rownames(gtex)


print("start splitting")
for(i in seq_len(dim(gtex_mat)[1])){
  print(i)
  file_name <- paste0(gene_names_ensmbl[i], ".rds")
  saveRDS(gtex_mat[i,], file.path("/nfs/home/users/michaell2/R-Packages/DiReG/gtex_splitted", file_name))
}