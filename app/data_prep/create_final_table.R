all_extracted_files <- list.files("all_free_pdfs/pdf_files/")

PMC_ids <- stringr::str_split_fixed(all_extracted_files, "_", n=2)[,1]

complete_df <- read.csv( "free_papers_w_pmc.csv", row.names = NULL)

is_in_pdfs <- lapply(complete_df$V3, function(x) {x %in% PMC_ids})
complete_df$has_pdf <-  (is_in_pdfs %>% unlist())

final_extracted_pdfs_df <- complete_df[complete_df$has_pdf,]

final_extracted_pdfs_df$X.1 <- NULL
final_extracted_pdfs_df$X <- NULL
final_extracted_pdfs_df$Classification <- NULL
final_extracted_pdfs_df$PMC <- NULL
final_extracted_pdfs_df$has_pdf <- NULL
final_extracted_pdfs_df$V5 <- NULL
final_extracted_pdfs_df$V1 <- NULL
final_extracted_pdfs_df$V4 <- NULL
final_extracted_pdfs_df$download_query <- NULL
colnames(final_extracted_pdfs_df) <- c("PMID",  "Title", "Journal",    "PMC_ID" )

write.csv(final_extracted_pdfs_df, "final_extracted_pdfs.csv", row.names = F)
