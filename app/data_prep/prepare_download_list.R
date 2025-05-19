library(httr)
library(xml2)
library(jsonlite)
library(dplyr)

#####
# Get PUBMED IDs based on query
####
query_url <- "https://eutils.ncbi.nlm.nih.gov/entrez/eutils/esearch.fcgi?db=pubmed&term=%28%22direct%20reprogramming%22%5BTitle%2FAbstract%5D%20OR%20%22direct%20conversion%22%5BTitle%2FAbstract%5D%29%20NOT%20%28Review%5BPublication%20Type%5D%20OR%20Meta-Analysis%5BPublication%20Type%5D%20OR%20Systematic%20Review%5BPublication%20Type%5D%20OR%20Editorial%5BPublication%20Type%5D%20OR%20Comment%5BPublication%20Type%5D%20OR%20Letter%5BPublication%20Type%5D%29%20AND%20%22free%20full%20text%22%5BFilter%5D&retmax=2000&retmode=json"

response <- httr::GET(query_url)
content <- httr::content(response, as = "parsed", type = "application/json")
pmid_list <- content$esearchresult$idlist

pmids <- unlist(pmid_list)

######
# Load and combine mapping files
######
oa_comm_use_files <- read.csv("oa_comm_use_file_list.txt", sep='\t', header = F)
oa_comm_use_files <- oa_comm_use_files[c(2:nrow(oa_comm_use_files)),]
oa_comm_use_files$PMID <- gsub("PMID:", "", oa_comm_use_files$V4)

oa_files <- read.csv("oa_file_list.txt", sep='\t', header = F)
oa_files <- oa_files[c(2:nrow(oa_files)),]
oa_files$PMID <- gsub("PMID:", "", oa_files$V4)

combined_mapping_file <- rbind(oa_comm_use_files, oa_files)
unique_mapping_file <- combined_mapping_file %>% distinct(PMID, .keep_all = TRUE)

###
# Fetch title for eahc pubmed id
###
batch_size <- 50  # Number of PMIDs per request
pmid_batches <- split(pmids, ceiling(seq_along(pmids)/batch_size))

fetch_titles_batch <- function(pmid_batch) {
  Sys.sleep(0.4) 
  efetch_url <- paste0("https://eutils.ncbi.nlm.nih.gov/entrez/eutils/efetch.fcgi?db=pubmed&id=", paste(pmid_batch, collapse = ","), "&retmode=xml")
  response <- httr::GET(efetch_url)
  content <- httr::content(response, as = "text", encoding = "UTF-8")
  xml <- xml2::read_xml(content)
  titles <- xml2::xml_text(xml2::xml_find_all(xml, ".//ArticleTitle"))
  return(titles)
}


# Apply the function to each batch
titles_list <- lapply(pmid_batches, fetch_titles_batch)
titles <- unlist(titles_list)  # Flatten list
results_df <- data.frame(PMID = pmids, Title = titles, stringsAsFactors = FALSE)


###
# Use chat gpt to filter out papers from other domains
###

api_key <- "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"  

classify_paper <- function(title) {

  prompt <- paste0(
    "Does the following paper title indicate that it speaks about 'direct reprogramming' or 'direct conversion' in the context of cell biology and cell type conversions, or does it belong to another field like chemistry or physics? Please answer 'Cell_Biology' or 'Other_Field'.\n\nTitle: ", 
    title
  )
  
  response <- httr::POST(
    url = "https://api.openai.com/v1/chat/completions",
    httr::add_headers(
      `Authorization` = paste("Bearer", api_key),
      `Content-Type` = "application/json"
    ),
    body = toJSON(list(
      model = "gpt-4o-mini",  # Use GPT-4 or another available model
      messages = list(
        list(role = "system", content = "You are an expert in scientific literature classification."),
        list(role = "user", content = prompt)
      ),
      max_tokens = 20  # Keep response short
    ), auto_unbox = TRUE)
  )
  
  result <- content(response, as = "parsed", type = "application/json")
  classification <- result$choices[[1]]$message$content
  return(classification)
}


results_df$Classification <- sapply(results_df$Title, classify_paper)
write.csv(results_df, "filtered_pubmedids.csv")
results_df <- read.csv("filtered_pubmedids.csv")

table(results_df$Classification) 

repro_results_df <- results_df[results_df$Classification == "Cell_Biology",]
repro_results_df$PMID <- as.character(repro_results_df$PMID)

###
# add pmc id and download location
###
final_df <- repro_results_df %>%
  left_join(unique_mapping_file, by = "PMID")

###
# divide into two datframe
# missing df are new papers that are not pmc indexed yet
###
missing_df <- final_df[is.na(final_df$V1), ]
complete_df <- final_df[!is.na(final_df$V1), ]
complete_df$PMC <- stringr::str_remove_all(complete_df$V3, "PMC")


ftp_url <- "https://ftp.ncbi.nlm.nih.gov/pub/pmc/"
complete_df$download_query <- paste0(ftp_url, complete_df$V1)

write.csv(complete_df, "free_papers_w_pmc.csv")

# convert_pmid_to_pmc <- function(pmids) {
#   Sys.sleep(0.1) 
#   base_url <- "https://www.ncbi.nlm.nih.gov/pmc/utils/idconv/v1.0/"
#   
#   # Construct API request with comma-separated PMIDs
#   query_url <- paste0(base_url, "?tool=my_tool&email=myemail@example.com&ids=", paste(pmids, collapse = ","))
#   
#   # Send GET request
#   response <- httr::GET(query_url)
#   content <- httr::content(response, as = "text", encoding = "UTF-8")
#   xml_data <- read_xml(content)
#   
#   # Extract records
#   records <- xml_find_all(xml_data, "//record")
#   
#   # Initialize an empty list to store results
#   results <- list()
#   
#   for (rec in records) {
#     pmid <- xml_attr(rec, "pmid")
#     pmcid <- xml_attr(rec, "pmc")
#     
#     # Handle missing PMC IDs (if status="error")
#     if (is.na(pmcid) || pmcid == "") {
#       pmcid <- "No PMC ID Available"
#     }
#     
#     # Store results
#     results <- append(results, list(data.frame(PMID = pmid, PMC = pmcid, stringsAsFactors = FALSE)))
#   }
#   
#   # Combine into a dataframe
#   final_df <- do.call(rbind, results)
#   
#   return(final_df)
# }



# get_pmc_pdf_url <- function(pmcid) {
#   pmcid
#   base_url <- paste0("https://www.ebi.ac.uk/europepmc/webservices/rest/", pmcid, "/fullTextPDF")
#   response <- httr::GET(base_url)
#   
#   if (response$status_code == 200) {
#     pdf_url <- content(response, as = "text")
#     return(pdf_url)
#   } else {
#     return(NA)
#   }
# }

# pmid_to_pmc_list <- lapply(missing_df$PMID, function(x) convert_pmid_to_pmc(x))
# pmc_results <- do.call(rbind, pmid_to_pmc_list)
# missing_df$PMC <- pmc_results$PMC
# missing_wpmc_df <- missing_df  %>% filter(PMC != "No PMC ID Available") 


#pdf_links <- sapply(complete_df$PMC[1:3], get_pmc_pdf_url)
#download.file(pdf_links[1], destfile = "paper1.pdf", mode = "wb")

# 
# find_doi <- function(pmid) {
#   url <- paste0("https://api.crossref.org/works?query=", pmid, "&rows=1")
#   
#   response <- httr::GET(url)
#   result <- content(response, as = "parsed", type = "application/json")
#   
#   if (length(result$message$items) > 0) {
#     doi <- result$message$items[[1]]$DOI
#     return(paste0("https://doi.org/", doi))
#   } else {
#     return(NA)
#   }
# }
# 
# # Find DOIs for all PMIDs
# doi_links <- sapply(missing_df$PMID[1:5], find_doi)


