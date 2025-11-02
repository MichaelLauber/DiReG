# Load required libraries
library(OmnipathR)
library(dplyr)
library(stringr)

# Function to process CollecTRI data for a given organism
process_collectri <- function(organism = "human") {
  
  # Fetch CollecTRI data
  message(paste("Fetching CollecTRI data for", organism))
  collectri <- OmnipathR::collectri(
    organism = organism,
    genesymbols = TRUE,
    add_counts = TRUE
  )
  
  # Define columns to keep
  cols <- c("source_genesymbol", "target_genesymbol", "is_stimulation", 
            "is_inhibition", "n_references", "n_resources")
  
  # Separate non-complex and complex interactions
  collectri_interactions <- collectri[!str_detect(collectri$source, "COMPLEX"), cols]
  collectri_complex <- collectri[str_detect(collectri$source, "COMPLEX"), cols]
  
  # Process complex interactions - simplify complex names
  collectri_complex <- collectri_complex %>% 
    mutate(source_genesymbol = case_when(
      str_detect(source_genesymbol, "JUN") | str_detect(source_genesymbol, "FOS") ~ "AP1", 
      str_detect(source_genesymbol, "REL") | str_detect(source_genesymbol, "NFKB") ~ "NFKB",
      TRUE ~ source_genesymbol  # Keep original if no match
    ))
  
  # Combine and process all interactions
  collectri_processed <- bind_rows(collectri_interactions, collectri_complex) %>% 
    distinct(source_genesymbol, target_genesymbol, .keep_all = TRUE) %>% 
    mutate(
      
      # Assign confidence levels based on resources and references
      confidence = case_when(
        n_resources >= 3 & n_references >= 3 ~ "A",
        n_resources >= 3 | n_references >= 3 ~ "B",
        n_resources >= 2 | n_references >= 2 ~ "C",
        TRUE ~ "D"
      )
    ) %>% 
    rename(source = source_genesymbol, 
           target = target_genesymbol)
  
  collectri_processed <- collectri_processed %>%
    mutate(
      mor_value = case_when(
        confidence == "D" ~ 0.25,
        confidence == "C" ~ 0.33,
        confidence == "B" ~ 0.5,
        confidence == "A" ~ 1,
        TRUE ~ NA_real_  # for any other values
      ),
      # Apply sign based on is_stimulation
      mor = ifelse(is_stimulation == 1, mor_value, -mor_value)
    )
  
  # Report on ambiguous interactions
  ambiguous <- collectri_processed %>% 
    filter(is_stimulation == is_inhibition)
  message(paste("Found", nrow(ambiguous), 
                "ambiguous interactions (treated as activating)"))
  
  # Display confidence distribution
  message("\nConfidence level distribution:")
  print(table(collectri_processed$confidence))
  
  return(collectri_processed)
}

# Process data for both organisms
collectri_human <- process_collectri("human")
collectri_mouse <- process_collectri("mouse")

# Save results
write.table(collectri_human, 
            file = "ct_hs.tsv", 
            sep = "\t", 
            row.names = FALSE, 
            quote = FALSE)

write.table(collectri_mouse, 
            file = "ct_mm.tsv", 
            sep = "\t", 
            row.names = FALSE, 
            quote = FALSE)

message("\nProcessing complete! Files saved:")
message("  - ct_hs.tsv (human)")
message("  - ct_mm.tsv (mouse)")