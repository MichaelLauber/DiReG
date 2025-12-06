getRanks <- function(inputTFs, network){
  
  nrCoveredGenes <- 0
  coveredThisRound <- inputTFs
  counter <- 1
  genesPerIteration <- list()
  
  # Initialize Level 1 (Input TFs)
  # NOTE: Assuming input TFs are "Activated" (1). 
  genesPerIteration[[counter]] <- data.frame(gene = inputTFs,
                                             mor = 1,
                                             level = counter)
  
  # Define the Universe (All genes in the network)
  allGenes <- unique(c(network$from, network$to))
  
  # Propagation Loop
  while(counter <= 5 ){
    counter <- counter + 1
    coveredLastRound <- coveredThisRound
    
    # Find targets of the previous round
    newSelection <- network$from %in% coveredLastRound
    if(sum(newSelection) == 0) break
    
    sub_df <- network[newSelection,]
    
    # Store targets
    df <- data.frame(gene = sub_df$to,
                     mor = sub_df$mor, 
                     level = counter)
    
    genesPerIteration[[counter]] <- df
    
    # Prepare for next round (find TFs among the targets)
    coveredThisRound <- unique(network$from[network$from %in% sub_df$to])
  }
  
  allRes <- do.call("rbind", genesPerIteration)
  weightFactor <- 1
  
  allRes$weigth <- allRes$mor * (1 / (allRes$level * weightFactor)) 
  
  # --- Aggregate Scores ---
  # If a gene is activated (+0.5) and repressed (-0.33) by different paths, 
  # the sum (0.17) reflects the net effect.
  combRes <- tapply(allRes$weigth, allRes$gene, sum)
  
  # Identify genes in the network universe that were not reached by propagation
  uncoveredGenes <- setdiff(allGenes, names(combRes))
  
  # Assign them 0 (neutral)
  missing <- setNames(rep(0, length(uncoveredGenes)), uncoveredGenes)
  
  # Combine
  final_ranks <- c(combRes, missing)
  
  # BREAK TIES (Jitter) ---
  # Add tiny random noise to allow fgsea to sort uniquely
  set.seed(42) # Set seed for reproducibility
  final_ranks <- final_ranks + runif(length(final_ranks), -1e-5, 1e-5)
  
  # Sort descending (Positive -> Zero -> Negative)
  final_ranks <- sort(final_ranks, decreasing = TRUE)
  
  return(final_ranks)
}