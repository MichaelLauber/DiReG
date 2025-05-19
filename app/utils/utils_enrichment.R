getRanks <- function(inputTFs, network){
  
  nrCoveredGenes <- 0
  coveredThisRound <- inputTFs
  counter <- 1
  genesPerIteration <- list()
  
  genesPerIteration[[counter]] <- data.frame(gene=inputTFs,
                                             mor = 1,
                                             level=counter)

  allGenes <- unique(c(network$from,network$to))
  
  while(counter <=5 ){
    
    counter <- counter+1
    
    coveredLastRound <- coveredThisRound
    newSelection <- network$from %in% coveredLastRound
    
    if(sum(newSelection) == 0) break
    
    sub_df <- network[newSelection,]

    df <- data.frame(gene=sub_df$to,
                     mor = sub_df$mor,
                     level=counter)

    genesPerIteration[[counter]] <- df
    
    #selects target nodes which are TFs (=occurence in from coloumn)
    coveredThisRound <- unique(network$from[network$from %in% sub_df$to])
    
    
  }
  
  allRes <- do.call("rbind", genesPerIteration)
  weightFactor <- 1
  allRes$weigth <- abs(allRes$mor)*(1/(allRes$level*weightFactor)) 

  combRes <- tapply(allRes$weigth, allRes$gene, sum)
  combRes <- sort(combRes, decreasing = T)
  
  uncoveredGenes <- allGenes[!allGenes %in% allRes$gene]
  nrUncovered <- uncoveredGenes %>%  unique() %>% length()
  missing <- rep(0, nrUncovered)
  names(missing) <- uncoveredGenes
  res <- c(combRes, missing)
  res
}
