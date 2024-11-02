all_infered_protocols <- read.csv(file.path("data","all_inferred_protocols.csv"))

reprogramming_protocols <-
  read.csv(file.path("data","reprogramming_protocols.csv"))

print("Networke created set to False")
#networkCreated <- FALSE 

print("Sourcing helper functions...")
source("utils/helper_functions.R")