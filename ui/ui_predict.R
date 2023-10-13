source("utils/utils_selectizeInput.R")


all_infered_protocols <- read.csv(file.path("data","all_inferred_protocols.csv"))
startInputList_inf <- createChoices(all_infered_protocols$Start)

navbarMenu("Discovery",
           source("ui/ui_pred_ame.R")$value#,
           #source("ui/ui_pred_tools.R")$value
)
