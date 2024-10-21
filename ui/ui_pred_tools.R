# tabPanel(
#   "Prediction Tools",
#   h1(align = "center", "Find and compare inferred TF sets"),
#   p(align = "center", "The collection contains:",  br(),
#     "230 predictions from", a("Mogrify,", href="https://www.nature.com/articles/ng.3487"), br(),
#     "225 from",  a("Alessio et al", href="https://www.ncbi.nlm.nih.gov/pmc/articles/PMC4649293/"), "(Here refred as JSD)", br(),
#     "25 from", a("Cellnet", href="https://www.ncbi.nlm.nih.gov/pmc/articles/PMC4291075/"), br(),
#     "25k from", a("Taiji", href="https://academic.oup.com/nargab/article/3/4/lqab100/6423166"), "(Arrow indicates up/down regualtion)"),
#   
#   wellPanel(id = "large",
#             
#             fluidRow(
#               column(3,
#             div(
#                 startcellSelection("selectStart_infered", "Start Cell", startInputList_inf),
#                 targetcellSelection('selectTarget_infered', 'Target Cell')
#             ),
#               ),
#             column(9,
#             div(class = "center-flex",
#                 tableOutput("tbl_inferred_protocols")
#             )
#             ),
#             ),
#             div(class = "center",
#                 checkboxGroupInput("checkGroupTools",
#                                    h5("Tools"),
#                                    inline = TRUE,
#                                    choices = list("Mogrify" = "Mogrify",
#                                                   "JSD"= "JSD",
#                                                   "Taiji" = "Taiji",
#                                                   "CellNet" = "CellNet",
#                                                   "IRENE" = "irene"
#                                    ),
#                                    selected = c("Mogrify"))
#             )
#   )
#   
# )