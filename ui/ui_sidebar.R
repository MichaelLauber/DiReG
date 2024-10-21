div(
  id = "sidebar-container",
  shinyWidgets::actionBttn(
    "btnORA",
    label = "OR Analysis",
    style = "simple",
    color = "primary",
    size = "md",
    class = "action-btn-custom" 
  ),
  shinyWidgets::actionBttn(
    "btnGSEA",
    label = "GSEA",
    style = "simple",
    color = "primary",
    size = "md",
    class = "action-btn-custom" 
  ),
  shinyWidgets::actionBttn(
    "btnGTEx",
    label = "Tissue Expression",
    style = "simple",
    color = "primary",
    size = "md",
    class = "action-btn-custom" 
  ),
  shinyWidgets::actionBttn(
    "btnIsoforms",
    label = "Isoform Potential",
    style = "simple",
    color = "primary",
    size = "md",
    class = "action-btn-custom" 
  ),
  shinyWidgets::actionBttn(
    "btnTFA",
    label = "TFA Analysis",
    style = "simple",
    color = "primary",
    size = "md",
    class = "action-btn-custom" 
  )
    
)