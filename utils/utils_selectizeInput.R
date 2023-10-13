
## creates select widget for targetcell
#actually redundant could be replaced by startcellSelection
# primarily used for debugging
targetcellSelection <- function(id, name){
  selectizeInput(
    id, h3(name), choices = list(),
    options = list(
      placeholder = 'Please select an option',
      onInitialize = I('function() { this.setValue(""); }')
    ),
    width = "230px"
  )
}


## create choices for selectinput widget
createChoices <- function(data){
  choices <- unique(data)
  choicesList <- as.list(choices)
  names(choicesList) <- choices
  choicesList
}


##creates the choices for the targetcell
getTargetCells <- function(startcell, dataFrame){
  targetcells <- unique(dataFrame[dataFrame$Start == startcell, ]$Target)
  createChoices(targetcells)
}


## creates select widget for startcell
startcellSelection <- function(id, name, selection){
  selectizeInput(id, h3(name),
                 choices = selection,
                 selected="Fibroblast",
                 width = "230px")
}