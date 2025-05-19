Shiny.addCustomMessageHandler("updateFileInputHandler", function(x) {

        var id =  x + "_progress";
        document.getElementById(id).style.visibility = "visible"
        document.getElementById(id).children[0].innerHTML = "Example Data loaded"
        document.getElementById(id).children[0].style.width = "100%"
        document.getElementById(id).children[0].style.background = "orange"
        document.getElementById(id).children[0].style.color = "black"
});
