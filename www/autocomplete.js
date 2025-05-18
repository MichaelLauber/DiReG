$(document).ready(function() {
  // Initialize autocomplete with empty array - will be populated dynamically
  var availableTFs = [];
  
  // Function to update autocomplete source
  function updateAutocompleteSource() {
    // Use Shiny's JavaScript API to get data from R
    Shiny.addCustomMessageHandler("updateTFsSource", function(data) {
      availableTFs = data;
      initAutocomplete();
    });
  }
  
  // Extract the last term being typed
  function extractLastTerm(term) {
    return term.split(/[\s,;]+/).pop();
  }
  
  // Function to initialize or reinitialize the autocomplete
  function initAutocomplete() {
    // Destroy existing autocomplete if it exists
    if ($('#inputTextTFs').autocomplete("instance")) {
      $('#inputTextTFs').autocomplete("destroy");
    }
    
    // Initialize autocomplete
    $('#inputTextTFs').autocomplete({
      delay: 100,
      minLength: 1,
      appendTo: "body",
      position: { 
        my: "left top", 
        at: "left bottom",
        of: "#inputTextTFs",
        collision: "flip"
      },
      source: function(request, response) {
        // Extract the current term being typed
        var term = extractLastTerm(request.term);
        
        // Filter available TFs based on the current term
        response($.ui.autocomplete.filter(availableTFs, term));
      },
      focus: function() {
        // Prevent default focus behavior to avoid replacing entire input
        return false;
      },
      open: function(event, ui) {
        // Apply CSS to ensure menu appears on top
        $('.ui-autocomplete').css('z-index', '9999');
      },
      select: function(event, ui) {
        var terms = this.value.split(/[\s,;]+/);
        // Remove the last term (which is being completed)
        terms.pop();
        // Add the selected item
        terms.push(ui.item.value);
        // Add a space to prepare for the next term
        this.value = terms.join(" ") + " ";
        return false;
      }
    }).on('keydown', function(event) {
      // Reset autocomplete on space, comma, semicolon to prepare for next term
      if (event.keyCode === $.ui.keyCode.SPACE || 
          event.keyCode === 188 || // comma
          event.keyCode === 186) { // semicolon
        $(this).autocomplete("close");
      }
    });
  }
  
  // Call function to set up the message handler
  updateAutocompleteSource();
  
  // Initialize with empty source (will be updated later)
  initAutocomplete();
  
  // Handle tab changes
  $(document).on('shown.bs.tab', 'a[data-toggle="tab"]', function(e) {
    // Reset autocomplete
    $('#inputTextTFs').autocomplete("close");
    // Remove any stray UI elements
    $(".ui-autocomplete").remove();
    $(".ui-helper-hidden-accessible").not(":has(#inputTextTFs)").remove();
    $("div[role='status']").remove();
  });
  
  // Additional cleanup for any stray elements when clicking elsewhere
  $(document).on('click', function(event) {
    if (!$(event.target).closest('#inputTextTFs, .ui-autocomplete').length) {
      $('#inputTextTFs').autocomplete("close");
    }
  });
  
  // Add CSS to ensure proper display
  $("<style>")
    .prop("type", "text/css")
    .html(`
      .ui-autocomplete {
        z-index: 9999 !important;
        position: absolute !important;
        max-height: 200px;
        overflow-y: auto;
      }
      .ui-helper-hidden-accessible {
        position: absolute !important;
        clip: rect(1px 1px 1px 1px);
        clip: rect(1px, 1px, 1px, 1px);
        padding: 0 !important;
        border: 0 !important;
        height: 1px !important;
        width: 1px !important;
        overflow: hidden;
      }
    `)
    .appendTo("head");
});