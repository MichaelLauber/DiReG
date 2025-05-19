var timeout;
shinyjs.idleTimer = function(time) {
  clearTimeout(timeout); 
  timeout = setTimeout(function(){Shiny.onInputChange('idle', true);}, time*1000);
  $(document).on('mousemove keypress', function(e) {
    clearTimeout(timeout);  
    timeout = setTimeout(function(){Shiny.onInputChange('idle', true);}, time*1000);
  });
};
shinyjs.resetIdleTimer = function() {
  clearTimeout(timeout);
};