#### UI ####
appUI <- function(defaults) {
  fluidPage(
    title = "M\u00b2ara",
    lang = "en",
    # selection state of the feature comparison: lives in the browser, keyed by
    # mzIdx, so paging/filtering does not lose it and the table is never
    # re-rendered by the server when a checkbox is clicked
    tags$script(HTML(paste0(
      "window.m2araSelected = window.m2araSelected || [];
       window.m2araColours = ['", paste(featureColours(4), collapse = "', '"), "'];

       window.m2araUpdateCheckboxes = function() {
         var sel = window.m2araSelected || [];
         var full = sel.length >= 4;
         jQuery('#mzTable input.m2ara-check').each(function() {
           var idx = parseInt(this.value, 10);
           var checked = sel.indexOf(idx) >= 0;
           this.checked = checked;
           this.disabled = (!checked && full);
         });
         jQuery('#mzTable tbody tr').each(function() {
           var boxes = jQuery(this).find('input.m2ara-check');
           if (boxes.length === 0) { return; }
           var pos = sel.indexOf(parseInt(boxes.val(), 10));
           if (pos >= 0) {
             var hex = window.m2araColours[pos];
             var r = parseInt(hex.substr(1, 2), 16);
             var g = parseInt(hex.substr(3, 2), 16);
             var b = parseInt(hex.substr(5, 2), 16);
             this.style.backgroundColor = 'rgba(' + r + ',' + g + ',' + b + ',0.18)';
             this.style.borderLeft = '4px solid ' + hex;
           } else {
             this.style.backgroundColor = '';
             this.style.borderLeft = '';
           }
         });
       };

       jQuery(document).on('change', '#mzTable input.m2ara-check', function() {
         var idx = parseInt(this.value, 10);
         var sel = window.m2araSelected || [];
         if (this.checked) {
           if (sel.length >= 4) { this.checked = false; return; }
           if (sel.indexOf(idx) < 0) { sel.push(idx); }
         } else {
           sel = sel.filter(function(v) { return v !== idx; });
         }
         window.m2araSelected = sel;
         window.m2araUpdateCheckboxes();
         Shiny.setInputValue('mzTable_selected', sel.slice(), {priority: 'event'});
       });

       Shiny.addCustomMessageHandler('m2ara_reset_selection', function(msg) {
         window.m2araSelected = [];
         window.m2araUpdateCheckboxes();
         Shiny.setInputValue('mzTable_selected', [], {priority: 'event'});
       });"
    ))),
    # shiny sidebar layout
    sidebarLayout(
      #### Sidebar ####
      sidebarPanel = appSidebar(defaults),

      #### Main panel ####
      mainPanel = appMainPanel(defaults)
    )
  )
}
