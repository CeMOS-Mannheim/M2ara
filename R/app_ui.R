#### UI ####
appUI <- function(defaults) {
  fluidPage(
    title = "M\u00b2ara",
    lang = "en",
    # colour mapping for the compared features, filled by the server
    tags$head(tags$script(HTML(
      "window.m2araRowColours = window.m2araRowColours || {};
       if (window.Shiny) {
         Shiny.addCustomMessageHandler('m2ara_row_colours', function(msg) {
           window.m2araRowColours = msg || {};
           var table = jQuery('#mzTable').DataTable();
           if (table) { table.rows().invalidate(); }
         });
       }"
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
