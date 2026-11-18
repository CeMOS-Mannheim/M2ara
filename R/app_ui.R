#### UI ####
appUI <- function(defaults) {
  fluidPage(
    title = "M\u00b2ara",
    lang = "en",
    # colour mapping for the compared features, filled by the server; the table
    # is only redrawn when the DT widget is initialised already
    tags$script(HTML(
      "window.m2araRowColours = window.m2araRowColours || {};
       Shiny.addCustomMessageHandler('m2ara_row_colours', function(msg) {
         window.m2araRowColours = msg || {};
         if (jQuery.fn.dataTable && jQuery.fn.dataTable.isDataTable('#mzTable')) {
           jQuery('#mzTable').DataTable().rows().invalidate();
         }
       });"
    )),
    # shiny sidebar layout
    sidebarLayout(
      #### Sidebar ####
      sidebarPanel = appSidebar(defaults),

      #### Main panel ####
      mainPanel = appMainPanel(defaults)
    )
  )
}
