#### UI ####
appUI <- function(defaults) {
  fluidPage(
    title = "M\u00b2ara",
    lang = "en",
    # shiny sidebar layout
    sidebarLayout(
      #### Sidebar ####
      sidebarPanel = appSidebar(defaults),

      #### Main panel ####
      mainPanel = appMainPanel(defaults)
    )
  )
}
