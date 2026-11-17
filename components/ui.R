#### load UI functions
source("components/sidebar.R")
source("components/mainpanel.R")
source("functions/createActionButton.R")

#### UI ####
appUI <- function(defaults) {
  fluidPage(
    title = "M²ara",
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
