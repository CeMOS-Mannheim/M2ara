#### load UI functions
source("components/sidebar.R")
source("components/mainpanel.R")

# load defaults
source("functions/defaultSettingsHandler.R")
source("functions/createActionButton.R")
defaults <- defaultsSettingsHandler(userSavedSettings = "settings.csv")

#### UI ####
ui <- fluidPage(
  title = "Peak Explorer",
  lang = "en",
  theme = shinytheme("flatly"),
  # shiny sidebar layout
  sidebarLayout(
    #### Sidebar ####
    sidebarPanel = appSidebar(defaults),

    #### Main panel ####
    mainPanel = appMainPanel(defaults)
  )
)
