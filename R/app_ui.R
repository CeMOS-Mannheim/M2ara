#### UI ####
appUI <- function(defaults) {
  fluidPage(
    title = "M\u00b2ara",
    lang = "en",
    # feature colours and the UI logic (selection, shortcuts, share links)
    tags$head(
      tags$script(HTML(paste0(
        "window.m2araColours = ['", paste(featureColours(4), collapse = "', '"), "'];"
      ))),
      tags$script(src = "m2ara/app.js")
    ),
    tags$style(HTML(
      "#mzTable td.m2ara-check-col, #mzTable th.m2ara-check-col {
         width: 20px !important; min-width: 20px !important;
         max-width: 20px !important; padding: 0 !important;
       }
       #mzTable td.m2ara-check-col input { margin: 0; }
       #mzTable th.m2ara-check-col { padding-right: 0 !important; }
       #m2araLinkBox {
         display: none; position: fixed; z-index: 10001; right: 40px; bottom: 40px;
         background: #ffffff; border: 1px solid #cccccc; border-radius: 4px;
         padding: 10px 14px; box-shadow: 0 2px 10px rgba(0,0,0,0.2);
         width: 520px; font-size: 90%;
       }
       #m2araShortcuts {
         display: none; position: fixed; z-index: 10000; right: 40px; top: 40px;
         background: #ffffff; border: 1px solid #cccccc; border-radius: 4px;
         padding: 10px 14px; box-shadow: 0 2px 10px rgba(0,0,0,0.2);
         font-size: 80%;
       }
       #m2araShortcuts table td { padding: 1px 8px 1px 0; }
       #m2araShortcuts kbd { background: #f7f7f7; border: 1px solid #cccccc;
         border-radius: 3px; padding: 0 4px; font-size: 90%; }"
    )),
    # share link box, filled by the server
    div(id = "m2araLinkBox",
        tags$b("Share link"),
        tags$div(style = "display: flex; gap: 6px; margin-top: 4px;",
                 tags$input(id = "m2araLinkInput", type = "text", readonly = "readonly",
                            style = "flex: 1; font-size: 90%;"),
                 tags$button(id = "m2araLinkCopy", type = "button",
                             class = "btn btn-default btn-sm", "Copy"),
                 tags$button(id = "m2araLinkClose", type = "button",
                             class = "btn btn-default btn-sm", "\u00d7")),
        tags$div(id = "m2araLinkHint", style = "font-size: 75%; color: #777;",
                 "Select the text and press Ctrl+C if the copy button does nothing."),
        tags$div(id = "m2araLinkLocal", style = "font-size: 75%; color: #b36b00; display: none;",
                 "This link points to this computer (localhost). To use it on another machine, open 'Open shared view' there and paste it.")),
    # shortcut overview, toggled with Ctrl+/
    div(id = "m2araShortcuts",
        tags$b("Keyboard shortcuts"),
        tags$table(
          tags$tr(tags$td(tags$kbd("Ctrl+Enter")), tags$td("Process spectra")),
          tags$tr(tags$td(tags$kbd("Ctrl+F")), tags$td("Focus the m/z search")),
          tags$tr(tags$td(tags$kbd("Ctrl+S")), tags$td("Save plot")),
          tags$tr(tags$td(tags$kbd("Ctrl+E")), tags$td("Export results (ZIP)")),
          tags$tr(tags$td(tags$kbd("Ctrl+D")), tags$td("Deselect all")),
          tags$tr(tags$td(tags$kbd("Ctrl+Shift+1-4")), tags$td("Toggle feature 1-4")),
          tags$tr(tags$td(tags$kbd("Ctrl+/")), tags$td("This overview")),
          tags$tr(tags$td(tags$kbd("Esc")), tags$td("Deselect all"))
        )
    ),
    # shiny sidebar layout
    sidebarLayout(
      #### Sidebar ####
      sidebarPanel = appSidebar(defaults),

      #### Main panel ####
      mainPanel = appMainPanel(defaults)
    )
  )
}
