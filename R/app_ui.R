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
       /* no filter box for the checkbox column */
       #mzTable tfoot tr td:first-child input { display: none !important; }
       #mzTable tfoot tr td:first-child { padding: 0 !important; }
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
         border-radius: 3px; padding: 0 4px; font-size: 90%; }

       .m2ara-data-status {
         font-size: 80%; color: #6b6b6b; margin-top: 5px; line-height: 1.3;
       }

       /* labelled groups in the sidebar */
       .m2ara-group {
         border: 1px solid #ececec; border-radius: 8px;
         padding: 6px 8px 2px; margin-bottom: 8px;
       }
       .m2ara-group-title {
         font-size: 85%; font-weight: 600; color: #555;
         margin-bottom: 4px; display: flex; align-items: center; gap: 4px;
       }

       /* the buttons of the settings row start on the same line */
       .m2ara-action { margin-top: 21px; }
       .m2ara-group .form-group { margin-bottom: 6px; }

       /* dataset card in the sidebar */
       .m2ara-dataset {
         border: 1px solid #e0e0e0; border-radius: 8px; background: #fafafa;
         padding: 7px 9px; margin-top: 8px; font-size: 85%;
         line-height: 1.35; overflow: hidden;
       }
       /* cut the path on its left side, so the end (the dataset) stays visible */
       .m2ara-path {
         direction: rtl; text-align: left; white-space: nowrap;
         overflow: hidden; text-overflow: ellipsis;
         font-family: monospace; font-size: 92%; color: #333;
       }
       .m2ara-path > span { direction: ltr; unicode-bidi: embed; }
       .m2ara-dataset-label {
         color: #9a9a9a; font-size: 85%; margin-top: 4px;
         text-transform: uppercase; letter-spacing: 0.04em;
       }
       .m2ara-dataset-meta {
         color: #6b6b6b; white-space: nowrap; overflow: hidden;
         text-overflow: ellipsis;
       }
       .m2ara-status { margin-top: 4px; display: flex; align-items: center; gap: 5px; }
       .m2ara-status-dot {
         width: 8px; height: 8px; border-radius: 50%; background: #b0b0b0;
         flex: 0 0 8px;
       }
       .m2ara-status.ok .m2ara-status-dot { background: #2e9e4f; }
       .m2ara-status.changed .m2ara-status-dot { background: #d98a00; }
       .m2ara-status.error .m2ara-status-dot { background: #cf3b3b; }
       .m2ara-status.pending .m2ara-status-dot { background: #3f7ecb; }
       .m2ara-status.changed { color: #a86b00; }
       .m2ara-status.error { color: #b02a2a; }
       .m2ara-dataset-details { margin-top: 5px; font-size: 95%; }
       .m2ara-dataset-details summary { cursor: pointer; color: #6b6b6b; outline: none; }
       .m2ara-dataset-path {
         word-break: break-all; color: #444; margin-top: 3px;
         font-family: monospace; font-size: 90%;
       }
       .m2ara-dataset-note { color: #6b6b6b; margin-top: 2px; }"
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
