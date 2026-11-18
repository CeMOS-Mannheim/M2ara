createActionButton <- function(inputId, label, icon) {
  actionButton(inputId = inputId,
               label = label,
               icon = icon(icon),
               style='padding:6px; font-size:80%')
}
