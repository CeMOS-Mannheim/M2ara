#' Map spots from 384 well plate to spots on another plate format
#'
#' @param spot     Character, spot on a 384 well plate (e.g. "A1")
#' @param target   Character, plate format to convert to. (Currently the only option is "96")
#'
#' @return
#' Character, spot on the target plate format.
mapSpot <- function(spot, target = c("96", "384")) {
  target <- match.arg(target)

  letter <- toupper(str_remove(spot, pattern = "([0-9]+)"))
  row <- grep(pattern = letter, x = LETTERS)
  col <- parse_number(spot)

  switch (target,
    "96" = {
      row96 <- ceiling(row/2)
      col96 <- ceiling(col/2)

      return(paste0(LETTERS[row96], col96))
    },
    "384" = {
      row384 <- c(row+(row-1), row+(row-1), row+row, row+row)
      col384 <- c(col+(col-1), col+col, col+(col-1), col+col)

      return(paste0(LETTERS[row384], col384))
    }
  )
}
