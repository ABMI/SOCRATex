#' drawPersonTable function
#'
#' @param Text   text data from Preprocessing step
#' @import dplyr
#'
#' @export

drawPersonTable <- function(Text){
  person <- Text %>% select(PERSON_ID) %>% dplyr::distinct() %>% dplyr::count()
  note <- Text %>% select(NOTE_ID) %>% dplyr::count()

  personTable <- as.data.frame(rbind(person, note))

  rownames(personTable) <- c("Person", "Note")
  colnames(personTable) <- c("Count")

  personTable
}
