#' noteExtract
#'
#' This function extracts NOTE data from CSV upload data. The CSV data need to be OMOP CDM NOTE table format.
#'
#' @param Text  text data from Preprocessing step
#' @param num   The number of clinical reports want to extract
#' @param date  The note dates of the reports. It should contain the min date and max date.
#' @param noteType The note type concept ids of the NOTE table. It uses the standardized concepts of OMOP CDM
#' @import dplyr
#' @export

noteExtract <- function(Text, num, date, noteType){
  JSON <<- c()
  Text <- Text %>% filter(NOTE_TYPE_CONCEPT_ID %in% noteType)
  Text <- Text %>% filter(substring(NOTE_DATE, 1, 4) >= date[1] & substring(NOTE_DATE, 1, 4) <= date[2])
  Text <<- head(Text, num)
  Text_corpus <<- Text$NOTE_TEXT
}
