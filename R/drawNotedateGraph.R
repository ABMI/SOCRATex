#' drawNotedateGraph function
#'
#' @param Text  text data from Preprocessing step
#' @import dplyr
#' @import plotly
#'
#' @export

drawNotedateGraph <- function(Text){
  Text$NOTE_DATE <- Text$NOTE_DATE %>% substring(1, 4)
  date <- Text %>% dplyr::group_by(NOTE_DATE, NOTE_TYPE_CONCEPT_ID) %>% dplyr::select(NOTE_DATE, NOTE_TYPE_CONCEPT_ID) %>% dplyr::count(NOTE_DATE, NOTE_TYPE_CONCEPT_ID)
  date <- as.data.frame(date)
  date$NOTE_TYPE_CONCEPT_ID <- factor(date$NOTE_TYPE_CONCEPT_ID)
  colnames(date) <- c("NoteDate", "NoteType", "Count")

  plotly::ggplotly(ggplot(date, aes(x=NoteDate, y=Count, fill=NoteType)) + geom_bar(stat = "identity", position = "stack") +
                     theme(axis.text.x = element_text(angle=45)) + scale_fill_brewer(palette = "Set1"))
}
