#' drawPieChart function
#'
#' @param Text   text data from Preprocessing step
#' @import dplyr
#' @import plotly
#'
#' @export

drawPieChart <- function(Text){
  pie <- Text %>% group_by(NOTE_TYPE_CONCEPT_ID) %>% select(NOTE_ID) %>% dplyr::count(NOTE_TYPE_CONCEPT_ID)
  pie <- as.data.frame(pie)
  colnames(pie) <- c("NoteType", "Count")

  plotly::plot_ly(pie, labels = ~pie$NoteType, values=pie$Count, type="pie") %>% layout(title="The Proportions of Note Types of Data")
}
