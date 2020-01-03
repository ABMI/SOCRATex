#' drawAgeGraph function
#'
#' @param Text  text data from Preprocessing step
#' @import dplyr
#' @import plotly
#'
#' @export
drawAgeGraph <- function(Text){
  age <- Text %>% dplyr::group_by(YEAR_OF_BIRTH, GENDER_CONCEPT_ID) %>% dplyr::select(YEAR_OF_BIRTH, GENDER_CONCEPT_ID) %>% dplyr::count(YEAR_OF_BIRTH, GENDER_CONCEPT_ID)
  age <-as.data.frame(age)
  colnames(age) <- c("BirthYear", "Gender", "Count")
  age$Gender <- factor(age$Gender)

  plotly::ggplotly(ggplot(age, aes(x=BirthYear, y=Count, color=Gender))
                   + geom_line(size=1) + theme_minimal() + scale_color_manual(values = c("#3300FF", "#FF9933")))
}
