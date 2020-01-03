#' databaseConnection function
#'
#' databaseConnection function is using DatabaseConnector package. The createConnectionDetails and connect functions are congested into a single function.
#'
#' @param dtm DocumentTermMatrix want to explore.
#' @param TopicNum  The range of topic numbers. It should contain at least to numbers
#' @param By The interval of topics when searching
#' @param metrics String or vector of possible metrics: "Griffiths2004", "CaoJuan2009", "Arun2010", "Deveaud2014".
#' @import ldatuning
#'
#' @export
drawOptimalTopicNumber <- function(dtm, TopicNum, By, metrics){
  OptimalTopicNumber <<- ldatuning::FindTopicsNumber(dtm, topics = seq(from = TopicNum[1], to = TopicNum[2], by = By), metrics = metrics, method = "Gibbs")
  ldatuning::FindTopicsNumber_plot(OptimalTopicNumber)
}
