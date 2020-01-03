#' databaseConnection function
#'
#' databaseConnection function is using DatabaseConnector package. The createConnectionDetails and connect functions are congested into a single function.
#'
#' @param server   server information of the database
#' @param user  user id of the database
#' @param password password of the database
#' @param schema  schema of the database want to access
#' @import DatabaseConnector
#'
#' @export
databaseConnection <- function(server, user, password, schema){
  connectionDetails <<- DatabaseConnector::createConnectionDetails(dbms='sql server'
                                                                  , server=server
                                                                  , schema=schema
                                                                  , user=user
                                                                  , password=password)
  connection <<- DatabaseConnector::connect(connectionDetails)
}
