#' cohortNoteExtract function
#'
#' cohortNoteExtract function extracts NOTE data from the Cohort generated fron ATLAS, an open source data analysis platform of OMOP CDM.
#'
#' @param num   The number of clinical reports want to extract
#' @param date  The note dates of the reports. It should contain the min date and max date.
#' @param result If TRUE, then the Note of the Cohort population will be extracted. Default is FALSE
#' @param resultdb The database which contains the cohort result of the ATLAS
#' @param cohort The cohort id generated from ATLAS
#' @param noteType The note type concept ids of the NOTE table. It uses the standardized concepts of OMOP CDM
#'
#' @import DatabaseConnector
#' @import SqlRender
#'
#' @export

cohortNoteExtract <- function(num, date, result = FALSE, resultdb, cohort, noteType){
  if(result==T){
    sql <- "select top @num a.*, b.YEAR_OF_BIRTH, b.GENDER_CONCEPT_ID from NOTE a, PERSON b
                            where (left(note_date, 4) >= @min and left(note_date, 4) <= @max)
                              and a.person_id in (select subject_id from @resultdb where cohort_definition_id=@cohort)
                              and note_type_concept_id in (@note_type)
                              and a.person_id=b.person_id
                            order by newid()"
    sql <- SqlRender::render(sql, num = num, min=date[1], max=date[2], resultdb=resultdb, cohort=cohort, note_type=noteType)

    Text <<- DatabaseConnector::querySql(connection, sql)
    colnames(Text) <<- toupper(colnames(Text))
    JSON <<- c()
  } else{
    sql <- "select top @num a.*, b.YEAR_OF_BIRTH, b.GENDER_CONCEPT_ID from NOTE a, PERSON b
                            where (left(note_date, 4) >= @min and left(note_date, 4) <= @max)
                              and note_type_concept_id in (@note_type)
                              and a.person_id=b.person_id
                            order by newid()"
    sql <- SqlRender::render(sql, num = num, min=date[1], max=date[2], note_type=noteType)

    Text <<- DatabaseConnector::querySql(connection, sql)
    JSON <<- c()
  }
}
