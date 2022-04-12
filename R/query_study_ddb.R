#' Query Dynamodb Data from JsPsych Production Server ----
#' @export
#' @importFrom httr VERB
#' @importFrom jsonlite fromJSON
query_study_ddb <- function(db="dynamodb",
                            api=NA,
                            study=NA,
                            token=NA,
                            fields=NA) {
  payload <- ""
  encode <- "raw"
  response <- VERB("GET",
                   api,
                   body = payload,
                   add_headers(experiment = study,
                               Authorization = token,
                               fields = fields),
                   content_type("application/json"),
                   encode = encode)
  df = jsonlite::fromJSON(content(response, "text"))
  return(df)
}
