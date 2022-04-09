#' Download DynamoDB export from S3 ----
#' @export
#' @importFrom httr VERB
#' @importFrom jsonlite fromJSON
dl_json_s3 <- function(url=NA) {

  payload <- ""
  encode <- "raw"
  response <- httr::VERB("GET",
                   url,
                   encode = "json")

  df = jsonlite::fromJSON(content(response, "text"))

  return(df)
}
