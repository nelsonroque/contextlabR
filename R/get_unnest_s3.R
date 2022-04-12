#' Query Dynamodb Data from JsPsych Production Server, return S3 file as JSON
#' @export
#' @importFrom httr VERB
#' @importFrom jsonlite fromJSON
get_unnest_s3 <- function(api=NA, token = NA, experiment=NA, fields=NA) {

  # %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

  # run code below to open browser and save auth token ----
  # enter the token into the `Console` below.
  api_url = api
  if(is.na(token)) {
    token = contextlabR::get_auth_token(api=paste0(api_url,"auth"))
  }

  # %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

  # query database for given data
  api_result = contextlabR::query_study_ddb(api=paste0(api_url,"query"),
                               study=experiment,
                               token=token,
                               fields=fields)

  print("api_result_received")
  print(api_result)
  print(api_result$url)

  # %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

  # download JSON from S3 from pre-signed URL ----
  df = contextlabR::dl_json_s3(api_result$url)

  print("dl_json_s3")

  # %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

  # unnest
  unnest_trials <- function(df) {

    datastack = tibble::tibble()
    for(i in 1:length(df$trials)) {
      is_invalid_record = is.null(unlist(df$trials[i]))

      if(!is_invalid_record) {
        fd = df$trials[i]
        fd_pp = bind_rows(fd) %>%
          mutate(across(where(is.integer), as.character)) %>%
          mutate(across(where(is.double), as.character)) %>%
          mutate(across(where(is.logical), as.character))

        datastack = bind_rows(datastack, fd_pp)
      }
    }
    return(datastack)
  }

  # %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

  # transform step: from json in a single cell, to a tabular dataset ----
  df_unnest = unnest_trials(df)

  # pre-process dataset to remove task_sections not of interest ----
  df_pp = df_unnest %>%
    distinct()

  return(list(raw=df, unnest = df_unnest, no_dups=df_pp))
}
