#' Get auth token to query Dynamodb Data from JsPsych Production Server ----
#' @export
get_auth_token <- function(api=NA) {
  browseURL(api)
  return(token=readline(prompt="Enter token: "))
}
