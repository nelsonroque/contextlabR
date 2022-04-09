#' Get auth token to query Dynamodb Data from JsPsych Production Server ----
#' @export
get_auth_token <- function(api="https://n1dlcfeo2l.execute-api.us-east-1.amazonaws.com/api/auth") {
  browseURL(api)
  return(token=readline(prompt="Enter token: "))
}
