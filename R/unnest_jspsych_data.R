#' Unnest JsPsych data ----
#' @export
#' @importFrom tibble tibble
#' @importFrom dplyr arrange
#' @importFrom jsonlite fromJSON
unnest_jspsych_data = function(df) {

  datastack = tibble::tibble()
  for(i in 1:length(df$records)) {
    fd = jsonlite::fromJSON(df$records[i])
    datastack = bind_rows(datastack, fd)
  }

  # clean datastack (reorder cols, remove any cols with repeating vals)
  datastack_clean = datastack %>%
    select(participant_id, session_uuid,
           task_section,
           trial_type, trial_index,
           experiment_label, experiment_name,
           stimulus,
           rt, response,
           key_press,
           button_pressed,
           correct,
           time_elapsed,
           record_save_time,
           experiment_trial_timeout, slider_start) %>%
    mutate(stimulus_clean = gsub("<.*?>", "", stimulus))

  return(datastack_clean)
}
