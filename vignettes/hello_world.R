install.packages("devtools")

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# install `contextlabR` package ----
devtools::install_github("nelsonroque/contextlabR")
library(contextlabR)

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# specify query parameters ----
fields = 'record_uuid,session_id,event_type,record_save_time,participant_id,records'
# if `fields` ^^^ contains records, you will receive a URL to download the file.
# remove records from `fields` ^^^^ to get ids

experiment = 'Gabby 1st Year Project'

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# run code below to open browser and save auth token ----
api_token = get_auth_token()

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# query database for given data
api_result = query_study_ddb(study=experiment,
                       token=api_token,
                       fields=fields)

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# download JSON from S3 from pre-signed URL ----
df = dl_json_s3(api_result$url)

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# transform step: from json in a single cell, to a tabular dataset ----
df_unnest = unnest_jspsych_data(df)

# pre-process dataset to remove task_sections not of interest ----
df_pp = df_unnest %>%
  mutate(flag_is_risky = grepl("this risky", stimulus_clean)) %>%
  mutate(flag_is_pollutant = grepl("this a pollutant", stimulus_clean)) %>%
  separate(col=stimulus_clean, sep="\\?", into=c("prompt", "word")) %>%
  filter(task_section != "fixation") %>%
  filter(task_section != "instructions") %>%
  filter(task_section != "debriefing")

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# visualize reaction time and overall response bias ----
ggplot(df_pp, aes(rt)) + geom_histogram() + theme_bw()
ggplot(df_pp, aes(response)) + geom_histogram() + theme_bw()

# visualize one word ----
propane_rt = ggplot(df_pp %>% filter(word == "propane"), aes(rt)) +
  geom_histogram() +
  theme_bw()
propane_response = ggplot(df_pp %>% filter(word == "propane"), aes(response)) +
  geom_histogram() +
  theme_bw()
cowplot::plot_grid(propane_response, propane_rt)
