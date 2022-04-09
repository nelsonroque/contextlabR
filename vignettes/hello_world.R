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
# enter the token into the `Console` below.
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
  distinct()

# derive study condition
df_debrief = df_pp %>%
  filter(task_section == "debriefing") %>%
  mutate(completion_code = substr(prompt, 16,20)) %>%
  mutate(study_condition = ifelse(completion_code == "01234", "is_risky", "is_pollutant"))

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# aggregates
agg_participant = df_pp %>%
  group_by(participant_id) %>%
  summarise(mean_rt = mean(rt, na.rm=T),
            sd_rt = sd(rt, na.rm=T),
            n = n())

# get aggregates by prompt
agg_participant_prompt = df_pp %>%
  filter(prompt != "+") %>%
  filter(!grepl("Completion Code",prompt)) %>%
  group_by(participant_id, prompt) %>%
  summarise(mean_rt = mean(rt, na.rm=T),
            median_rt = median(rt, na.rm=T),
            max_rt = max(rt, na.rm=T),
            sd_rt = sd(rt, na.rm=T),
            n = n())

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# create filtered dataset
df_filt =  df_pp %>%
  filter(task_section != "fixation") %>%
  filter(task_section != "instructions") %>%
  filter(task_section != "debriefing")

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# visualize reaction time and overall response bias ----
ggplot(df_filt, aes(rt)) + geom_histogram() + theme_bw()
ggplot(df_filt, aes(response)) + geom_histogram() + theme_bw()

# visualize one word ----
propane_rt = ggplot(df_filt %>% filter(word == "propane"), aes(rt)) +
  geom_histogram() +
  theme_bw()
propane_response = ggplot(df_filt %>% filter(word == "propane"), aes(response)) +
  geom_histogram() +
  theme_bw()
cowplot::plot_grid(propane_response, propane_rt)