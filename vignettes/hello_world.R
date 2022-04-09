devtools::install_github("nelsonroque/contextlabR")
library(contextlab)
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# specify query parameters
fields = 'record_uuid,session_id,event_type,record_save_time,participant_id,records'
# if `fields` ^^^ contains records, you will receive a URL to download the file.
# remove records from `fields` ^^^^ to get ids

experiment = 'Gabby 1st Year Project'

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# run code below to open browser and save auth token
get_auth_token()
api_token = 'eyJraWQiOiJiR3NOR2x2TjJYMXU1S1BKcGl2dGZiZ2UrMUJhZ0NZZWlnVitDSUl0cVhrPSIsImFsZyI6IlJTMjU2In0.eyJhdF9oYXNoIjoiYTJDeEZsM0RsdXY5cFVLVUl3OHRadyIsInN1YiI6IjNkMjYxY2IzLTdkMTktNDZhZi05NWFiLTNjZDk0YWZjMjRkNSIsImVtYWlsX3ZlcmlmaWVkIjp0cnVlLCJpc3MiOiJodHRwczpcL1wvY29nbml0by1pZHAudXMtZWFzdC0xLmFtYXpvbmF3cy5jb21cL3VzLWVhc3QtMV9ucENsanVPQUEiLCJwaG9uZV9udW1iZXJfdmVyaWZpZWQiOnRydWUsImNvZ25pdG86dXNlcm5hbWUiOiJuZWxzb25yb3F1ZSIsImF1ZCI6IjQwbG4yZ3RkazliNWZwNmI4ZzhybGZ2N3I1IiwidG9rZW5fdXNlIjoiaWQiLCJhdXRoX3RpbWUiOjE2NDk0NzYzNjksInBob25lX251bWJlciI6IisxNzg2ODUzMjA4NCIsImV4cCI6MTY0OTQ3OTk2OSwiaWF0IjoxNjQ5NDc2MzY5LCJqdGkiOiJhNjJjMTA4MS1lYTg4LTQyOTctYWQwZC01Yzg0MTY5OGY1N2YiLCJlbWFpbCI6Im5lbHNvbi5yb3F1ZUB1Y2YuZWR1In0.h05Uw0fsEYkRnuWOeIi9bEyyJGFi2ZuJGVIgWz1QKgFUdSjidkEoypRjJs-Z5yuLOaeIrBcuL6cR43vPS2DcgW4ar38RsmJFjJrQ1E-A3UWQ3mmFzA9P5J2ZKG_jSie8AUoYVgJXC-NeVFjEcJEb2fS0q5cItCa6EoprYuLF4JUUl_GmuE9TyArVV__dr8miUDT6JzRFIQRH2gvOvmRVwrvEkukujfXGTuwPmghPkj3XqT9nsVr9NESgizSlckVGFjbB6JFxqKMXtyKvT1LVa9xM0HBFdBZDXjpWI0ThfOn5NiwiY7o0kcQklfyUCgSBANmziUeULgxVK1BX1hJrxg'
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# query database for given data
api_result = query_study_ddb(study=experiment,
                       token=api_token,
                       fields=fields)

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# download JSON from S3 from pre-signed URL
df = dl_json_s3(api_result$url)

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# transform step: from json in a single cell, to a tabular dataset
df_unnest = unnest_jspsych_data(df)

# pre-process dataset to remove task_sections not of interest
df_pp = df_unnest %>%
  mutate(flag_is_risky = grepl("this risky", stimulus_clean)) %>%
  mutate(flag_is_pollutant = grepl("this a pollutant", stimulus_clean)) %>%
  separate(col=stimulus_clean, sep="\\?", into=c("prompt", "word")) %>%
  filter(task_section != "fixation") %>%
  filter(task_section != "instructions") %>%
  filter(task_section != "debriefing")

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# visualize reaction time
ggplot(df_pp, aes(rt)) + geom_histogram() + theme_bw()

# visualize one word
propane_rt = ggplot(df_pp %>% filter(word == "propane"), aes(rt)) +
  geom_histogram() +
  theme_bw()
propane_response = ggplot(df_pp %>% filter(word == "propane"), aes(response)) +
  geom_histogram() +
  theme_bw()
cowplot::plot_grid(propane_response, propane_rt)
