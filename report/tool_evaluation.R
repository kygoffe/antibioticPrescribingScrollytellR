# Read and append all Response tools

library(tidyverse)
path <- "./report/Response_Tools"

filenames <- list.files(
  path = path,
  pattern = "*.csv",
  full.names = TRUE
)

tools_response <- purrr::map_df(filenames, read.csv, stringsAsFactors  = FALSE)  


tools_response <- tools_response %>% 
  filter(!is.na(Participant.Private.ID)) %>% 
  filter(Question.Key != "BEGIN QUESTIONNAIRE" & Question.Key != "END QUESTIONNAIRE" ) %>% 
  filter(!str_detect(Question.Key,"-quantised"))


tools_response %>% 
  group_by(Question.Key) %>% 
  summarise(median_score = median(Response),
            mean_score = mean(Response),
            min_score = min(Response),
            max_score = max(Response))
# SUS evaluation

sus_path <- "./report/SUS"

sus_filenames <- dir(sus_path, pattern = "*.csv")

sus <- sus_filenames %>% 
  set_names(fs::path_ext_remove(basename(.))) %>% 
  map_df(~ read_csv(file.path(sus_path, .)), .id = "name") %>% 
  janitor::clean_names()

  
  

sus <- sus %>% 
  filter(!is.na(participant_private_id)) %>% 
  filter(question_key != "BEGIN QUESTIONNAIRE" & question_key != "END QUESTIONNAIRE" ) %>% 
  filter(event_index != "END OF FILE") %>% 
  filter(!str_detect(question_key,"-quantised")) %>% 
  mutate(event_index = as.numeric(event_index)/2)


# Calculate SUS
# Q1, Q3, Q5, Q7, Q9 take 1 away
# 5 - Q2, Q4, Q6, Q8, Q10

sus <- sus %>% 
  mutate(response_2 = case_when(
    event_index == 1 ~ response - 1,
    event_index == 3 ~ response - 1,
    event_index == 5 ~ response - 1,
    event_index == 7 ~ response - 1,
    event_index == 9 ~ response - 1,
    event_index == 2 ~ 5 - response,
    event_index == 4 ~ 5 - response,
    event_index == 6 ~ 5 - response,
    event_index == 8 ~ 5 - response,
    event_index == 10 ~ 5 - response
  ))

sus_score <- sus %>% 
  group_by(name) %>% 
  summarise(sus_score = sum(response_2) * 2.5)


sus_score_summary <- sus_score %>%
  separate(name, c("first", "id", "id_1"), sep = "_") %>% 
  select(-first)




sus_score_summary %>% group_by(id) %>% 
  summarise(average_sus_score = mean(sus_score),
            .groups = "drop")
  
sus_score_summary %>% 
  arrange(id_1)  %>% 
  View()
  
# 8 out of 10 gave the highest score.
  
 
