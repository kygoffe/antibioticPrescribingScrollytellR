library(tidyverse)
library(gridExtra)

# Read participants information
par_path <- "./report/Info"

par_filenames <- dir(par_path, pattern = "*.csv")

par <- par_filenames %>% 
  set_names(fs::path_ext_remove(basename(.))) %>% 
  map_df(~ read_csv(file.path(par_path, .)), .id = "name") %>% 
  janitor::clean_names()

par <- par %>% 
  select(name, question_key, response) %>% 
  filter(!is.na(response)) %>% 
  filter(!str_detect(question_key, "-quantised")) %>% 
  filter(question_key != "END QUESTIONNAIRE") %>% 
  mutate(question = case_when(
    question_key == 'parinfo1' ~ 'job_title',
    question_key == 'parinfo2' ~ 'industry',
    question_key == 'parinfo3' ~ 'age_band',
    question_key == 'parinfo4' ~ 'field_of_expertise',
    question_key == 'parinfo5' ~ 'datavis_skills'
    )
  ) %>% 
  relocate(question,.before = response)
  

parinfo <- par %>% select(-question_key) %>% 
  pivot_wider(
  names_from = question, 
  values_from = response
) %>% 
  arrange(name)


# Read and append all Response tools
path <- "./report/Response_Tools"

tool_filenames <- dir(path, pattern = "*.csv")

tools <- tool_filenames %>% 
  set_names(fs::path_ext_remove(basename(.))) %>% 
  map_df(~ read_csv(file.path(path, .)), .id = "name") %>% 
  janitor::clean_names()


tools_response <- tools %>% 
  filter(!is.na(participant_private_id)) %>% 
  filter(question_key != "BEGIN QUESTIONNAIRE" & question_key != "END QUESTIONNAIRE" ) %>% 
  filter(!str_detect(question_key,"-quantised")) %>% 
  select(name, question_key, response)


tools_response %>% 
  group_by(question_key) %>% 
  summarise(mean_score = mean(response),
            min_score = min(response),
            max_score = max(response))

# ANOVA test?

ggpubr::ggboxplot(
  tools_response %>% 
    filter(str_detect(question_key, "4")) %>% 
    mutate(group = substr(name,1,5)),
  x = "group", y = "response"
)

res1 <-  tools_response %>% 
  filter(str_detect(question_key, "1")) %>% 
  mutate(group = substr(name,1,5))

res1.aov <- aov(response ~ group, data = res1)

summary(res1.aov)


res2 <-  tools_response %>% 
  filter(str_detect(question_key, "2")) %>% 
  mutate(group = substr(name,2,5))

res2.aov <- aov(response ~ group, data = res2)

summary(res2.aov)

res3 <-  tools_response %>% 
  filter(str_detect(question_key, "3")) %>% 
  mutate(group = substr(name,2,5))

res3.aov <- aov(response ~ group, data = res3)

summary(res3.aov)

res4 <-  tools_response %>% 
  filter(str_detect(question_key, "4")) %>% 
  mutate(group = substr(name,2,5))

res4.aov <- aov(response ~ group, data = res4)

summary(res4.aov)


res5 <-  tools_response %>% 
  filter(str_detect(question_key, "5")) %>% 
  mutate(group = substr(name,2,5))

res5.aov <- aov(response ~ group, data = res5)

summary(res5.aov)

res6 <-  tools_response %>% 
  filter(str_detect(question_key, "6")) %>% 
  mutate(group = substr(name,2,5))

res6.aov <- aov(response ~ group, data = res6)

summary(res6.aov)

res7 <-  tools_response %>% 
  filter(str_detect(question_key, "7")) %>% 
  mutate(group = substr(name,2,5))

res7.aov <- aov(response ~ group, data = res7)

summary(res7.aov)


# Create summary data
tools_summary <- aggregate(response ~ question_key, tools_response,
                           function(x) c(mean = mean(x),
                                         sd = sd(x),
                                         se = sd(x)/sqrt(length(x))))

tools_summary <- data.frame(question = tools_summary[,1], tools_summary$response)

tools_summary <- tools_summary %>% 
  mutate(group = case_when(
    str_detect(question, "1") ~ 'User customisation',
    str_detect(question, "2") ~ 'Knowledge discovery',
    str_detect(question, "3") ~ 'Alerting',
    str_detect(question, "4") ~ 'Information delivery',
    str_detect(question, "5") ~ 'Visual design',
    str_detect(question, "6") ~ 'Structure',
    str_detect(question, "7") ~ 'Sign-posting'
  ),
  tools = case_when(
    str_detect(question, "dd") ~ "DDS",
    str_detect(question, "excel") ~ "Excel",
    str_detect(question, "prequipp") ~ "Tableau"
  )
  ) %>% 
  mutate(tools = factor(tools, levels = c("Excel", "Tableau", "DDS")),
         group = factor(group, levels = c("User customisation","Knowledge discovery",
                                          "Alerting", "Information delivery", "Visual design",
                                          "Structure", "Sign-posting")))

g1 <- ggplot(tools_summary, 
       aes(x = group, y = mean, fill = tools)) +
  geom_bar(stat = "identity", color = "black",
           position = position_dodge()) +
  nhsbsaR::theme_nhsbsa_gg() +
  nhsbsaR::scale_fill_nhsbsa (palette = NA) +
  ylab("Mean Likert score") +
  xlab("Key criteria")
  

g1
  

# Y axis: I found this task easy to answer. 
# 1: stronly disagree, 5: stronly agree
# X axis: Usability testing tasks, remove C only keep 1: xxx etc


# Plot Par_11 for the comparison
par11 <- tools %>% 
  filter(!is.na(participant_private_id)) %>% 
  filter(question_key != "BEGIN QUESTIONNAIRE" & question_key != "END QUESTIONNAIRE" ) %>% 
  filter(!str_detect(question_key,"-quantised")) %>% 
  select(name, question_key, response) %>% 
  #filter to par_11
  filter(str_detect(name, "_11"))

# Create summary data
tools_summary11 <- aggregate(response ~ question_key, par11,
                           function(x) c(mean = mean(x),
                                         sd = sd(x),
                                         se = sd(x)/sqrt(length(x))))

tools_summary11 <- data.frame(question = tools_summary11[,1], tools_summary11$response)

tools_summary11 <- tools_summary11 %>% 
  mutate(group = case_when(
    str_detect(question, "1") ~ 'User customisation',
    str_detect(question, "2") ~ 'Knowledge discovery',
    str_detect(question, "3") ~ 'Alerting',
    str_detect(question, "4") ~ 'Information delivery',
    str_detect(question, "5") ~ 'Visual design',
    str_detect(question, "6") ~ 'Structure',
    str_detect(question, "7") ~ 'Sign-posting'
  ),
  tools = case_when(
    str_detect(question, "dd") ~ "DDS",
    str_detect(question, "excel") ~ "Excel",
    str_detect(question, "prequipp") ~ "Tableau"
  )
  ) %>% 
  mutate(tools = factor(tools, levels = c("Excel", "Tableau", "DDS")),
         group = factor(group, levels = c("User customisation","Knowledge discovery",
                                          "Alerting", "Information delivery", "Visual design",
                                          "Structure", "Sign-posting")))


g2 <- ggplot(tools_summary11, 
       aes(x = group, y = mean, fill = tools)) +
  geom_bar(stat = "identity", color = "black",
           position = position_dodge()) +
  nhsbsaR::theme_nhsbsa_gg() +
  nhsbsaR::scale_fill_nhsbsa (palette = NA) +
  ylab("Likert score") +
  xlab("Key criteria") #+
  # theme(legend.position = "none")

g2


grid.arrange(g1, g2,ncol=1, nrow =2)

#################### Suggestion from Alma (02/09/2022)

# ANOVA analysis on 





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
  separate(name, c("name", "id", "id_1"), sep = "_") 


overall_sus <- sus_score_summary %>% group_by(name) %>% 
  summarise(average_sus_score = mean(sus_score),
            .groups = "drop")

sus_score11 <- sus %>% 
  filter(str_detect(name, "_11")) %>% 
  group_by(name) %>% 
  summarise(sus_score = sum(response_2) * 2.5)

sus_score_summary11 <- sus_score11 %>%
  separate(name, c("name", "id", "id_1"), sep = "_") 

overall_sus11 <- sus_score_summary11 %>% group_by(name) %>% 
  summarise(average_sus_score = mean(sus_score),
            .groups = "drop") %>% 
  mutate(name = case_when(
    name == "Tool1" ~ "Tool1_P11",
    name == "Tool2" ~ "Tool2_P11",
    name == "Tool3" ~ "Tool3_P11",
    
  ))
 
sus_df <- bind_rows(overall_sus, overall_sus11) %>% 
  mutate(group = case_when(
    str_detect(name, "Tool1") ~ "Excel",
    str_detect(name, "Tool2") ~ "Tableau",
    str_detect(name, "Tool3") ~ "DDS"
  ) ) %>% 
  mutate(average_sus_score = round(average_sus_score,1))
  

ggplot(sus_df, 
       aes(x = name, y = average_sus_score, fill = group)) +
  geom_bar(stat = "identity", color = "black",
           position = position_dodge()) +
  ylim(c(0,100)) +
  # geom_errorbar(aes(ymin = mean-sd, ymax = mean+sd),
  #               width = .2,
  #               position = position_dodge(.9)) +
  theme_classic() +
  geom_text(aes(label = average_sus_score), position = position_dodge(width = 0.9),
            vjust = -0.25)




sus_score_summary %>% 
  group_by(id_1) %>% 
  summarise(max_score = max(sus_score)) %>% 
  inner_join(sus_score_summary ,
             by = c("id_1","max_score" = "sus_score" )) %>% 
  group_by(name) %>% 
  count(id)
  
# 9 out of 11 gave the highest score.
  
 
