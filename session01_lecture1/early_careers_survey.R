library(tidyverse)
library(skimr)
library(mosaic)

# while it's fine to know about working directories, I suggest 
# you learn to use the package 'here' that makes organising files easy
# https://malco.io/2018/11/05/why-should-i-use-the-here-package/
survey <- read_csv(here::here('data', 'early_careers_survey.csv'))



# Histogram on stress about analytics (anxiety_about_analytics) and day-to-day stress (relaxed_stressful)
ggplot(survey, aes (x=anxiety_about_analytics))+
  geom_histogram(fill="#001e62", alpha = 0.88, boundary=1)+
  theme_minimal()+
  geom_vline(xintercept = 1.5, colour="orange", size = 3, linetype = "dashed")+
  labs(
    title = "Anxiety about analytics",
    subtitle = "1: No anxiety - 5: Very high anxiety",
    x="",
    caption = "Source: LBS Early Careers Analytics Survey"
  )


ggplot(survey, aes (x=relaxed_stressful))+
  geom_histogram(fill="tomato", alpha = 0.88, boundary=1)+
  theme_minimal()+
  labs(
    title = "How stressful is your day to day life?",
    subtitle = "1: No stress - 5: Very High Stress",
    x="",
    caption = "Source: LBS Early Careers Analytics Survey"
  )


# summary stats on measures of stress

survey %>% 
  select(anxiety_about_analytics, relaxed_stressful ) %>% 
  skim()


# Is there a difference in anxiety_about_analytics between men and women?
ggplot(survey, aes (x=anxiety_about_analytics, y = gender))+
  geom_boxplot()+
  geom_jitter()+
  theme_minimal()+
  NULL


mosaic::favstats(anxiety_about_analytics ~ gender, data = survey)



# Is there a difference in day-to-day stress between men and women?

ggplot(survey, aes (x=relaxed_stressful, y = gender))+
  geom_boxplot()+
  geom_jitter()+
  theme_minimal()+
  NULL


mosaic::favstats(relaxed_stressful ~ gender, data = survey)


# Histogram on handedness
ggplot(survey, aes (x=handedness))+
  geom_histogram(bins = 20,fill="#001e62", alpha = .87, boundary=1)+
  theme_minimal()+
  scale_x_continuous(breaks = seq(-1,1,by = 0.2))+
  labs(
    title = "Handedness",
    subtitle = "-1: Exclusively left handed, +1: Exclusively right handed",
    x="",
    caption = "LBS Data Analytics Survey"
  )


# --------------------- HAIRCUT SPEND ------------------------
# summary statistics of haircut 
favstats(~haircut_spend, data=survey)

# summary statistics ofhaircut_spend_spend vs. student
mosaic::favstats(haircut_spend ~ gender, data = survey)

ggplot(survey, aes (x=haircut_spend, y = gender))+
  geom_boxplot()+
  theme_minimal()+
  NULL

ggplot(survey, aes (x=haircut_spend, fill = gender))+
  geom_density(alpha = 0.3)+
  theme_minimal()+
  NULL

t.test(haircut_spend ~ gender, data = survey)

# --------------------- EXERcISE HOURS ------------------------
# summary statistics 
favstats(~exercise_hrs, data=survey)

# summary statistics by gender
mosaic::favstats(exercise_hrs ~ gender, data = survey)

ggplot(survey, aes (x=exercise_hrs, y = gender))+
  geom_boxplot()+
  theme_minimal()+
  NULL

ggplot(survey, aes (x=exercise_hrs, fill = gender))+
  geom_density(alpha = 0.3)+
  theme_minimal()+
  NULL

t.test(exercise_hrs ~ gender, data = survey)

# --------------------- ONLINE HOURS ------------------------
# summary statistics 
favstats(~online_hrs, data=survey)

# summary statistics by gender
mosaic::favstats(online_hrs ~ gender, data = survey)

ggplot(survey, aes (x=online_hrs, y = gender))+
  geom_boxplot()+
  theme_minimal()+
  NULL

ggplot(survey, aes (x=online_hrs, fill = gender))+
  geom_density(alpha = 0.3)+
  theme_minimal()+
  NULL

t.test(online_hrs ~ gender, data = survey)

# --------------------- SLEEP HOURS ------------------------
# summary statistics 
favstats(~sleep_hrs, data=survey)

# summary statistics by gender
mosaic::favstats(sleep_hrs ~ gender, data = survey)

ggplot(survey, aes (x=sleep_hrs, y = gender))+
  geom_boxplot()+
  theme_minimal()+
  NULL

ggplot(survey, aes (x=sleep_hrs, fill = gender))+
  geom_density(alpha = 0.3)+
  theme_minimal()+
  NULL

t.test(sleep_hrs ~ gender, data = survey)


# --------------------- FACEBOOK FRIENDS  ------------------------
# summary statistics 
favstats(~facebook_friends, data=survey)

# summary statistics by gender
mosaic::favstats(facebook_friends ~ gender, data = survey)

ggplot(survey, aes (x=facebook_friends, y = gender))+
  geom_boxplot()+
  theme_minimal()+
  NULL

ggplot(survey, aes (x=facebook_friends, fill = gender))+
  geom_density(alpha = 0.3)+
  theme_minimal()+
  NULL

t.test(facebook_friends ~ gender, data = survey)


# --------------------- motivated_course_grade  ------------------------
# summary statistics 
favstats(~motivated_course_grade, data=survey)

# summary statistics by gender
mosaic::favstats(motivated_course_grade ~ gender, data = survey)

ggplot(survey, aes (x=motivated_course_grade, y = gender))+
  geom_boxplot()+
  theme_minimal()+
  NULL

ggplot(survey, aes (x=motivated_course_grade, fill = gender))+
  geom_density(alpha = 0.3)+
  theme_minimal()+
  NULL

t.test(motivated_course_grade ~ gender, data = survey)

# --------------------- STRESS DURING THE DAY  ------------------------
# summary statistics 
favstats(~relaxed_stressful, data=survey)

# summary statistics by gender
mosaic::favstats(relaxed_stressful ~ gender, data = survey)

ggplot(survey, aes (x=relaxed_stressful, y = gender))+
  geom_boxplot()+
  theme_minimal()+
  NULL

ggplot(survey, aes (x=relaxed_stressful, fill = gender))+
  geom_density(alpha = 0.3)+
  theme_minimal()+
  NULL

t.test(relaxed_stressful ~ gender, data = survey)

# --------------------- STRESS ANALYTICS  ------------------------
# summary statistics 
favstats(~anxiety_about_analytics, data=survey)

# summary statistics by gender
mosaic::favstats(anxiety_about_analytics ~ gender, data = survey)

ggplot(survey, aes (x=anxiety_about_analytics, y = gender))+
  geom_boxplot()+
  geom_jitter()+
  theme_minimal()+
  NULL

ggplot(survey, aes (x=anxiety_about_analytics, fill = gender))+
  geom_density(alpha = 0.3)+
  theme_minimal()+
  NULL

t.test(anxiety_about_analytics ~ gender, data = survey)


# --------------------- HOMEOPATHY  ------------------------
# summary statistics 
survey |> 
  count(homeopathy_works, sort=TRUE) |> 
  mutate(prop = n/sum(n))


# summary statistics by gender
survey |> 
  dplyr::group_by(gender, homeopathy_works) |> 
  summarise (count=n()) |> 
  mutate(prop = count/sum(count))

#using janitor::tabyl()
janitor::tabyl(survey, gender, homeopathy_works,show_na=FALSE)
