#Gael Blanchard
library(e1071)
library(tidyr)
library(dplyr)
library(devtools)
library(ggplot2)
library(ggcorrplot)
library(gganimate)
library(gapminder)
setwd("/Users/ak/Desktop/Visualization_Portfolio")

# 1. Gun Violence Data
gun_data <- data.frame(read.csv("gunviolence.csv", header = TRUE, sep = ","))
gun_data$date <- as.Date(gun_data$date)
#Remove years 2013 and 2018 from our data for analysis because
#both years are incomplete and will give us incomplete insights
#Preparing our data for analyis based on gender, state is already satisfied,suicide
gun_2014_2017_data <- gun_data[grepl("^2014",gun_data$date) | grepl("^2015",gun_data$date) | grepl("^2016",gun_data$date) | grepl("^2017",gun_data$date) ,]
#Brief analysis of our rows of interest
summary(gun_2014_2017_data)
participant_status.freq <- gun_2014_2017_data$participant_status
summary(participant_status.freq)
participant_age_group.freq <- gun_2014_2017_data$participant_age_group
summary(participant_age_group.freq)
participant_age.freq <- gun_2014_2017_data$participant_age
summary(participant_age.freq)
notes.freq <- gun_2014_2017_data$notes
summary(notes.freq)
incident_characteristics.freq <- gun_2014_2017_data$incident_characteristics
summary(incident_characteristics)
#After analyis extract data we want to use
gun_2014_2017_data <- gun_2014_2017_data %>% mutate(
	n_affected = n_killed + n_injured,
	male_involved = case_when(
									grepl("*Male*",participant_gender) ~ 1,
									TRUE ~ 0
									),
	female_involved = case_when(
									grepl("*Female*",participant_gender) ~ 1,
									TRUE ~ 0
									),
	multiple_involved = case_when(
		                            grepl("*1::*",participant_gender) ~ 1,
		                            grepl("*1::*",participant_status) ~ 1,
		                            grepl("*1::*",participant_type) ~ 1,
		                            grepl("*1::*",participant_age_group) ~1,
		                            grepl("*1::*",participant_age) ~ 1,
		                            TRUE ~ 0
		                            ),
	resulted_in_suicide = case_when(
		                          	grepl("*suicide*",incident_characteristics, ignore.case=TRUE) ~ 1,
		                          	TRUE ~ 0
		                          	),
	officer_related = case_when(
									grepl("*officer*",incident_characteristics,ignore.case=TRUE) ~ 1,
									TRUE ~ 0
									),
	drug_related = case_when(
									grepl("*drug*",incident_characteristics,ignore.case=TRUE) ~1,
									TRUE ~ 0
									),
	vehicular = case_when(
									grepl("*drive-by*",incident_characteristics, ignore.case=TRUE) ~1,
									TRUE ~ 0
									),
	accidental = case_when(
									grepl("*accidental*",incident_characteristics, ignore.case=TRUE) ~1,
									TRUE ~ 0
									),
	alcohol_related = case_when(
									grepl("*alcohol*",incident_characteristics, ignore.case=TRUE) ~1,
									TRUE ~ 0
									),
	child_participant = case_when(
									grepl("*child*",participant_age_group, ignore.case=TRUE) ~1,
									TRUE ~ 0
									),
	teen_participant = case_when(
									grepl("*teen*",participant_age_group, ignore.case=TRUE) ~1,
									TRUE ~ 0
									),
	adult_participant = case_when(
									grepl("*adult*",participant_age_group, ignore.case=TRUE) ~1,
									TRUE ~ 0
									),
	led_to_arrest = case_when(
									grepl("*arrest*", participant_status, ignore.case=TRUE) ~1,
									TRUE ~ 0
									),
	led_to_death = case_when(
									grepl("*killed*",participant_status, ignore.case=TRUE) ~1,
									TRUE ~ 0
									),
	led_to_injury = case_when(
									grepl("*injured*",participant_status,ignore.case=TRUE) ~1,
									TRUE ~ 0
									),
	robbery_related = case_when(
									grepl("*robbery*",incident_characteristics,ignore.case=TRUE) ~1,
									TRUE ~ 0
									),
	domestic_related = case_when(
									grepl("*domestic*",incident_characteristics,ignore.case=TRUE) ~1,
									TRUE ~ 0
									),
	school_related = case_when(
									grepl("*school*",incident_characteristics,ignore.case=TRUE) ~1,
									TRUE ~ 0
									),
	gang_related = case_when(
									grepl("*gang*",incident_characteristics,ignore.case=TRUE) ~1,
									TRUE ~ 0
									),
	defense_related = case_when(
									grepl("*dgu*",incident_characteristics,ignore.case=TRUE) ~1,
									grepl("*defensive*",incident_characteristics,ignore.case=TRUE) ~1,
									TRUE ~ 0
									)
 )
summary(gun_2014_2017_data)
#correlations [At this stage correlations do not give any insight to these relationships]
cor(gun_2014_2017_data$n_affected,gun_2014_2017_data$drug_related,method = "pearson")
cor(gun_2014_2017_data$n_affected,gun_2014_2017_data$officer_related,method = "pearson")
cor(gun_2014_2017_data$n_affected,gun_2014_2017_data$multiple_involved,method = "pearson")
cor(gun_2014_2017_data$n_affected,gun_2014_2017_data$robbery_related,method = "pearson")
cor(gun_2014_2017_data$n_affected,gun_2014_2017_data$domestic_related,method = "pearson")
cor(gun_2014_2017_data$n_affected,gun_2014_2017_data$gang_related,method = "pearson")
cor(gun_2014_2017_data$n_affected,gun_2014_2017_data$school_related,method = "pearson")
cor(gun_2014_2017_data$led_to_death,gun_2014_2017_data$drug_related,method = "pearson")
cor(gun_2014_2017_data$led_to_death,gun_2014_2017_data$officer_related,method = "pearson")
cor(gun_2014_2017_data$led_to_death,gun_2014_2017_data$multiple_involved,method = "pearson")
cor(gun_2014_2017_data$led_to_death,gun_2014_2017_data$robbery_related,method = "pearson")
cor(gun_2014_2017_data$led_to_death,gun_2014_2017_data$domestic_related,method = "pearson")
cor(gun_2014_2017_data$led_to_death,gun_2014_2017_data$gang_related,method = "pearson")
cor(gun_2014_2017_data$led_to_death,gun_2014_2017_data$school_related,method = "pearson")
cor(gun_2014_2017_data$led_to_arrest,gun_2014_2017_data$drug_related,method = "pearson")
cor(gun_2014_2017_data$led_to_arrest,gun_2014_2017_data$officer_related,method = "pearson")
cor(gun_2014_2017_data$led_to_arrest,gun_2014_2017_data$multiple_involved,method = "pearson")
cor(gun_2014_2017_data$led_to_arrest,gun_2014_2017_data$robbery_related,method = "pearson")
cor(gun_2014_2017_data$led_to_arrest,gun_2014_2017_data$domestic_related,method = "pearson")
cor(gun_2014_2017_data$led_to_arrest,gun_2014_2017_data$gang_related,method = "pearson")
cor(gun_2014_2017_data$led_to_arrest,gun_2014_2017_data$school_related,method = "pearson")
#correlogram
subset_gun_14_17 <- subset(gun_2014_2017_data, select=c(n_affected,led_to_death,led_to_arrest,drug_related,officer_related,multiple_involved,robbery_related,domestic_related,gang_related,school_related))
c_gram_14_17 <- round(cor(subset_gun_14_17), 1)
# Plots the correlogram
ggcorrplot(
			c_gram_14_17, 
			hc.order = TRUE, 
            type = "lower", 
            lab = TRUE, 
            lab_size = 3, 
            method="circle", 
            colors = c("tomato2", "white", "springgreen3"), 
            title="Correlogram of Gun Violence Data", 
            ggtheme=theme_bw
            )
#group by state
gun_by_state <- gun_2014_2017_data %>% group_by(state) %>% summarise(
										n_killed = sum(n_killed),
										n_injured = sum(n_injured), 
										n_affected = n_killed + n_injured,
										alcohol_related = sum(alcohol_related),
										accidental = sum(accidental),
										drug_related = sum(drug_related),
										vehicular = sum(vehicular),
										officer_related = sum(officer_related),
										resulted_in_suicide = sum(resulted_in_suicide),
										multiple_involved = sum(multiple_involved),
										female_involved = sum(female_involved),
										male_involved = sum(male_involved),
										child_participant = sum(child_participant),
										teen_participant = sum(teen_participant),
										adult_participant = sum(adult_participant),
										led_to_arrest = sum(led_to_arrest),
										led_to_death = sum(led_to_death),
										led_to_injury = sum(led_to_injury),
										robbery_related = sum(robbery_related),
										domestic_related = sum(domestic_related),
										gang_related = sum(domestic_related),
										defense_related = sum(defense_related),
										school_related = sum(school_related)
										)
#correlations
cor(gun_by_state$n_affected,gun_by_state$drug_related,method = "pearson")
cor(gun_by_state$n_affected,gun_by_state$officer_related,method = "pearson")
cor(gun_by_state$n_affected,gun_by_state$multiple_involved,method = "pearson")
cor(gun_by_state$n_affected,gun_by_state$robbery_related,method = "pearson")
cor(gun_by_state$n_affected,gun_by_state$domestic_related,method = "pearson")
cor(gun_by_state$n_affected,gun_by_state$gang_related,method = "pearson")
cor(gun_by_state$n_affected,gun_by_state$school_related,method = "pearson")
cor(gun_by_state$led_to_death,gun_by_state$drug_related,method = "pearson")
cor(gun_by_state$led_to_death,gun_by_state$officer_related,method = "pearson")
cor(gun_by_state$led_to_death,gun_by_state$multiple_involved,method = "pearson")
cor(gun_by_state$led_to_death,gun_by_state$robbery_related,method = "pearson")
cor(gun_by_state$led_to_death,gun_by_state$domestic_related,method = "pearson")
cor(gun_by_state$led_to_death,gun_by_state$gang_related,method = "pearson")
cor(gun_by_state$led_to_death,gun_by_state$school_related,method = "pearson")
cor(gun_by_state$led_to_arrest,gun_by_state$drug_related,method = "pearson")
cor(gun_by_state$led_to_arrest,gun_by_state$officer_related,method = "pearson")
cor(gun_by_state$led_to_arrest,gun_by_state$multiple_involved,method = "pearson")
cor(gun_by_state$led_to_arrest,gun_by_state$robbery_related,method = "pearson")
cor(gun_by_state$led_to_arrest,gun_by_state$domestic_related,method = "pearson")
cor(gun_by_state$led_to_arrest,gun_by_state$gang_related,method = "pearson")
cor(gun_by_state$led_to_arrest,gun_by_state$school_related,method = "pearson")
#correlogram
subset_gun_state <- subset(gun_by_state, select=c(n_affected,led_to_death,led_to_arrest,drug_related,officer_related,multiple_involved,robbery_related,domestic_related,gang_related,school_related))
c_gram_state <- round(cor(subset_gun_state), 1)
# Plots the correlogram
ggcorrplot(
			c_gram_state, 
			hc.order = TRUE, 
            type = "lower", 
            lab = TRUE, 
            lab_size = 3, 
            method="circle", 
            colors = c("tomato2", "white", "springgreen3"), 
            title="Correlogram of Gun Violence Data by State", 
            ggtheme=theme_bw
            )
#get top 6 and bottom 6 states (Overall Killed, Injured and Affected)
gun_by_state <- gun_by_state[order(gun_by_state$n_affected),]
summary(gun_by_state)
least_affected_states <- head(gun_by_state)
#Male and Female Involment theme_set(theme_classic())
g_b_s_male_female <- least_affected_states %>%
  select(state, male_involved, female_involved) %>%
  gather(key = "variable", value = "value", -state)

ggplot(g_b_s_male_female, aes(x = state, y = value, fill=variable), xlab="State", ylab="Number of Incidents") + 
  geom_bar(stat="identity", width = .5, position="dodge")
#Incidents resulting in Suicide, Death, Arrest and Injury
g_b_s_result <- least_affected_states %>%
  select(state, resulted_in_suicide,led_to_death,led_to_arrest,led_to_injury) %>%
  gather(key = "variable", value = "value", -state)

ggplot(g_b_s_result, aes(x = state, y = value, fill=variable), xlab="State", ylab="Number of Incidents") + 
  geom_bar(stat="identity", width = .5, position="dodge")
#Incidents that were vehicular, robbery related, gang related, or drug related
g_b_s_related <- least_affected_states %>%
  select(state, vehicular,robbery_related, gang_related,drug_related) %>%
  gather(key = "variable", value = "value", -state)

ggplot(g_b_s_related, aes(x = state, y = value, fill=variable), xlab="State", ylab="Number of Incidents") + 
  geom_bar(stat="identity", width = .5, position="dodge")

#Incidents with multiple participants and those without
g_b_s_multiple <-  least_affected_states %>% mutate(
	                 state = state,
	                 multiple_involved = multiple_involved
	                 )

ggplot(g_b_s_multiple, aes(x=state, y=multiple_involved)) + 
  geom_point(size=3) + 
  geom_segment(aes(x=state, 
                   xend=state, 
                   y=0, 
                   yend=multiple_involved)) + 
  labs(title="Lollipop Chart", 
       subtitle="Incidents with Multiple participants per state"
       ) + 
  theme(axis.text.x = element_text(angle=65, vjust=0.6))

#Which age group did the participant(s) for gun related incidents belong to
g_b_s_age_group <- least_affected_states %>%
  select(state, child_participant,teen_participant,adult_participant) %>%
  gather(key = "variable", value = "value", -state)

ggplot(g_b_s_age_group, aes(x = state, y = value, fill=variable), xlab="State", ylab="Number of Incidents") + 
  geom_bar(stat="identity", width = .5, position="dodge")

#Incidents that were school related, domestic in nature, or self defense related
g_b_s_related <- least_affected_states %>%
  select(state, school_related, domestic_related, defense_related) %>%
  gather(key = "variable", value = "value", -state)

ggplot(g_b_s_related, aes(x = state, y = value, fill=variable), xlab="State", ylab="Number of Incidents") + 
  geom_bar(stat="identity", width = .5, position="dodge")

summary(least_affected_states)
#MOST AFFECTED STATES
most_affected_states <- tail(gun_by_state)
#Male and Female Involvement
g_b_s_male_female <- most_affected_states %>%
  select(state, male_involved, female_involved) %>%
  gather(key = "variable", value = "value", -state)

ggplot(g_b_s_male_female, aes(x = state, y = value, fill=variable), xlab="State", ylab="Number of Incidents") + 
  geom_bar(stat="identity", width = .5, position="dodge")
#Incidents resulting in Suicide, Death, Arrest and Injury
g_b_s_result <- most_affected_states %>%
  select(state, resulted_in_suicide,led_to_death,led_to_arrest,led_to_injury) %>%
  gather(key = "variable", value = "value", -state)

ggplot(g_b_s_result, aes(x = state, y = value, fill=variable), xlab="State", ylab="Number of Incidents") + 
  geom_bar(stat="identity", width = .5, position="dodge")
#Incidents that were vehicular, robbery related, gang related, or drug related
g_b_s_related <- most_affected_states %>%
  select(state, vehicular,robbery_related, gang_related,drug_related) %>%
  gather(key = "variable", value = "value", -state)

ggplot(g_b_s_related, aes(x = state, y = value, fill=variable), xlab="State", ylab="Number of Incidents") + 
  geom_bar(stat="identity", width = .5, position="dodge")

#Incidents with multiple participants and those without
g_b_s_multiple <-  most_affected_states %>% mutate(
	                 state = state,
	                 multiple_involved = multiple_involved
	                 )

ggplot(g_b_s_multiple, aes(x=state, y=multiple_involved)) + 
  geom_point(size=3) + 
  geom_segment(aes(x=state, 
                   xend=state, 
                   y=0, 
                   yend=multiple_involved)) + 
  labs(title="Lollipop Chart", 
       subtitle="Incidents with Multiple participants per state"
       ) + 
  theme(axis.text.x = element_text(angle=65, vjust=0.6))

#Which age group did the participant(s) for gun related incidents belong to
g_b_s_age_group <- most_affected_states %>%
  select(state, child_participant,teen_participant,adult_participant) %>%
  gather(key = "variable", value = "value", -state)

ggplot(g_b_s_age_group, aes(x = state, y = value, fill=variable), xlab="State", ylab="Number of Incidents") + 
  geom_bar(stat="identity", width = .5, position="dodge")

#Incidents that were school related, domestic in nature, or self defense related
g_b_s_related <- most_affected_states %>%
  select(state, school_related, domestic_related, defense_related) %>%
  gather(key = "variable", value = "value", -state)

ggplot(g_b_s_related, aes(x = state, y = value, fill=variable), xlab="State", ylab="Number of Incidents") + 
  geom_bar(stat="identity", width = .5, position="dodge")

#Incidents that were school related, domestic in nature, or self defense related
summary(most_affected_states)
#Most Affected States Statistics
illinois_time_gv <- gun_2014_2017_data[grepl("Illinois",gun_2014_2017_data$state),]
illinois_gv <- illinois_time_gv %>% group_by(date) %>% mutate(
										n_killed = sum(n_killed),
										n_injured = sum(n_injured), 
										n_affected = n_killed + n_injured,
										alcohol_related = sum(alcohol_related),
										accidental = sum(accidental),
										drug_related = sum(drug_related),
										vehicular = sum(vehicular),
										officer_related = sum(officer_related),
										resulted_in_suicide = sum(resulted_in_suicide),
										multiple_involved = sum(multiple_involved),
										female_involved = sum(female_involved),
										male_involved = sum(male_involved),
										child_participant = sum(child_participant),
										teen_participant = sum(teen_participant),
										adult_participant = sum(adult_participant),
										led_to_arrest = sum(led_to_arrest),
										led_to_death = sum(led_to_death),
										led_to_injury = sum(led_to_injury),
										robbery_related = sum(robbery_related),
										domestic_related = sum(domestic_related),
										gang_related = sum(domestic_related),
										defense_related = sum(defense_related),
										school_related = sum(school_related),
										year = case_when(
												grepl("^2014",date) ~ 2014,
												grepl("^2015",date) ~ 2015,
												grepl("^2016",date) ~ 2016,
												grepl("^2017",date) ~ 2017,
												TRUE ~ 12
											)
										)
# Animated Bubble Chart Persons Killed and Affected
ani_bubble_chart <- ggplot(illinois_gv, aes(n_killed, n_affected, size = n_injured, frame = year)) +
  geom_point() +
  geom_smooth(aes(group = year), 
              method = "lm", 
              show.legend = FALSE) +
  scale_x_log10()

gganimate(ani_bubble_chart)

#Illinois N_affected, N_killed, N_injured Time Series
n_affected_over_time <- illinois_gv %>%
  select(date, n_killed, n_injured, n_affected) %>%
  gather(key = "variable", value = "value", -date)

ggplot(n_affected_over_time, aes(x = date, y = value)) + 
  geom_line(aes(color = variable), size = 1) +
  scale_color_manual(values = c("#00AFBB", "#E7B800", "#FC4E07")) +
  theme_minimal()

#Incidents resulting in Suicide, Death, Arrest and Injury
g_b_s_result <- illinois_time_gv %>% group_by(city_or_county) %>% 
   summarise(
   	 resulted_in_suicide = sum(resulted_in_suicide),
   	 led_to_death = sum(led_to_death),
   	 led_to_arrest = sum(led_to_arrest),
   	 led_to_injury = sum(led_to_injury)
   	 )
#Want to find the most complete data for graph visualization purposes
g_b_s_zero_values <- g_b_s_result[g_b_s_result$led_to_death == 0 | g_b_s_result$led_to_injury ==0 |g_b_s_result$led_to_arrest ==0,]
g_b_s_result <- anti_join(g_b_s_result,g_b_s_zero_values)
g_b_s_result <- g_b_s_result[order(g_b_s_result$led_to_death),]
g_b_s_result <- tail(g_b_s_result)
g_b_s_result <- g_b_s_result%>%
  select(city_or_county, resulted_in_suicide,led_to_death,led_to_arrest,led_to_injury) %>%
  gather(key = "variable", value = "value", -city_or_county)

ggplot(g_b_s_result, aes(x = city_or_county, y = value, fill=variable), xlab="State", ylab="Number of Incidents") + 
  geom_bar(stat="identity", width = .5, position="dodge")

#Incidents that were vehicular, robbery related, gang related, or drug related
g_b_s_related <- illinois_time_gv %>% group_by(city_or_county) %>% 
   summarise(
     vehicular = sum(vehicular),
     robbery_related = sum(robbery_related),
     gang_related = sum(gang_related),
     drug_related = sum(drug_related)
     )
#Want to find the most complete data for graph visualization purposes
g_b_s_related_zero_values <- g_b_s_related[g_b_s_related$vehicular== 0 | g_b_s_related$robbery_related ==0 | g_b_s_related$drug_related == 0 | g_b_s_related$gang_related == 0,] 
g_b_s_related <- anti_join(g_b_s_related,g_b_s_related_zero_values)
g_b_s_related <- g_b_s_related[order(g_b_s_related$gang_related),]
g_b_s_related <- tail(g_b_s_related)
g_b_s_related <- g_b_s_related %>%
  select(city_or_county, vehicular,robbery_related,gang_related,drug_related) %>%
  gather(key = "variable", value = "value", -city_or_county)

ggplot(g_b_s_related, aes(x = city_or_county, y = value, fill=variable), xlab="State", ylab="Number of Incidents") + 
  geom_bar(stat="identity", width = .5, position="dodge")

#Incidents with multiple participants and those without
g_b_s_multiple <- illinois_time_gv %>% group_by(city_or_county) %>% 
   summarise(
     multiple_involved = sum(multiple_involved)
     )
g_b_s_multiple <- g_b_s_multiple[order(g_b_s_multiple$multiple_involved),]
g_b_s_multiple <- tail(g_b_s_multiple)
g_b_s_multiple <- g_b_s_multiple[order(g_b_s_multiple$multiple_involved)]
ggplot(g_b_s_multiple, aes(x=city_or_county, y=multiple_involved)) + 
  geom_point(size=3) + 
  geom_segment(aes(x=city_or_county, 
                   xend=city_or_county, 
                   y=0, 
                   yend=multiple_involved)) + 
  labs(title="Lollipop Chart", 
       subtitle="Incidents with Multiple participants per City/County"
       ) + 
  theme(axis.text.x = element_text(angle=65, vjust=0.6))

#Which age group did the participant(s) for gun related incidents belong to
g_b_s_age_group <- illinois_time_gv %>% group_by(city_or_county) %>% 
   summarise(
     child_participant = sum(child_participant),
     teen_participant = sum(teen_participant),
     adult_participant = sum(adult_participant)
     )
g_b_s_zero_values <- g_b_s_age_group[g_b_s_age_group$child_participant == 0 | g_b_s_age_group$teen_participant == 0 | g_b_s_age_group$adult_participant == 0 ,]
g_b_s_age_group <- anti_join(g_b_s_age_group,g_b_s_zero_values)
g_b_s_age_group <- g_b_s_age_group[order(g_b_s_age_group$child_participant),]
g_b_s_age_group <- tail(g_b_s_age_group)
g_b_s_age_group <- g_b_s_age_group %>%
  select(city_or_county, child_participant,teen_participant) %>%
  gather(key = "variable", value = "value", -city_or_county)

ggplot(g_b_s_age_group, aes(x = city_or_county, y = value, fill=variable), xlab="State", ylab="Number of Incidents") + 
  geom_bar(stat="identity", width = .5, position="dodge")
#Incidents that were school related, domestic in nature, or self defense related
g_b_s_related <- illinois_time_gv %>% group_by(city_or_county) %>% 
   summarise(
     school_related = sum(school_related),
     domestic_related = sum(domestic_related),
     defense_related = sum(defense_related)
     )
g_b_s_zero_values <- g_b_s_related[g_b_s_related$school_related == 0 | g_b_s_related$domestic_related == 0| g_b_s_related$defense_related == 0,]
g_b_s_related <- anti_join(g_b_s_related, g_b_s_zero_values)
g_b_s_related <- g_b_s_related[order(g_b_s_related$defense_related),]
g_b_s_related <- tail(g_b_s_related)
g_b_s_related <- g_b_s_related %>%
  select(city_or_county, school_related, domestic_related, defense_related) %>%
  gather(key = "variable", value = "value", -city_or_county)

ggplot(g_b_s_related, aes(x = city_or_county, y = value, fill=variable), xlab="State", ylab="Number of Incidents") + 
  geom_bar(stat="identity", width = .5, position="dodge")

cali_time_gv <- gun_2014_2017_data[grepl("California",gun_2014_2017_data$state),]
cali_gv <- cali_time_gv %>% group_by(date) %>% mutate(
                    n_killed = sum(n_killed),
                    n_injured = sum(n_injured), 
                    n_affected = n_killed + n_injured,
                    alcohol_related = sum(alcohol_related),
                    accidental = sum(accidental),
                    drug_related = sum(drug_related),
                    vehicular = sum(vehicular),
                    officer_related = sum(officer_related),
                    resulted_in_suicide = sum(resulted_in_suicide),
                    multiple_involved = sum(multiple_involved),
                    female_involved = sum(female_involved),
                    male_involved = sum(male_involved),
                    child_participant = sum(child_participant),
                    teen_participant = sum(teen_participant),
                    adult_participant = sum(adult_participant),
                    led_to_arrest = sum(led_to_arrest),
                    led_to_death = sum(led_to_death),
                    led_to_injury = sum(led_to_injury),
                    robbery_related = sum(robbery_related),
                    domestic_related = sum(domestic_related),
                    gang_related = sum(domestic_related),
                    defense_related = sum(defense_related),
                    school_related = sum(school_related),
                    year = case_when(
                        grepl("^2014",date) ~ 2014,
                        grepl("^2015",date) ~ 2015,
                        grepl("^2016",date) ~ 2016,
                        grepl("^2017",date) ~ 2017,
                        TRUE ~ 12
                      )
                    )
# Animated Bubble Chart Persons Killed and Affected
ani_bubble_chart <- ggplot(cali_gv, aes(n_killed, n_affected, size = n_injured, frame = year)) +
  geom_point() +
  geom_smooth(aes(group = year), 
              method = "lm", 
              show.legend = FALSE) +
  scale_x_log10()

gganimate(ani_bubble_chart)

#cali N_affected, N_killed, N_injured Time Series
n_affected_over_time <- cali_gv %>%
  select(date, n_killed, n_injured, n_affected) %>%
  gather(key = "variable", value = "value", -date)

ggplot(n_affected_over_time, aes(x = date, y = value)) + 
  geom_line(aes(color = variable), size = 1) +
  scale_color_manual(values = c("#00AFBB", "#E7B800", "#FC4E07")) +
  theme_minimal()

#Incidents resulting in Suicide, Death, Arrest and Injury
g_b_s_result <- cali_time_gv %>% group_by(city_or_county) %>% 
   summarise(
     resulted_in_suicide = sum(resulted_in_suicide),
     led_to_death = sum(led_to_death),
     led_to_arrest = sum(led_to_arrest),
     led_to_injury = sum(led_to_injury)
     )
#Want to find the most complete data for graph visualization purposes
g_b_s_zero_values <- g_b_s_result[g_b_s_result$led_to_death == 0 | g_b_s_result$led_to_injury ==0 |g_b_s_result$led_to_arrest ==0,]
g_b_s_result <- anti_join(g_b_s_result,g_b_s_zero_values)
g_b_s_result <- g_b_s_result[order(g_b_s_result$led_to_death),]
g_b_s_result <- tail(g_b_s_result)
g_b_s_result <- g_b_s_result%>%
  select(city_or_county, resulted_in_suicide,led_to_death,led_to_arrest,led_to_injury) %>%
  gather(key = "variable", value = "value", -city_or_county)

ggplot(g_b_s_result, aes(x = city_or_county, y = value, fill=variable), xlab="State", ylab="Number of Incidents") + 
  geom_bar(stat="identity", width = .5, position="dodge")

#Incidents that were vehicular, robbery related, gang related, or drug related
g_b_s_related <- cali_time_gv %>% group_by(city_or_county) %>% 
   summarise(
     vehicular = sum(vehicular),
     robbery_related = sum(robbery_related),
     gang_related = sum(gang_related),
     drug_related = sum(drug_related)
     )
#Want to find the most complete data for graph visualization purposes
g_b_s_related_zero_values <- g_b_s_related[g_b_s_related$vehicular== 0 | g_b_s_related$robbery_related ==0 | g_b_s_related$drug_related == 0 | g_b_s_related$gang_related == 0,] 
g_b_s_related <- anti_join(g_b_s_related,g_b_s_related_zero_values)
g_b_s_related <- g_b_s_related[order(g_b_s_related$gang_related),]
g_b_s_related <- tail(g_b_s_related)
g_b_s_related <- g_b_s_related %>%
  select(city_or_county, vehicular,robbery_related,gang_related,drug_related) %>%
  gather(key = "variable", value = "value", -city_or_county)

ggplot(g_b_s_related, aes(x = city_or_county, y = value, fill=variable), xlab="State", ylab="Number of Incidents") + 
  geom_bar(stat="identity", width = .5, position="dodge")

#Incidents with multiple participants and those without
g_b_s_multiple <- cali_time_gv %>% group_by(city_or_county) %>% 
   summarise(
     multiple_involved = sum(multiple_involved)
     )
g_b_s_multiple <- g_b_s_multiple[order(g_b_s_multiple$multiple_involved),]
g_b_s_multiple <- tail(g_b_s_multiple)
g_b_s_multiple <- g_b_s_multiple[order(g_b_s_multiple$multiple_involved)]
ggplot(g_b_s_multiple, aes(x=city_or_county, y=multiple_involved)) + 
  geom_point(size=3) + 
  geom_segment(aes(x=city_or_county, 
                   xend=city_or_county, 
                   y=0, 
                   yend=multiple_involved)) + 
  labs(title="Lollipop Chart", 
       subtitle="Incidents with Multiple participants per City/County"
       ) + 
  theme(axis.text.x = element_text(angle=65, vjust=0.6))

#Which age group did the participant(s) for gun related incidents belong to
g_b_s_age_group <- cali_time_gv %>% group_by(city_or_county) %>% 
   summarise(
     child_participant = sum(child_participant),
     teen_participant = sum(teen_participant),
     adult_participant = sum(adult_participant)
     )
g_b_s_zero_values <- g_b_s_age_group[g_b_s_age_group$child_participant == 0 | g_b_s_age_group$teen_participant == 0 | g_b_s_age_group$adult_participant == 0 ,]
g_b_s_age_group <- anti_join(g_b_s_age_group,g_b_s_zero_values)
g_b_s_age_group <- g_b_s_age_group[order(g_b_s_age_group$child_participant),]
g_b_s_age_group <- tail(g_b_s_age_group)
g_b_s_age_group <- g_b_s_age_group %>%
  select(city_or_county, child_participant,teen_participant) %>%
  gather(key = "variable", value = "value", -city_or_county)

ggplot(g_b_s_age_group, aes(x = city_or_county, y = value, fill=variable), xlab="State", ylab="Number of Incidents") + 
  geom_bar(stat="identity", width = .5, position="dodge")
#Incidents that were school related, domestic in nature, or self defense related
g_b_s_related <- cali_time_gv %>% group_by(city_or_county) %>% 
   summarise(
     school_related = sum(school_related),
     domestic_related = sum(domestic_related),
     defense_related = sum(defense_related)
     )
g_b_s_zero_values <- g_b_s_related[g_b_s_related$school_related == 0 | g_b_s_related$domestic_related == 0| g_b_s_related$defense_related == 0,]
g_b_s_related <- anti_join(g_b_s_related, g_b_s_zero_values)
g_b_s_related <- g_b_s_related[order(g_b_s_related$defense_related),]
g_b_s_related <- tail(g_b_s_related)
g_b_s_related <- g_b_s_related %>%
  select(city_or_county, school_related, domestic_related, defense_related) %>%
  gather(key = "variable", value = "value", -city_or_county)

ggplot(g_b_s_related, aes(x = city_or_county, y = value, fill=variable), xlab="State", ylab="Number of Incidents") + 
  geom_bar(stat="identity", width = .5, position="dodge")

texas_time_gv <- gun_2014_2017_data[grepl("Texas",gun_2014_2017_data$state),]
texas_gv <- texas_time_gv %>% group_by(date) %>% mutate(
                    n_killed = sum(n_killed),
                    n_injured = sum(n_injured), 
                    n_affected = n_killed + n_injured,
                    alcohol_related = sum(alcohol_related),
                    accidental = sum(accidental),
                    drug_related = sum(drug_related),
                    vehicular = sum(vehicular),
                    officer_related = sum(officer_related),
                    resulted_in_suicide = sum(resulted_in_suicide),
                    multiple_involved = sum(multiple_involved),
                    female_involved = sum(female_involved),
                    male_involved = sum(male_involved),
                    child_participant = sum(child_participant),
                    teen_participant = sum(teen_participant),
                    adult_participant = sum(adult_participant),
                    led_to_arrest = sum(led_to_arrest),
                    led_to_death = sum(led_to_death),
                    led_to_injury = sum(led_to_injury),
                    robbery_related = sum(robbery_related),
                    domestic_related = sum(domestic_related),
                    gang_related = sum(domestic_related),
                    defense_related = sum(defense_related),
                    school_related = sum(school_related),
                    year = case_when(
                        grepl("^2014",date) ~ 2014,
                        grepl("^2015",date) ~ 2015,
                        grepl("^2016",date) ~ 2016,
                        grepl("^2017",date) ~ 2017,
                        TRUE ~ 12
                      )
                    )
# Animated Bubble Chart Persons Killed and Affected
ani_bubble_chart <- ggplot(texas_gv, aes(n_killed, n_affected, size = n_injured, frame = year)) +
  geom_point() +
  geom_smooth(aes(group = year), 
              method = "lm", 
              show.legend = FALSE) +
  scale_x_log10()

gganimate(ani_bubble_chart)

#texas N_affected, N_killed, N_injured Time Series
n_affected_over_time <- texas_gv %>%
  select(date, n_killed, n_injured, n_affected) %>%
  gather(key = "variable", value = "value", -date)

ggplot(n_affected_over_time, aes(x = date, y = value)) + 
  geom_line(aes(color = variable), size = 1) +
  scale_color_manual(values = c("#00AFBB", "#E7B800", "#FC4E07")) +
  theme_minimal()

#Incidents resulting in Suicide, Death, Arrest and Injury
g_b_s_result <- texas_time_gv %>% group_by(city_or_county) %>% 
   summarise(
     resulted_in_suicide = sum(resulted_in_suicide),
     led_to_death = sum(led_to_death),
     led_to_arrest = sum(led_to_arrest),
     led_to_injury = sum(led_to_injury)
     )
#Want to find the most complete data for graph visualization purposes
g_b_s_zero_values <- g_b_s_result[g_b_s_result$led_to_death == 0 | g_b_s_result$led_to_injury ==0 |g_b_s_result$led_to_arrest ==0,]
g_b_s_result <- anti_join(g_b_s_result,g_b_s_zero_values)
g_b_s_result <- g_b_s_result[order(g_b_s_result$led_to_death),]
g_b_s_result <- tail(g_b_s_result)
g_b_s_result <- g_b_s_result%>%
  select(city_or_county, resulted_in_suicide,led_to_death,led_to_arrest,led_to_injury) %>%
  gather(key = "variable", value = "value", -city_or_county)

ggplot(g_b_s_result, aes(x = city_or_county, y = value, fill=variable), xlab="State", ylab="Number of Incidents") + 
  geom_bar(stat="identity", width = .5, position="dodge")

#Incidents that were vehicular, robbery related, gang related, or drug related
g_b_s_related <- texas_time_gv %>% group_by(city_or_county) %>% 
   summarise(
     vehicular = sum(vehicular),
     robbery_related = sum(robbery_related),
     gang_related = sum(gang_related),
     drug_related = sum(drug_related)
     )
#Want to find the most complete data for graph visualization purposes
g_b_s_related_zero_values <- g_b_s_related[g_b_s_related$vehicular== 0 | g_b_s_related$robbery_related ==0 | g_b_s_related$drug_related == 0 | g_b_s_related$gang_related == 0,] 
g_b_s_related <- anti_join(g_b_s_related,g_b_s_related_zero_values)
g_b_s_related <- g_b_s_related[order(g_b_s_related$gang_related),]
g_b_s_related <- tail(g_b_s_related)
g_b_s_related <- g_b_s_related %>%
  select(city_or_county, vehicular,robbery_related,gang_related,drug_related) %>%
  gather(key = "variable", value = "value", -city_or_county)

ggplot(g_b_s_related, aes(x = city_or_county, y = value, fill=variable), xlab="State", ylab="Number of Incidents") + 
  geom_bar(stat="identity", width = .5, position="dodge")

#Incidents with multiple participants and those without
g_b_s_multiple <- texas_time_gv %>% group_by(city_or_county) %>% 
   summarise(
     multiple_involved = sum(multiple_involved)
     )
g_b_s_multiple <- g_b_s_multiple[order(g_b_s_multiple$multiple_involved),]
g_b_s_multiple <- tail(g_b_s_multiple)
g_b_s_multiple <- g_b_s_multiple[order(g_b_s_multiple$multiple_involved)]
ggplot(g_b_s_multiple, aes(x=city_or_county, y=multiple_involved)) + 
  geom_point(size=3) + 
  geom_segment(aes(x=city_or_county, 
                   xend=city_or_county, 
                   y=0, 
                   yend=multiple_involved)) + 
  labs(title="Lollipop Chart", 
       subtitle="Incidents with Multiple participants per City/County"
       ) + 
  theme(axis.text.x = element_text(angle=65, vjust=0.6))

#Which age group did the participant(s) for gun related incidents belong to
g_b_s_age_group <- texas_time_gv %>% group_by(city_or_county) %>% 
   summarise(
     child_participant = sum(child_participant),
     teen_participant = sum(teen_participant),
     adult_participant = sum(adult_participant)
     )
g_b_s_zero_values <- g_b_s_age_group[g_b_s_age_group$child_participant == 0 | g_b_s_age_group$teen_participant == 0 | g_b_s_age_group$adult_participant == 0 ,]
g_b_s_age_group <- anti_join(g_b_s_age_group,g_b_s_zero_values)
g_b_s_age_group <- g_b_s_age_group[order(g_b_s_age_group$child_participant),]
g_b_s_age_group <- tail(g_b_s_age_group)
g_b_s_age_group <- g_b_s_age_group %>%
  select(city_or_county, child_participant,teen_participant) %>%
  gather(key = "variable", value = "value", -city_or_county)

ggplot(g_b_s_age_group, aes(x = city_or_county, y = value, fill=variable), xlab="State", ylab="Number of Incidents") + 
  geom_bar(stat="identity", width = .5, position="dodge")
#Incidents that were school related, domestic in nature, or self defense related
g_b_s_related <- texas_time_gv %>% group_by(city_or_county) %>% 
   summarise(
     school_related = sum(school_related),
     domestic_related = sum(domestic_related),
     defense_related = sum(defense_related)
     )
g_b_s_zero_values <- g_b_s_related[g_b_s_related$school_related == 0 | g_b_s_related$domestic_related == 0| g_b_s_related$defense_related == 0,]
g_b_s_related <- anti_join(g_b_s_related, g_b_s_zero_values)
g_b_s_related <- g_b_s_related[order(g_b_s_related$defense_related),]
g_b_s_related <- tail(g_b_s_related)
g_b_s_related <- g_b_s_related %>%
  select(city_or_county, school_related, domestic_related, defense_related) %>%
  gather(key = "variable", value = "value", -city_or_county)

ggplot(g_b_s_related, aes(x = city_or_county, y = value, fill=variable), xlab="State", ylab="Number of Incidents") + 
  geom_bar(stat="identity", width = .5, position="dodge")

florida_time_gv <- gun_2014_2017_data[grepl("Florida",gun_2014_2017_data$state),]
florida_gv <- florida_time_gv %>% group_by(date) %>% mutate(
                    n_killed = sum(n_killed),
                    n_injured = sum(n_injured), 
                    n_affected = n_killed + n_injured,
                    alcohol_related = sum(alcohol_related),
                    accidental = sum(accidental),
                    drug_related = sum(drug_related),
                    vehicular = sum(vehicular),
                    officer_related = sum(officer_related),
                    resulted_in_suicide = sum(resulted_in_suicide),
                    multiple_involved = sum(multiple_involved),
                    female_involved = sum(female_involved),
                    male_involved = sum(male_involved),
                    child_participant = sum(child_participant),
                    teen_participant = sum(teen_participant),
                    adult_participant = sum(adult_participant),
                    led_to_arrest = sum(led_to_arrest),
                    led_to_death = sum(led_to_death),
                    led_to_injury = sum(led_to_injury),
                    robbery_related = sum(robbery_related),
                    domestic_related = sum(domestic_related),
                    gang_related = sum(domestic_related),
                    defense_related = sum(defense_related),
                    school_related = sum(school_related),
                    year = case_when(
                        grepl("^2014",date) ~ 2014,
                        grepl("^2015",date) ~ 2015,
                        grepl("^2016",date) ~ 2016,
                        grepl("^2017",date) ~ 2017,
                        TRUE ~ 12
                      )
                    )
# Animated Bubble Chart Persons Killed and Affected
ani_bubble_chart <- ggplot(florida_gv, aes(n_killed, n_affected, size = n_injured, frame = year)) +
  geom_point() +
  geom_smooth(aes(group = year), 
              method = "lm", 
              show.legend = FALSE) +
  scale_x_log10()

gganimate(ani_bubble_chart)

#florida N_affected, N_killed, N_injured Time Series
n_affected_over_time <- florida_gv %>%
  select(date, n_killed, n_injured, n_affected) %>%
  gather(key = "variable", value = "value", -date)

ggplot(n_affected_over_time, aes(x = date, y = value)) + 
  geom_line(aes(color = variable), size = 1) +
  scale_color_manual(values = c("#00AFBB", "#E7B800", "#FC4E07")) +
  theme_minimal()

#Incidents resulting in Suicide, Death, Arrest and Injury
g_b_s_result <- florida_time_gv %>% group_by(city_or_county) %>% 
   summarise(
     resulted_in_suicide = sum(resulted_in_suicide),
     led_to_death = sum(led_to_death),
     led_to_arrest = sum(led_to_arrest),
     led_to_injury = sum(led_to_injury)
     )
#Want to find the most complete data for graph visualization purposes
g_b_s_zero_values <- g_b_s_result[g_b_s_result$led_to_death == 0 | g_b_s_result$led_to_injury ==0 |g_b_s_result$led_to_arrest ==0,]
g_b_s_result <- anti_join(g_b_s_result,g_b_s_zero_values)
g_b_s_result <- g_b_s_result[order(g_b_s_result$led_to_death),]
g_b_s_result <- tail(g_b_s_result)
g_b_s_result <- g_b_s_result%>%
  select(city_or_county, resulted_in_suicide,led_to_death,led_to_arrest,led_to_injury) %>%
  gather(key = "variable", value = "value", -city_or_county)

ggplot(g_b_s_result, aes(x = city_or_county, y = value, fill=variable), xlab="State", ylab="Number of Incidents") + 
  geom_bar(stat="identity", width = .5, position="dodge")

#Incidents that were vehicular, robbery related, gang related, or drug related
g_b_s_related <- florida_time_gv %>% group_by(city_or_county) %>% 
   summarise(
     vehicular = sum(vehicular),
     robbery_related = sum(robbery_related),
     gang_related = sum(gang_related),
     drug_related = sum(drug_related)
     )
#Want to find the most complete data for graph visualization purposes
g_b_s_related_zero_values <- g_b_s_related[g_b_s_related$vehicular== 0 | g_b_s_related$robbery_related ==0 | g_b_s_related$drug_related == 0 | g_b_s_related$gang_related == 0,] 
g_b_s_related <- anti_join(g_b_s_related,g_b_s_related_zero_values)
g_b_s_related <- g_b_s_related[order(g_b_s_related$gang_related),]
g_b_s_related <- tail(g_b_s_related)
g_b_s_related <- g_b_s_related %>%
  select(city_or_county, vehicular,robbery_related,gang_related,drug_related) %>%
  gather(key = "variable", value = "value", -city_or_county)

ggplot(g_b_s_related, aes(x = city_or_county, y = value, fill=variable), xlab="State", ylab="Number of Incidents") + 
  geom_bar(stat="identity", width = .5, position="dodge")

#Incidents with multiple participants and those without
g_b_s_multiple <- florida_time_gv %>% group_by(city_or_county) %>% 
   summarise(
     multiple_involved = sum(multiple_involved)
     )
g_b_s_multiple <- g_b_s_multiple[order(g_b_s_multiple$multiple_involved),]
g_b_s_multiple <- tail(g_b_s_multiple)
g_b_s_multiple <- g_b_s_multiple[order(g_b_s_multiple$multiple_involved)]
ggplot(g_b_s_multiple, aes(x=city_or_county, y=multiple_involved)) + 
  geom_point(size=3) + 
  geom_segment(aes(x=city_or_county, 
                   xend=city_or_county, 
                   y=0, 
                   yend=multiple_involved)) + 
  labs(title="Lollipop Chart", 
       subtitle="Incidents with Multiple participants per City/County"
       ) + 
  theme(axis.text.x = element_text(angle=65, vjust=0.6))

#Which age group did the participant(s) for gun related incidents belong to
g_b_s_age_group <- florida_time_gv %>% group_by(city_or_county) %>% 
   summarise(
     child_participant = sum(child_participant),
     teen_participant = sum(teen_participant),
     adult_participant = sum(adult_participant)
     )
g_b_s_zero_values <- g_b_s_age_group[g_b_s_age_group$child_participant == 0 | g_b_s_age_group$teen_participant == 0 | g_b_s_age_group$adult_participant == 0 ,]
g_b_s_age_group <- anti_join(g_b_s_age_group,g_b_s_zero_values)
g_b_s_age_group <- g_b_s_age_group[order(g_b_s_age_group$child_participant),]
g_b_s_age_group <- tail(g_b_s_age_group)
g_b_s_age_group <- g_b_s_age_group %>%
  select(city_or_county, child_participant,teen_participant) %>%
  gather(key = "variable", value = "value", -city_or_county)

ggplot(g_b_s_age_group, aes(x = city_or_county, y = value, fill=variable), xlab="State", ylab="Number of Incidents") + 
  geom_bar(stat="identity", width = .5, position="dodge")
#Incidents that were school related, domestic in nature, or self defense related
g_b_s_related <- florida_time_gv %>% group_by(city_or_county) %>% 
   summarise(
     school_related = sum(school_related),
     domestic_related = sum(domestic_related),
     defense_related = sum(defense_related)
     )
g_b_s_zero_values <- g_b_s_related[g_b_s_related$school_related == 0 | g_b_s_related$domestic_related == 0| g_b_s_related$defense_related == 0,]
g_b_s_related <- anti_join(g_b_s_related, g_b_s_zero_values)
g_b_s_related <- g_b_s_related[order(g_b_s_related$defense_related),]
g_b_s_related <- tail(g_b_s_related)
g_b_s_related <- g_b_s_related %>%
  select(city_or_county, school_related, domestic_related, defense_related) %>%
  gather(key = "variable", value = "value", -city_or_county)

ggplot(g_b_s_related, aes(x = city_or_county, y = value, fill=variable), xlab="State", ylab="Number of Incidents") + 
  geom_bar(stat="identity", width = .5, position="dodge")

ohio_time_gv <- gun_2014_2017_data[grepl("Ohio",gun_2014_2017_data$state),]
ohio_gv <- ohio_time_gv %>% group_by(date) %>% mutate(
                    n_killed = sum(n_killed),
                    n_injured = sum(n_injured), 
                    n_affected = n_killed + n_injured,
                    alcohol_related = sum(alcohol_related),
                    accidental = sum(accidental),
                    drug_related = sum(drug_related),
                    vehicular = sum(vehicular),
                    officer_related = sum(officer_related),
                    resulted_in_suicide = sum(resulted_in_suicide),
                    multiple_involved = sum(multiple_involved),
                    female_involved = sum(female_involved),
                    male_involved = sum(male_involved),
                    child_participant = sum(child_participant),
                    teen_participant = sum(teen_participant),
                    adult_participant = sum(adult_participant),
                    led_to_arrest = sum(led_to_arrest),
                    led_to_death = sum(led_to_death),
                    led_to_injury = sum(led_to_injury),
                    robbery_related = sum(robbery_related),
                    domestic_related = sum(domestic_related),
                    gang_related = sum(domestic_related),
                    defense_related = sum(defense_related),
                    school_related = sum(school_related),
                    year = case_when(
                        grepl("^2014",date) ~ 2014,
                        grepl("^2015",date) ~ 2015,
                        grepl("^2016",date) ~ 2016,
                        grepl("^2017",date) ~ 2017,
                        TRUE ~ 12
                      )
                    )
# Animated Bubble Chart Persons Killed and Affected
ani_bubble_chart <- ggplot(ohio_gv, aes(n_killed, n_affected, size = n_injured, frame = year)) +
  geom_point() +
  geom_smooth(aes(group = year), 
              method = "lm", 
              show.legend = FALSE) +
  scale_x_log10()

gganimate(ani_bubble_chart)

#ohio N_affected, N_killed, N_injured Time Series
n_affected_over_time <- ohio_gv %>%
  select(date, n_killed, n_injured, n_affected) %>%
  gather(key = "variable", value = "value", -date)

ggplot(n_affected_over_time, aes(x = date, y = value)) + 
  geom_line(aes(color = variable), size = 1) +
  scale_color_manual(values = c("#00AFBB", "#E7B800", "#FC4E07")) +
  theme_minimal()

#Incidents resulting in Suicide, Death, Arrest and Injury
g_b_s_result <- ohio_time_gv %>% group_by(city_or_county) %>% 
   summarise(
     resulted_in_suicide = sum(resulted_in_suicide),
     led_to_death = sum(led_to_death),
     led_to_arrest = sum(led_to_arrest),
     led_to_injury = sum(led_to_injury)
     )
#Want to find the most complete data for graph visualization purposes
g_b_s_zero_values <- g_b_s_result[g_b_s_result$led_to_death == 0 | g_b_s_result$led_to_injury ==0 |g_b_s_result$led_to_arrest ==0,]
g_b_s_result <- anti_join(g_b_s_result,g_b_s_zero_values)
g_b_s_result <- g_b_s_result[order(g_b_s_result$led_to_death),]
g_b_s_result <- tail(g_b_s_result)
g_b_s_result <- g_b_s_result%>%
  select(city_or_county, resulted_in_suicide,led_to_death,led_to_arrest,led_to_injury) %>%
  gather(key = "variable", value = "value", -city_or_county)

ggplot(g_b_s_result, aes(x = city_or_county, y = value, fill=variable), xlab="State", ylab="Number of Incidents") + 
  geom_bar(stat="identity", width = .5, position="dodge")

#Incidents that were vehicular, robbery related, gang related, or drug related
g_b_s_related <- ohio_time_gv %>% group_by(city_or_county) %>% 
   summarise(
     vehicular = sum(vehicular),
     robbery_related = sum(robbery_related),
     gang_related = sum(gang_related),
     drug_related = sum(drug_related)
     )
#Want to find the most complete data for graph visualization purposes
g_b_s_related_zero_values <- g_b_s_related[g_b_s_related$vehicular== 0 | g_b_s_related$robbery_related ==0 | g_b_s_related$drug_related == 0 | g_b_s_related$gang_related == 0,] 
g_b_s_related <- anti_join(g_b_s_related,g_b_s_related_zero_values)
g_b_s_related <- g_b_s_related[order(g_b_s_related$gang_related),]
g_b_s_related <- tail(g_b_s_related)
g_b_s_related <- g_b_s_related %>%
  select(city_or_county, vehicular,robbery_related,gang_related,drug_related) %>%
  gather(key = "variable", value = "value", -city_or_county)

ggplot(g_b_s_related, aes(x = city_or_county, y = value, fill=variable), xlab="State", ylab="Number of Incidents") + 
  geom_bar(stat="identity", width = .5, position="dodge")

#Incidents with multiple participants and those without
g_b_s_multiple <- ohio_time_gv %>% group_by(city_or_county) %>% 
   summarise(
     multiple_involved = sum(multiple_involved)
     )
g_b_s_multiple <- g_b_s_multiple[order(g_b_s_multiple$multiple_involved),]
g_b_s_multiple <- tail(g_b_s_multiple)
g_b_s_multiple <- g_b_s_multiple[order(g_b_s_multiple$multiple_involved)]
ggplot(g_b_s_multiple, aes(x=city_or_county, y=multiple_involved)) + 
  geom_point(size=3) + 
  geom_segment(aes(x=city_or_county, 
                   xend=city_or_county, 
                   y=0, 
                   yend=multiple_involved)) + 
  labs(title="Lollipop Chart", 
       subtitle="Incidents with Multiple participants per City/County"
       ) + 
  theme(axis.text.x = element_text(angle=65, vjust=0.6))

#Which age group did the participant(s) for gun related incidents belong to
g_b_s_age_group <- ohio_time_gv %>% group_by(city_or_county) %>% 
   summarise(
     child_participant = sum(child_participant),
     teen_participant = sum(teen_participant),
     adult_participant = sum(adult_participant)
     )
g_b_s_zero_values <- g_b_s_age_group[g_b_s_age_group$child_participant == 0 | g_b_s_age_group$teen_participant == 0 | g_b_s_age_group$adult_participant == 0 ,]
g_b_s_age_group <- anti_join(g_b_s_age_group,g_b_s_zero_values)
g_b_s_age_group <- g_b_s_age_group[order(g_b_s_age_group$child_participant),]
g_b_s_age_group <- tail(g_b_s_age_group)
g_b_s_age_group <- g_b_s_age_group %>%
  select(city_or_county, child_participant,teen_participant) %>%
  gather(key = "variable", value = "value", -city_or_county)

ggplot(g_b_s_age_group, aes(x = city_or_county, y = value, fill=variable), xlab="State", ylab="Number of Incidents") + 
  geom_bar(stat="identity", width = .5, position="dodge")
#Incidents that were school related, domestic in nature, or self defense related
g_b_s_related <- ohio_time_gv %>% group_by(city_or_county) %>% 
   summarise(
     school_related = sum(school_related),
     domestic_related = sum(domestic_related),
     defense_related = sum(defense_related)
     )
g_b_s_zero_values <- g_b_s_related[g_b_s_related$school_related == 0 | g_b_s_related$domestic_related == 0| g_b_s_related$defense_related == 0,]
g_b_s_related <- anti_join(g_b_s_related, g_b_s_zero_values)
g_b_s_related <- g_b_s_related[order(g_b_s_related$defense_related),]
g_b_s_related <- tail(g_b_s_related)
g_b_s_related <- g_b_s_related %>%
  select(city_or_county, school_related, domestic_related, defense_related) %>%
  gather(key = "variable", value = "value", -city_or_county)

ggplot(g_b_s_related, aes(x = city_or_county, y = value, fill=variable), xlab="State", ylab="Number of Incidents") + 
  geom_bar(stat="identity", width = .5, position="dodge")

penn_time_gv <- gun_2014_2017_data[grepl("Pennsylvania",gun_2014_2017_data$state),]
penn_gv <- penn_time_gv %>% group_by(date) %>% mutate(
                    n_killed = sum(n_killed),
                    n_injured = sum(n_injured), 
                    n_affected = n_killed + n_injured,
                    alcohol_related = sum(alcohol_related),
                    accidental = sum(accidental),
                    drug_related = sum(drug_related),
                    vehicular = sum(vehicular),
                    officer_related = sum(officer_related),
                    resulted_in_suicide = sum(resulted_in_suicide),
                    multiple_involved = sum(multiple_involved),
                    female_involved = sum(female_involved),
                    male_involved = sum(male_involved),
                    child_participant = sum(child_participant),
                    teen_participant = sum(teen_participant),
                    adult_participant = sum(adult_participant),
                    led_to_arrest = sum(led_to_arrest),
                    led_to_death = sum(led_to_death),
                    led_to_injury = sum(led_to_injury),
                    robbery_related = sum(robbery_related),
                    domestic_related = sum(domestic_related),
                    gang_related = sum(domestic_related),
                    defense_related = sum(defense_related),
                    school_related = sum(school_related),
                    year = case_when(
                        grepl("^2014",date) ~ 2014,
                        grepl("^2015",date) ~ 2015,
                        grepl("^2016",date) ~ 2016,
                        grepl("^2017",date) ~ 2017,
                        TRUE ~ 12
                      )
                    )
# Animated Bubble Chart Persons Killed and Affected
ani_bubble_chart <- ggplot(penn_gv, aes(n_killed, n_affected, size = n_injured, frame = year)) +
  geom_point() +
  geom_smooth(aes(group = year), 
              method = "lm", 
              show.legend = FALSE) +
  scale_x_log10()

gganimate(ani_bubble_chart)

#penn N_affected, N_killed, N_injured Time Series
n_affected_over_time <- penn_gv %>%
  select(date, n_killed, n_injured, n_affected) %>%
  gather(key = "variable", value = "value", -date)

ggplot(n_affected_over_time, aes(x = date, y = value)) + 
  geom_line(aes(color = variable), size = 1) +
  scale_color_manual(values = c("#00AFBB", "#E7B800", "#FC4E07")) +
  theme_minimal()

#Incidents resulting in Suicide, Death, Arrest and Injury
g_b_s_result <- penn_time_gv %>% group_by(city_or_county) %>% 
   summarise(
     resulted_in_suicide = sum(resulted_in_suicide),
     led_to_death = sum(led_to_death),
     led_to_arrest = sum(led_to_arrest),
     led_to_injury = sum(led_to_injury)
     )
#Want to find the most complete data for graph visualization purposes
g_b_s_zero_values <- g_b_s_result[g_b_s_result$led_to_death == 0 | g_b_s_result$led_to_injury ==0 |g_b_s_result$led_to_arrest ==0,]
g_b_s_result <- anti_join(g_b_s_result,g_b_s_zero_values)
g_b_s_result <- g_b_s_result[order(g_b_s_result$led_to_death),]
g_b_s_result <- tail(g_b_s_result)
g_b_s_result <- g_b_s_result%>%
  select(city_or_county, resulted_in_suicide,led_to_death,led_to_arrest,led_to_injury) %>%
  gather(key = "variable", value = "value", -city_or_county)

ggplot(g_b_s_result, aes(x = city_or_county, y = value, fill=variable), xlab="State", ylab="Number of Incidents") + 
  geom_bar(stat="identity", width = .5, position="dodge")

#Incidents that were vehicular, robbery related, gang related, or drug related
g_b_s_related <- penn_time_gv %>% group_by(city_or_county) %>% 
   summarise(
     vehicular = sum(vehicular),
     robbery_related = sum(robbery_related),
     gang_related = sum(gang_related),
     drug_related = sum(drug_related)
     )
#Want to find the most complete data for graph visualization purposes
g_b_s_related_zero_values <- g_b_s_related[g_b_s_related$vehicular== 0 | g_b_s_related$robbery_related ==0 | g_b_s_related$drug_related == 0 | g_b_s_related$gang_related == 0,] 
g_b_s_related <- anti_join(g_b_s_related,g_b_s_related_zero_values)
g_b_s_related <- g_b_s_related[order(g_b_s_related$gang_related),]
g_b_s_related <- tail(g_b_s_related)
g_b_s_related <- g_b_s_related %>%
  select(city_or_county, vehicular,robbery_related,gang_related,drug_related) %>%
  gather(key = "variable", value = "value", -city_or_county)

ggplot(g_b_s_related, aes(x = city_or_county, y = value, fill=variable), xlab="State", ylab="Number of Incidents") + 
  geom_bar(stat="identity", width = .5, position="dodge")

#Incidents with multiple participants and those without
g_b_s_multiple <- penn_time_gv %>% group_by(city_or_county) %>% 
   summarise(
     multiple_involved = sum(multiple_involved)
     )
g_b_s_multiple <- g_b_s_multiple[order(g_b_s_multiple$multiple_involved),]
g_b_s_multiple <- tail(g_b_s_multiple)
g_b_s_multiple <- g_b_s_multiple[order(g_b_s_multiple$multiple_involved)]
ggplot(g_b_s_multiple, aes(x=city_or_county, y=multiple_involved)) + 
  geom_point(size=3) + 
  geom_segment(aes(x=city_or_county, 
                   xend=city_or_county, 
                   y=0, 
                   yend=multiple_involved)) + 
  labs(title="Lollipop Chart", 
       subtitle="Incidents with Multiple participants per City/County"
       ) + 
  theme(axis.text.x = element_text(angle=65, vjust=0.6))

#Which age group did the participant(s) for gun related incidents belong to
g_b_s_age_group <- penn_time_gv %>% group_by(city_or_county) %>% 
   summarise(
     child_participant = sum(child_participant),
     teen_participant = sum(teen_participant),
     adult_participant = sum(adult_participant)
     )
g_b_s_zero_values <- g_b_s_age_group[g_b_s_age_group$child_participant == 0 | g_b_s_age_group$teen_participant == 0 | g_b_s_age_group$adult_participant == 0 ,]
g_b_s_age_group <- anti_join(g_b_s_age_group,g_b_s_zero_values)
g_b_s_age_group <- g_b_s_age_group[order(g_b_s_age_group$child_participant),]
g_b_s_age_group <- tail(g_b_s_age_group)
g_b_s_age_group <- g_b_s_age_group %>%
  select(city_or_county, child_participant,teen_participant) %>%
  gather(key = "variable", value = "value", -city_or_county)

ggplot(g_b_s_age_group, aes(x = city_or_county, y = value, fill=variable), xlab="State", ylab="Number of Incidents") + 
  geom_bar(stat="identity", width = .5, position="dodge")
#Incidents that were school related, domestic in nature, or self defense related
g_b_s_related <- penn_time_gv %>% group_by(city_or_county) %>% 
   summarise(
     school_related = sum(school_related),
     domestic_related = sum(domestic_related),
     defense_related = sum(defense_related)
     )
g_b_s_zero_values <- g_b_s_related[g_b_s_related$school_related == 0 | g_b_s_related$domestic_related == 0| g_b_s_related$defense_related == 0,]
g_b_s_related <- anti_join(g_b_s_related, g_b_s_zero_values)
g_b_s_related <- g_b_s_related[order(g_b_s_related$defense_related),]
g_b_s_related <- tail(g_b_s_related)
g_b_s_related <- g_b_s_related %>%
  select(city_or_county, school_related, domestic_related, defense_related) %>%
  gather(key = "variable", value = "value", -city_or_county)

ggplot(g_b_s_related, aes(x = city_or_county, y = value, fill=variable), xlab="State", ylab="Number of Incidents") + 
  geom_bar(stat="identity", width = .5, position="dodge")

#Least Affected States Statistics
wyo_time_gv <- gun_2014_2017_data[grepl("Wyoming",gun_2014_2017_data$state),]
wyo_gv <- wyo_time_gv %>% group_by(date) %>% mutate(
                    n_killed = sum(n_killed),
                    n_injured = sum(n_injured), 
                    n_affected = n_killed + n_injured,
                    alcohol_related = sum(alcohol_related),
                    accidental = sum(accidental),
                    drug_related = sum(drug_related),
                    vehicular = sum(vehicular),
                    officer_related = sum(officer_related),
                    resulted_in_suicide = sum(resulted_in_suicide),
                    multiple_involved = sum(multiple_involved),
                    female_involved = sum(female_involved),
                    male_involved = sum(male_involved),
                    child_participant = sum(child_participant),
                    teen_participant = sum(teen_participant),
                    adult_participant = sum(adult_participant),
                    led_to_arrest = sum(led_to_arrest),
                    led_to_death = sum(led_to_death),
                    led_to_injury = sum(led_to_injury),
                    robbery_related = sum(robbery_related),
                    domestic_related = sum(domestic_related),
                    gang_related = sum(domestic_related),
                    defense_related = sum(defense_related),
                    school_related = sum(school_related),
                    year = case_when(
                        grepl("^2014",date) ~ 2014,
                        grepl("^2015",date) ~ 2015,
                        grepl("^2016",date) ~ 2016,
                        grepl("^2017",date) ~ 2017,
                        TRUE ~ 12
                      )
                    )
# Animated Bubble Chart Persons Killed and Affected
ani_bubble_chart <- ggplot(wyo_gv, aes(n_killed, n_affected, size = n_injured, frame = year)) +
  geom_point() +
  geom_smooth(aes(group = year), 
              method = "lm", 
              show.legend = FALSE) +
  scale_x_log10()

gganimate(ani_bubble_chart)

#wyo N_affected, N_killed, N_injured Time Series
n_affected_over_time <- wyo_gv %>%
  select(date, n_killed, n_injured, n_affected) %>%
  gather(key = "variable", value = "value", -date)

ggplot(n_affected_over_time, aes(x = date, y = value)) + 
  geom_line(aes(color = variable), size = 1) +
  scale_color_manual(values = c("#00AFBB", "#E7B800", "#FC4E07")) +
  theme_minimal()

#Incidents resulting in Suicide, Death, Arrest and Injury
g_b_s_result <- wyo_time_gv %>% group_by(city_or_county) %>% 
   summarise(
     resulted_in_suicide = sum(resulted_in_suicide),
     led_to_death = sum(led_to_death),
     led_to_arrest = sum(led_to_arrest),
     led_to_injury = sum(led_to_injury)
     )
#Want to find the most complete data for graph visualization purposes
g_b_s_zero_values <- g_b_s_result[g_b_s_result$led_to_death == 0 | g_b_s_result$led_to_injury ==0 |g_b_s_result$led_to_arrest ==0,]
g_b_s_result <- anti_join(g_b_s_result,g_b_s_zero_values)
g_b_s_result <- g_b_s_result[order(g_b_s_result$led_to_death),]
g_b_s_result <- tail(g_b_s_result)
g_b_s_result <- g_b_s_result%>%
  select(city_or_county, resulted_in_suicide,led_to_death,led_to_arrest,led_to_injury) %>%
  gather(key = "variable", value = "value", -city_or_county)

ggplot(g_b_s_result, aes(x = city_or_county, y = value, fill=variable), xlab="State", ylab="Number of Incidents") + 
  geom_bar(stat="identity", width = .5, position="dodge")

#Which age group did the participant(s) for gun related incidents belong to
g_b_s_age_group <- wyo_time_gv %>% group_by(city_or_county) %>% 
   summarise(
     child_participant = sum(child_participant),
     teen_participant = sum(teen_participant),
     adult_participant = sum(adult_participant)
     )
g_b_s_age_group <- g_b_s_age_group[order(g_b_s_age_group$child_participant),]
g_b_s_age_group <- tail(g_b_s_age_group)
g_b_s_age_group <- g_b_s_age_group %>%
  select(city_or_county, child_participant,teen_participant) %>%
  gather(key = "variable", value = "value", -city_or_county)

ggplot(g_b_s_age_group, aes(x = city_or_county, y = value, fill=variable), xlab="State", ylab="Number of Incidents") + 
  geom_bar(stat="identity", width = .5, position="dodge")
#Incidents that were school related, domestic in nature, or self defense related
g_b_s_related <- wyo_time_gv %>% group_by(city_or_county) %>% 
   summarise(
     school_related = sum(school_related),
     domestic_related = sum(domestic_related),
     defense_related = sum(defense_related)
     )
g_b_s_related <- g_b_s_related[order(g_b_s_related$defense_related),]
g_b_s_related <- tail(g_b_s_related)
g_b_s_related <- g_b_s_related %>%
  select(city_or_county, school_related, domestic_related, defense_related) %>%
  gather(key = "variable", value = "value", -city_or_county)

ggplot(g_b_s_related, aes(x = city_or_county, y = value, fill=variable), xlab="State", ylab="Number of Incidents") + 
  geom_bar(stat="identity", width = .5, position="dodge")

ver_time_gv <- gun_2014_2017_data[grepl("Vermont",gun_2014_2017_data$state),]
ver_gv <- ver_time_gv %>% group_by(date) %>% mutate(
                    n_killed = sum(n_killed),
                    n_injured = sum(n_injured), 
                    n_affected = n_killed + n_injured,
                    alcohol_related = sum(alcohol_related),
                    accidental = sum(accidental),
                    drug_related = sum(drug_related),
                    vehicular = sum(vehicular),
                    officer_related = sum(officer_related),
                    resulted_in_suicide = sum(resulted_in_suicide),
                    multiple_involved = sum(multiple_involved),
                    female_involved = sum(female_involved),
                    male_involved = sum(male_involved),
                    child_participant = sum(child_participant),
                    teen_participant = sum(teen_participant),
                    adult_participant = sum(adult_participant),
                    led_to_arrest = sum(led_to_arrest),
                    led_to_death = sum(led_to_death),
                    led_to_injury = sum(led_to_injury),
                    robbery_related = sum(robbery_related),
                    domestic_related = sum(domestic_related),
                    gang_related = sum(domestic_related),
                    defense_related = sum(defense_related),
                    school_related = sum(school_related),
                    year = case_when(
                        grepl("^2014",date) ~ 2014,
                        grepl("^2015",date) ~ 2015,
                        grepl("^2016",date) ~ 2016,
                        grepl("^2017",date) ~ 2017,
                        TRUE ~ 12
                      )
                    )
# Animated Bubble Chart Persons Killed and Affected
ani_bubble_chart <- ggplot(ver_gv, aes(n_killed, n_affected, size = n_injured, frame = year)) +
  geom_point() +
  geom_smooth(aes(group = year), 
              method = "lm", 
              show.legend = FALSE) +
  scale_x_log10()

gganimate(ani_bubble_chart)

#ver N_affected, N_killed, N_injured Time Series
n_affected_over_time <- ver_gv %>%
  select(date, n_killed, n_injured, n_affected) %>%
  gather(key = "variable", value = "value", -date)

ggplot(n_affected_over_time, aes(x = date, y = value)) + 
  geom_line(aes(color = variable), size = 1) +
  scale_color_manual(values = c("#00AFBB", "#E7B800", "#FC4E07")) +
  theme_minimal()

#Incidents resulting in Suicide, Death, Arrest and Injury
g_b_s_result <- ver_time_gv %>% group_by(city_or_county) %>% 
   summarise(
     resulted_in_suicide = sum(resulted_in_suicide),
     led_to_death = sum(led_to_death),
     led_to_arrest = sum(led_to_arrest),
     led_to_injury = sum(led_to_injury)
     )
#Want to find the most complete data for graph visualization purposes
g_b_s_zero_values <- g_b_s_result[g_b_s_result$led_to_death == 0 | g_b_s_result$led_to_injury ==0 |g_b_s_result$led_to_arrest ==0,]
g_b_s_result <- anti_join(g_b_s_result,g_b_s_zero_values)
g_b_s_result <- g_b_s_result[order(g_b_s_result$led_to_death),]
g_b_s_result <- tail(g_b_s_result)
g_b_s_result <- g_b_s_result%>%
  select(city_or_county, resulted_in_suicide,led_to_death,led_to_arrest,led_to_injury) %>%
  gather(key = "variable", value = "value", -city_or_county)

ggplot(g_b_s_result, aes(x = city_or_county, y = value, fill=variable), xlab="State", ylab="Number of Incidents") + 
  geom_bar(stat="identity", width = .5, position="dodge")

#Which age group did the participant(s) for gun related incidents belong to
g_b_s_age_group <- ver_time_gv %>% group_by(city_or_county) %>% 
   summarise(
     child_participant = sum(child_participant),
     teen_participant = sum(teen_participant),
     adult_participant = sum(adult_participant)
     )
g_b_s_age_group <- g_b_s_age_group[order(g_b_s_age_group$child_participant),]
g_b_s_age_group <- tail(g_b_s_age_group)
g_b_s_age_group <- g_b_s_age_group %>%
  select(city_or_county, child_participant,teen_participant) %>%
  gather(key = "variable", value = "value", -city_or_county)

ggplot(g_b_s_age_group, aes(x = city_or_county, y = value, fill=variable), xlab="State", ylab="Number of Incidents") + 
  geom_bar(stat="identity", width = .5, position="dodge")
#Incidents that were school related, domestic in nature, or self defense related
g_b_s_related <- ver_time_gv %>% group_by(city_or_county) %>% 
   summarise(
     school_related = sum(school_related),
     domestic_related = sum(domestic_related),
     defense_related = sum(defense_related)
     )
g_b_s_related <- g_b_s_related[order(g_b_s_related$defense_related),]
g_b_s_related <- tail(g_b_s_related)
g_b_s_related <- g_b_s_related %>%
  select(city_or_county, school_related, domestic_related, defense_related) %>%
  gather(key = "variable", value = "value", -city_or_county)

ggplot(g_b_s_related, aes(x = city_or_county, y = value, fill=variable), xlab="State", ylab="Number of Incidents") + 
  geom_bar(stat="identity", width = .5, position="dodge")

hawaii_time_gv <- gun_2014_2017_data[grepl("Hawaii",gun_2014_2017_data$state),]
hawaii_gv <- hawaii_time_gv %>% group_by(date) %>% mutate(
                    n_killed = sum(n_killed),
                    n_injured = sum(n_injured), 
                    n_affected = n_killed + n_injured,
                    alcohol_related = sum(alcohol_related),
                    accidental = sum(accidental),
                    drug_related = sum(drug_related),
                    vehicular = sum(vehicular),
                    officer_related = sum(officer_related),
                    resulted_in_suicide = sum(resulted_in_suicide),
                    multiple_involved = sum(multiple_involved),
                    female_involved = sum(female_involved),
                    male_involved = sum(male_involved),
                    child_participant = sum(child_participant),
                    teen_participant = sum(teen_participant),
                    adult_participant = sum(adult_participant),
                    led_to_arrest = sum(led_to_arrest),
                    led_to_death = sum(led_to_death),
                    led_to_injury = sum(led_to_injury),
                    robbery_related = sum(robbery_related),
                    domestic_related = sum(domestic_related),
                    gang_related = sum(domestic_related),
                    defense_related = sum(defense_related),
                    school_related = sum(school_related),
                    year = case_when(
                        grepl("^2014",date) ~ 2014,
                        grepl("^2015",date) ~ 2015,
                        grepl("^2016",date) ~ 2016,
                        grepl("^2017",date) ~ 2017,
                        TRUE ~ 12
                      )
                    )
# Animated Bubble Chart Persons Killed and Affected
ani_bubble_chart <- ggplot(hawaii_gv, aes(n_killed, n_affected, size = n_injured, frame = year)) +
  geom_point() +
  geom_smooth(aes(group = year), 
              method = "lm", 
              show.legend = FALSE) +
  scale_x_log10()

gganimate(ani_bubble_chart)

#ver N_affected, N_killed, N_injured Time Series
n_affected_over_time <- hawaii_gv %>%
  select(date, n_killed, n_injured, n_affected) %>%
  gather(key = "variable", value = "value", -date)

ggplot(n_affected_over_time, aes(x = date, y = value)) + 
  geom_line(aes(color = variable), size = 1) +
  scale_color_manual(values = c("#00AFBB", "#E7B800", "#FC4E07")) +
  theme_minimal()

#Incidents resulting in Suicide, Death, Arrest and Injury
g_b_s_result <- hawaii_time_gv %>% group_by(city_or_county) %>% 
   summarise(
     resulted_in_suicide = sum(resulted_in_suicide),
     led_to_death = sum(led_to_death),
     led_to_arrest = sum(led_to_arrest),
     led_to_injury = sum(led_to_injury)
     )
#Want to find the most complete data for graph visualization purposes
g_b_s_zero_values <- g_b_s_result[g_b_s_result$led_to_death == 0 | g_b_s_result$led_to_injury ==0 |g_b_s_result$led_to_arrest ==0,]
g_b_s_result <- anti_join(g_b_s_result,g_b_s_zero_values)
g_b_s_result <- g_b_s_result[order(g_b_s_result$led_to_death),]
g_b_s_result <- tail(g_b_s_result)
g_b_s_result <- g_b_s_result%>%
  select(city_or_county, resulted_in_suicide,led_to_death,led_to_arrest,led_to_injury) %>%
  gather(key = "variable", value = "value", -city_or_county)

ggplot(g_b_s_result, aes(x = city_or_county, y = value, fill=variable), xlab="State", ylab="Number of Incidents") + 
  geom_bar(stat="identity", width = .5, position="dodge")

#Which age group did the participant(s) for gun related incidents belong to
g_b_s_age_group <- hawaii_time_gv %>% group_by(city_or_county) %>% 
   summarise(
     child_participant = sum(child_participant),
     teen_participant = sum(teen_participant),
     adult_participant = sum(adult_participant)
     )
g_b_s_age_group <- g_b_s_age_group[order(g_b_s_age_group$child_participant),]
g_b_s_age_group <- tail(g_b_s_age_group)
g_b_s_age_group <- g_b_s_age_group %>%
  select(city_or_county, child_participant,teen_participant) %>%
  gather(key = "variable", value = "value", -city_or_county)

ggplot(g_b_s_age_group, aes(x = city_or_county, y = value, fill=variable), xlab="State", ylab="Number of Incidents") + 
  geom_bar(stat="identity", width = .5, position="dodge")
#Incidents that were school related, domestic in nature, or self defense related
g_b_s_related <- hawaii_time_gv %>% group_by(city_or_county) %>% 
   summarise(
     school_related = sum(school_related),
     domestic_related = sum(domestic_related),
     defense_related = sum(defense_related)
     )
g_b_s_related <- g_b_s_related[order(g_b_s_related$defense_related),]
g_b_s_related <- tail(g_b_s_related)
g_b_s_related <- g_b_s_related %>%
  select(city_or_county, school_related, domestic_related, defense_related) %>%
  gather(key = "variable", value = "value", -city_or_county)

ggplot(g_b_s_related, aes(x = city_or_county, y = value, fill=variable), xlab="State", ylab="Number of Incidents") + 
  geom_bar(stat="identity", width = .5, position="dodge")

s_dakota_time_gv <- gun_2014_2017_data[grepl("South Dakota",gun_2014_2017_data$state),]
s_dakota_gv <- s_dakota_time_gv %>% group_by(date) %>% mutate(
                    n_killed = sum(n_killed),
                    n_injured = sum(n_injured), 
                    n_affected = n_killed + n_injured,
                    alcohol_related = sum(alcohol_related),
                    accidental = sum(accidental),
                    drug_related = sum(drug_related),
                    vehicular = sum(vehicular),
                    officer_related = sum(officer_related),
                    resulted_in_suicide = sum(resulted_in_suicide),
                    multiple_involved = sum(multiple_involved),
                    female_involved = sum(female_involved),
                    male_involved = sum(male_involved),
                    child_participant = sum(child_participant),
                    teen_participant = sum(teen_participant),
                    adult_participant = sum(adult_participant),
                    led_to_arrest = sum(led_to_arrest),
                    led_to_death = sum(led_to_death),
                    led_to_injury = sum(led_to_injury),
                    robbery_related = sum(robbery_related),
                    domestic_related = sum(domestic_related),
                    gang_related = sum(domestic_related),
                    defense_related = sum(defense_related),
                    school_related = sum(school_related),
                    year = case_when(
                        grepl("^2014",date) ~ 2014,
                        grepl("^2015",date) ~ 2015,
                        grepl("^2016",date) ~ 2016,
                        grepl("^2017",date) ~ 2017,
                        TRUE ~ 12
                      )
                    )
# Animated Bubble Chart Persons Killed and Affected
ani_bubble_chart <- ggplot(s_dakota_gv, aes(n_killed, n_affected, size = n_injured, frame = year)) +
  geom_point() +
  geom_smooth(aes(group = year), 
              method = "lm", 
              show.legend = FALSE) +
  scale_x_log10()

gganimate(ani_bubble_chart)

#ver N_affected, N_killed, N_injured Time Series
n_affected_over_time <- s_dakota_gv %>%
  select(date, n_killed, n_injured, n_affected) %>%
  gather(key = "variable", value = "value", -date)

ggplot(n_affected_over_time, aes(x = date, y = value)) + 
  geom_line(aes(color = variable), size = 1) +
  scale_color_manual(values = c("#00AFBB", "#E7B800", "#FC4E07")) +
  theme_minimal()

#Incidents resulting in Suicide, Death, Arrest and Injury
g_b_s_result <- s_dakota_time_gv %>% group_by(city_or_county) %>% 
   summarise(
     resulted_in_suicide = sum(resulted_in_suicide),
     led_to_death = sum(led_to_death),
     led_to_arrest = sum(led_to_arrest),
     led_to_injury = sum(led_to_injury)
     )
#Want to find the most complete data for graph visualization purposes
g_b_s_zero_values <- g_b_s_result[g_b_s_result$led_to_death == 0 | g_b_s_result$led_to_injury ==0 |g_b_s_result$led_to_arrest ==0,]
g_b_s_result <- anti_join(g_b_s_result,g_b_s_zero_values)
g_b_s_result <- g_b_s_result[order(g_b_s_result$led_to_death),]
g_b_s_result <- tail(g_b_s_result)
g_b_s_result <- g_b_s_result%>%
  select(city_or_county, resulted_in_suicide,led_to_death,led_to_arrest,led_to_injury) %>%
  gather(key = "variable", value = "value", -city_or_county)

ggplot(g_b_s_result, aes(x = city_or_county, y = value, fill=variable), xlab="State", ylab="Number of Incidents") + 
  geom_bar(stat="identity", width = .5, position="dodge")

#Which age group did the participant(s) for gun related incidents belong to
g_b_s_age_group <- s_dakota_time_gv %>% group_by(city_or_county) %>% 
   summarise(
     child_participant = sum(child_participant),
     teen_participant = sum(teen_participant),
     adult_participant = sum(adult_participant)
     )
g_b_s_age_group <- g_b_s_age_group[order(g_b_s_age_group$child_participant),]
g_b_s_age_group <- tail(g_b_s_age_group)
g_b_s_age_group <- g_b_s_age_group %>%
  select(city_or_county, child_participant,teen_participant) %>%
  gather(key = "variable", value = "value", -city_or_county)

ggplot(g_b_s_age_group, aes(x = city_or_county, y = value, fill=variable), xlab="State", ylab="Number of Incidents") + 
  geom_bar(stat="identity", width = .5, position="dodge")
#Incidents that were school related, domestic in nature, or self defense related
g_b_s_related <- s_dakota_time_gv %>% group_by(city_or_county) %>% 
   summarise(
     school_related = sum(school_related),
     domestic_related = sum(domestic_related),
     defense_related = sum(defense_related)
     )
g_b_s_related <- g_b_s_related[order(g_b_s_related$defense_related),]
g_b_s_related <- tail(g_b_s_related)
g_b_s_related <- g_b_s_related %>%
  select(city_or_county, school_related, domestic_related, defense_related) %>%
  gather(key = "variable", value = "value", -city_or_county)

ggplot(g_b_s_related, aes(x = city_or_county, y = value, fill=variable), xlab="State", ylab="Number of Incidents") + 
  geom_bar(stat="identity", width = .5, position="dodge")

n_dakota_time_gv <- gun_2014_2017_data[grepl("North Dakota",gun_2014_2017_data$state),]
n_dakota_gv <- n_dakota_time_gv %>% group_by(date) %>% mutate(
                    n_killed = sum(n_killed),
                    n_injured = sum(n_injured), 
                    n_affected = n_killed + n_injured,
                    alcohol_related = sum(alcohol_related),
                    accidental = sum(accidental),
                    drug_related = sum(drug_related),
                    vehicular = sum(vehicular),
                    officer_related = sum(officer_related),
                    resulted_in_suicide = sum(resulted_in_suicide),
                    multiple_involved = sum(multiple_involved),
                    female_involved = sum(female_involved),
                    male_involved = sum(male_involved),
                    child_participant = sum(child_participant),
                    teen_participant = sum(teen_participant),
                    adult_participant = sum(adult_participant),
                    led_to_arrest = sum(led_to_arrest),
                    led_to_death = sum(led_to_death),
                    led_to_injury = sum(led_to_injury),
                    robbery_related = sum(robbery_related),
                    domestic_related = sum(domestic_related),
                    gang_related = sum(domestic_related),
                    defense_related = sum(defense_related),
                    school_related = sum(school_related),
                    year = case_when(
                        grepl("^2014",date) ~ 2014,
                        grepl("^2015",date) ~ 2015,
                        grepl("^2016",date) ~ 2016,
                        grepl("^2017",date) ~ 2017,
                        TRUE ~ 12
                      )
                    )
# Animated Bubble Chart Persons Killed and Affected
ani_bubble_chart <- ggplot(n_dakota_gv, aes(n_killed, n_affected, size = n_injured, frame = year)) +
  geom_point() +
  geom_smooth(aes(group = year), 
              method = "lm", 
              show.legend = FALSE) +
  scale_x_log10()

gganimate(ani_bubble_chart)

#ver N_affected, N_killed, N_injured Time Series
n_affected_over_time <- n_dakota_gv %>%
  select(date, n_killed, n_injured, n_affected) %>%
  gather(key = "variable", value = "value", -date)

ggplot(n_affected_over_time, aes(x = date, y = value)) + 
  geom_line(aes(color = variable), size = 1) +
  scale_color_manual(values = c("#00AFBB", "#E7B800", "#FC4E07")) +
  theme_minimal()

#Incidents resulting in Suicide, Death, Arrest and Injury
g_b_s_result <- n_dakota_time_gv %>% group_by(city_or_county) %>% 
   summarise(
     resulted_in_suicide = sum(resulted_in_suicide),
     led_to_death = sum(led_to_death),
     led_to_arrest = sum(led_to_arrest),
     led_to_injury = sum(led_to_injury)
     )
#Want to find the most complete data for graph visualization purposes
g_b_s_zero_values <- g_b_s_result[g_b_s_result$led_to_death == 0 | g_b_s_result$led_to_injury ==0 |g_b_s_result$led_to_arrest ==0,]
g_b_s_result <- anti_join(g_b_s_result,g_b_s_zero_values)
g_b_s_result <- g_b_s_result[order(g_b_s_result$led_to_death),]
g_b_s_result <- tail(g_b_s_result)
g_b_s_result <- g_b_s_result%>%
  select(city_or_county, resulted_in_suicide,led_to_death,led_to_arrest,led_to_injury) %>%
  gather(key = "variable", value = "value", -city_or_county)

ggplot(g_b_s_result, aes(x = city_or_county, y = value, fill=variable), xlab="State", ylab="Number of Incidents") + 
  geom_bar(stat="identity", width = .5, position="dodge")

#Which age group did the participant(s) for gun related incidents belong to
g_b_s_age_group <- n_dakota_time_gv %>% group_by(city_or_county) %>% 
   summarise(
     child_participant = sum(child_participant),
     teen_participant = sum(teen_participant),
     adult_participant = sum(adult_participant)
     )
g_b_s_age_group <- g_b_s_age_group[order(g_b_s_age_group$child_participant),]
g_b_s_age_group <- tail(g_b_s_age_group)
g_b_s_age_group <- g_b_s_age_group %>%
  select(city_or_county, child_participant,teen_participant) %>%
  gather(key = "variable", value = "value", -city_or_county)

ggplot(g_b_s_age_group, aes(x = city_or_county, y = value, fill=variable), xlab="State", ylab="Number of Incidents") + 
  geom_bar(stat="identity", width = .5, position="dodge")
#Incidents that were school related, domestic in nature, or self defense related
g_b_s_related <- n_dakota_time_gv %>% group_by(city_or_county) %>% 
   summarise(
     school_related = sum(school_related),
     domestic_related = sum(domestic_related),
     defense_related = sum(defense_related)
     )
g_b_s_related <- g_b_s_related[order(g_b_s_related$defense_related),]
g_b_s_related <- tail(g_b_s_related)
g_b_s_related <- g_b_s_related %>%
  select(city_or_county, school_related, domestic_related, defense_related) %>%
  gather(key = "variable", value = "value", -city_or_county)

ggplot(g_b_s_related, aes(x = city_or_county, y = value, fill=variable), xlab="State", ylab="Number of Incidents") + 
  geom_bar(stat="identity", width = .5, position="dodge")



#group by city or county
gun_by_city <- gun_2014_2017_data %>% group_by(city_or_county) %>% summarise(
										n_killed = sum(n_killed),
										n_injured = sum(n_injured), 
										n_affected = n_killed + n_injured,
										alcohol_related = sum(alcohol_related),
										accidental = sum(accidental),
										drug_related = sum(drug_related),
										vehicular = sum(vehicular),
										officer_related = sum(officer_related),
										resulted_in_suicide = sum(resulted_in_suicide),
										multiple_involved = sum(multiple_involved),
										female_involved = sum(female_involved),
										male_involved = sum(male_involved),
										child_participant = sum(child_participant),
										teen_participant = sum(teen_participant),
										adult_participant = sum(adult_participant),
										led_to_arrest = sum(led_to_arrest),
										led_to_death = sum(led_to_death),
										led_to_injury = sum(led_to_injury),
										robbery_related = sum(robbery_related),
										domestic_related = sum(domestic_related),
										gang_related = sum(domestic_related),
										defense_related = sum(defense_related),
										school_related = sum(school_related)
										)
#correlations
cor(gun_by_city$n_affected,gun_by_city$drug_related,method = "pearson")
cor(gun_by_city$n_affected,gun_by_city$officer_related,method = "pearson")
cor(gun_by_city$n_affected,gun_by_city$multiple_involved,method = "pearson")
cor(gun_by_city$n_affected,gun_by_city$robbery_related,method = "pearson")
cor(gun_by_city$n_affected,gun_by_city$domestic_related,method = "pearson")
cor(gun_by_city$n_affected,gun_by_city$gang_related,method = "pearson")
cor(gun_by_city$n_affected,gun_by_city$school_related,method = "pearson")
cor(gun_by_city$led_to_death,gun_by_city$drug_related,method = "pearson")
cor(gun_by_city$led_to_death,gun_by_city$officer_related,method = "pearson")
cor(gun_by_city$led_to_death,gun_by_city$multiple_involved,method = "pearson")
cor(gun_by_city$led_to_death,gun_by_city$robbery_related,method = "pearson")
cor(gun_by_city$led_to_death,gun_by_city$domestic_related,method = "pearson")
cor(gun_by_city$led_to_death,gun_by_city$gang_related,method = "pearson")
cor(gun_by_city$led_to_death,gun_by_city$school_related,method = "pearson")
cor(gun_by_city$led_to_arrest,gun_by_city$drug_related,method = "pearson")
cor(gun_by_city$led_to_arrest,gun_by_city$officer_related,method = "pearson")
cor(gun_by_city$led_to_arrest,gun_by_city$multiple_involved,method = "pearson")
cor(gun_by_city$led_to_arrest,gun_by_city$robbery_related,method = "pearson")
cor(gun_by_city$led_to_arrest,gun_by_city$domestic_related,method = "pearson")
cor(gun_by_city$led_to_arrest,gun_by_city$gang_related,method = "pearson")
cor(gun_by_city$led_to_arrest,gun_by_city$school_related,method = "pearson")
#correlogram
subset_gun_city <- subset(gun_by_city, select=c(n_affected,led_to_death,led_to_arrest,drug_related,officer_related,multiple_involved,robbery_related,domestic_related,gang_related,school_related))
c_gram_city <- round(cor(subset_gun_city), 1)
# Plots the correlogram
ggcorrplot(
			c_gram_city, 
			hc.order = TRUE, 
            type = "lower", 
            lab = TRUE, 
            lab_size = 3, 
            method="circle", 
            colors = c("tomato2", "white", "springgreen3"), 
            title="Correlogram of Gun Violence Data by City", 
            ggtheme=theme_bw
            )
#get top 6 and bottom 6 city_or_county
gun_by_city <- gun_by_city[order(gun_by_city$n_affected),]
summary(gun_by_city)
least_affected_cities <- head(gun_by_city)
summary(least_affected_cities)
most_affected_cities <- tail(gun_by_city)
summary(most_affected_cities)
#Most Affected City/County Statistics
chicago_time_gv <- gun_2014_2017_data[grepl("Chicago",gun_2014_2017_data$city_or_county),]
chicago_gv <- chicago_time_gv %>% group_by(date) %>% mutate(
                    n_killed = sum(n_killed),
                    n_injured = sum(n_injured), 
                    n_affected = n_killed + n_injured,
                    alcohol_related = sum(alcohol_related),
                    accidental = sum(accidental),
                    drug_related = sum(drug_related),
                    vehicular = sum(vehicular),
                    officer_related = sum(officer_related),
                    resulted_in_suicide = sum(resulted_in_suicide),
                    multiple_involved = sum(multiple_involved),
                    female_involved = sum(female_involved),
                    male_involved = sum(male_involved),
                    child_participant = sum(child_participant),
                    teen_participant = sum(teen_participant),
                    adult_participant = sum(adult_participant),
                    led_to_arrest = sum(led_to_arrest),
                    led_to_death = sum(led_to_death),
                    led_to_injury = sum(led_to_injury),
                    robbery_related = sum(robbery_related),
                    domestic_related = sum(domestic_related),
                    gang_related = sum(domestic_related),
                    defense_related = sum(defense_related),
                    school_related = sum(school_related),
                    year = case_when(
                        grepl("^2014",date) ~ 2014,
                        grepl("^2015",date) ~ 2015,
                        grepl("^2016",date) ~ 2016,
                        grepl("^2017",date) ~ 2017,
                        TRUE ~ 12
                      )
                    )
# Animated Bubble Chart Persons Killed and Affected
ani_bubble_chart <- ggplot(chicago_gv, aes(n_killed, n_affected, size = n_injured, frame = year)) +
  geom_point() +
  geom_smooth(aes(group = year), 
              method = "lm", 
              show.legend = FALSE) +
  scale_x_log10()

gganimate(ani_bubble_chart)

#ver N_affected, N_killed, N_injured Time Series
n_affected_over_time <- chicago_gv %>%
  select(date, n_killed, n_injured, n_affected) %>%
  gather(key = "variable", value = "value", -date)

ggplot(n_affected_over_time, aes(x = date, y = value)) + 
  geom_line(aes(color = variable), size = 1) +
  scale_color_manual(values = c("#00AFBB", "#E7B800", "#FC4E07")) +
  theme_minimal()

#Incidents resulting in Suicide, Death, Arrest and Injury
g_b_s_result <- chicago_gv %>% group_by(year) %>% 
   summarise(
     resulted_in_suicide = sum(resulted_in_suicide),
     led_to_death = sum(led_to_death),
     led_to_arrest = sum(led_to_arrest),
     led_to_injury = sum(led_to_injury)
     )
#Want to find the most complete data for graph visualization purposes
g_b_s_zero_values <- g_b_s_result[g_b_s_result$led_to_death == 0 | g_b_s_result$led_to_injury ==0 |g_b_s_result$led_to_arrest ==0,]
g_b_s_result <- anti_join(g_b_s_result,g_b_s_zero_values)
g_b_s_result <- g_b_s_result[order(g_b_s_result$led_to_death),]
g_b_s_result <- tail(g_b_s_result)
g_b_s_result <- g_b_s_result%>%
  select(year, resulted_in_suicide,led_to_death,led_to_arrest,led_to_injury) %>%
  gather(key = "variable", value = "value", -year)

ggplot(g_b_s_result, aes(x = year, y = value, fill=variable), xlab="State", ylab="Number of Incidents") + 
  geom_bar(stat="identity", width = .5, position="dodge")

#Which age group did the participant(s) for gun related incidents belong to
g_b_s_age_group <- chicago_gv %>% group_by(year) %>% 
   summarise(
     child_participant = sum(child_participant),
     teen_participant = sum(teen_participant),
     adult_participant = sum(adult_participant)
     )
g_b_s_age_group <- g_b_s_age_group[order(g_b_s_age_group$child_participant),]
g_b_s_age_group <- tail(g_b_s_age_group)
g_b_s_age_group <- g_b_s_age_group %>%
  select(year, child_participant,teen_participant) %>%
  gather(key = "variable", value = "value", -year)

ggplot(g_b_s_age_group, aes(x = year, y = value, fill=variable), xlab="State", ylab="Number of Incidents") + 
  geom_bar(stat="identity", width = .5, position="dodge")
#Incidents that were school related, domestic in nature, or self defense related
g_b_s_related <- chicago_gv %>% group_by(year) %>% 
   summarise(
     school_related = sum(school_related),
     domestic_related = sum(domestic_related),
     defense_related = sum(defense_related)
     )
g_b_s_related <- g_b_s_related[order(g_b_s_related$defense_related),]
g_b_s_related <- tail(g_b_s_related)
g_b_s_related <- g_b_s_related %>%
  select(year, school_related, domestic_related, defense_related) %>%
  gather(key = "variable", value = "value", -year)

ggplot(g_b_s_related, aes(x = year, y = value, fill=variable), xlab="State", ylab="Number of Incidents") + 
  geom_bar(stat="identity", width = .5, position="dodge")

baltimore_time_gv <- gun_2014_2017_data[grepl("Baltimore",gun_2014_2017_data$city_or_county),]
baltimore_gv <- baltimore_time_gv %>% group_by(date) %>% mutate(
                    n_killed = sum(n_killed),
                    n_injured = sum(n_injured), 
                    n_affected = n_killed + n_injured,
                    alcohol_related = sum(alcohol_related),
                    accidental = sum(accidental),
                    drug_related = sum(drug_related),
                    vehicular = sum(vehicular),
                    officer_related = sum(officer_related),
                    resulted_in_suicide = sum(resulted_in_suicide),
                    multiple_involved = sum(multiple_involved),
                    female_involved = sum(female_involved),
                    male_involved = sum(male_involved),
                    child_participant = sum(child_participant),
                    teen_participant = sum(teen_participant),
                    adult_participant = sum(adult_participant),
                    led_to_arrest = sum(led_to_arrest),
                    led_to_death = sum(led_to_death),
                    led_to_injury = sum(led_to_injury),
                    robbery_related = sum(robbery_related),
                    domestic_related = sum(domestic_related),
                    gang_related = sum(domestic_related),
                    defense_related = sum(defense_related),
                    school_related = sum(school_related),
                    year = case_when(
                        grepl("^2014",date) ~ 2014,
                        grepl("^2015",date) ~ 2015,
                        grepl("^2016",date) ~ 2016,
                        grepl("^2017",date) ~ 2017,
                        TRUE ~ 12
                      )
                    )
# Animated Bubble Chart Persons Killed and Affected
ani_bubble_chart <- ggplot(baltimore_gv, aes(n_killed, n_affected, size = n_injured, frame = year)) +
  geom_point() +
  geom_smooth(aes(group = year), 
              method = "lm", 
              show.legend = FALSE) +
  scale_x_log10()

gganimate(ani_bubble_chart)

#ver N_affected, N_killed, N_injured Time Series
n_affected_over_time <- baltimore_gv %>%
  select(date, n_killed, n_injured, n_affected) %>%
  gather(key = "variable", value = "value", -date)

ggplot(n_affected_over_time, aes(x = date, y = value)) + 
  geom_line(aes(color = variable), size = 1) +
  scale_color_manual(values = c("#00AFBB", "#E7B800", "#FC4E07")) +
  theme_minimal()

#Incidents resulting in Suicide, Death, Arrest and Injury
g_b_s_result <- baltimore_gv %>% group_by(year) %>% 
   summarise(
     resulted_in_suicide = sum(resulted_in_suicide),
     led_to_death = sum(led_to_death),
     led_to_arrest = sum(led_to_arrest),
     led_to_injury = sum(led_to_injury)
     )
#Want to find the most complete data for graph visualization purposes
g_b_s_zero_values <- g_b_s_result[g_b_s_result$led_to_death == 0 | g_b_s_result$led_to_injury ==0 |g_b_s_result$led_to_arrest ==0,]
g_b_s_result <- anti_join(g_b_s_result,g_b_s_zero_values)
g_b_s_result <- g_b_s_result[order(g_b_s_result$led_to_death),]
g_b_s_result <- tail(g_b_s_result)
g_b_s_result <- g_b_s_result%>%
  select(year, resulted_in_suicide,led_to_death,led_to_arrest,led_to_injury) %>%
  gather(key = "variable", value = "value", -year)

ggplot(g_b_s_result, aes(x = year, y = value, fill=variable), xlab="State", ylab="Number of Incidents") + 
  geom_bar(stat="identity", width = .5, position="dodge")

#Which age group did the participant(s) for gun related incidents belong to
g_b_s_age_group <- baltimore_gv %>% group_by(year) %>% 
   summarise(
     child_participant = sum(child_participant),
     teen_participant = sum(teen_participant),
     adult_participant = sum(adult_participant)
     )
g_b_s_age_group <- g_b_s_age_group[order(g_b_s_age_group$child_participant),]
g_b_s_age_group <- tail(g_b_s_age_group)
g_b_s_age_group <- g_b_s_age_group %>%
  select(year, child_participant,teen_participant) %>%
  gather(key = "variable", value = "value", -year)

ggplot(g_b_s_age_group, aes(x = year, y = value, fill=variable), xlab="State", ylab="Number of Incidents") + 
  geom_bar(stat="identity", width = .5, position="dodge")
#Incidents that were school related, domestic in nature, or self defense related
g_b_s_related <- baltimore_gv %>% group_by(year) %>% 
   summarise(
     school_related = sum(school_related),
     domestic_related = sum(domestic_related),
     defense_related = sum(defense_related)
     )
g_b_s_related <- g_b_s_related[order(g_b_s_related$defense_related),]
g_b_s_related <- tail(g_b_s_related)
g_b_s_related <- g_b_s_related %>%
  select(year, school_related, domestic_related, defense_related) %>%
  gather(key = "variable", value = "value", -year)

ggplot(g_b_s_related, aes(x = year, y = value, fill=variable), xlab="State", ylab="Number of Incidents") + 
  geom_bar(stat="identity", width = .5, position="dodge")

philly_time_gv <- gun_2014_2017_data[grepl("Philadelphia",gun_2014_2017_data$city_or_county),]
philly_gv <- philly_time_gv %>% group_by(date) %>% mutate(
                    n_killed = sum(n_killed),
                    n_injured = sum(n_injured), 
                    n_affected = n_killed + n_injured,
                    alcohol_related = sum(alcohol_related),
                    accidental = sum(accidental),
                    drug_related = sum(drug_related),
                    vehicular = sum(vehicular),
                    officer_related = sum(officer_related),
                    resulted_in_suicide = sum(resulted_in_suicide),
                    multiple_involved = sum(multiple_involved),
                    female_involved = sum(female_involved),
                    male_involved = sum(male_involved),
                    child_participant = sum(child_participant),
                    teen_participant = sum(teen_participant),
                    adult_participant = sum(adult_participant),
                    led_to_arrest = sum(led_to_arrest),
                    led_to_death = sum(led_to_death),
                    led_to_injury = sum(led_to_injury),
                    robbery_related = sum(robbery_related),
                    domestic_related = sum(domestic_related),
                    gang_related = sum(domestic_related),
                    defense_related = sum(defense_related),
                    school_related = sum(school_related),
                    year = case_when(
                        grepl("^2014",date) ~ 2014,
                        grepl("^2015",date) ~ 2015,
                        grepl("^2016",date) ~ 2016,
                        grepl("^2017",date) ~ 2017,
                        TRUE ~ 12
                      )
                    )
# Animated Bubble Chart Persons Killed and Affected
ani_bubble_chart <- ggplot(philly_gv, aes(n_killed, n_affected, size = n_injured, frame = year)) +
  geom_point() +
  geom_smooth(aes(group = year), 
              method = "lm", 
              show.legend = FALSE) +
  scale_x_log10()

gganimate(ani_bubble_chart)

#ver N_affected, N_killed, N_injured Time Series
n_affected_over_time <- philly_gv %>%
  select(date, n_killed, n_injured, n_affected) %>%
  gather(key = "variable", value = "value", -date)

ggplot(n_affected_over_time, aes(x = date, y = value)) + 
  geom_line(aes(color = variable), size = 1) +
  scale_color_manual(values = c("#00AFBB", "#E7B800", "#FC4E07")) +
  theme_minimal()

#Incidents resulting in Suicide, Death, Arrest and Injury
g_b_s_result <- philly_gv %>% group_by(year) %>% 
   summarise(
     resulted_in_suicide = sum(resulted_in_suicide),
     led_to_death = sum(led_to_death),
     led_to_arrest = sum(led_to_arrest),
     led_to_injury = sum(led_to_injury)
     )
#Want to find the most complete data for graph visualization purposes
g_b_s_zero_values <- g_b_s_result[g_b_s_result$led_to_death == 0 | g_b_s_result$led_to_injury ==0 |g_b_s_result$led_to_arrest ==0,]
g_b_s_result <- anti_join(g_b_s_result,g_b_s_zero_values)
g_b_s_result <- g_b_s_result[order(g_b_s_result$led_to_death),]
g_b_s_result <- tail(g_b_s_result)
g_b_s_result <- g_b_s_result%>%
  select(year, resulted_in_suicide,led_to_death,led_to_arrest,led_to_injury) %>%
  gather(key = "variable", value = "value", -year)

ggplot(g_b_s_result, aes(x = year, y = value, fill=variable), xlab="State", ylab="Number of Incidents") + 
  geom_bar(stat="identity", width = .5, position="dodge")

#Which age group did the participant(s) for gun related incidents belong to
g_b_s_age_group <- philly_gv %>% group_by(year) %>% 
   summarise(
     child_participant = sum(child_participant),
     teen_participant = sum(teen_participant),
     adult_participant = sum(adult_participant)
     )
g_b_s_age_group <- g_b_s_age_group[order(g_b_s_age_group$child_participant),]
g_b_s_age_group <- tail(g_b_s_age_group)
g_b_s_age_group <- g_b_s_age_group %>%
  select(year, child_participant,teen_participant) %>%
  gather(key = "variable", value = "value", -year)

ggplot(g_b_s_age_group, aes(x = year, y = value, fill=variable), xlab="State", ylab="Number of Incidents") + 
  geom_bar(stat="identity", width = .5, position="dodge")
#Incidents that were school related, domestic in nature, or self defense related
g_b_s_related <- philly_gv %>% group_by(year) %>% 
   summarise(
     school_related = sum(school_related),
     domestic_related = sum(domestic_related),
     defense_related = sum(defense_related)
     )
g_b_s_related <- g_b_s_related[order(g_b_s_related$defense_related),]
g_b_s_related <- tail(g_b_s_related)
g_b_s_related <- g_b_s_related %>%
  select(year, school_related, domestic_related, defense_related) %>%
  gather(key = "variable", value = "value", -year)

ggplot(g_b_s_related, aes(x = year, y = value, fill=variable), xlab="State", ylab="Number of Incidents") + 
  geom_bar(stat="identity", width = .5, position="dodge")

saint_time_gv <- gun_2014_2017_data[grepl("Saint Louis",gun_2014_2017_data$city_or_county),]
saint_gv <- saint_time_gv %>% group_by(date) %>% mutate(
                    n_killed = sum(n_killed),
                    n_injured = sum(n_injured), 
                    n_affected = n_killed + n_injured,
                    alcohol_related = sum(alcohol_related),
                    accidental = sum(accidental),
                    drug_related = sum(drug_related),
                    vehicular = sum(vehicular),
                    officer_related = sum(officer_related),
                    resulted_in_suicide = sum(resulted_in_suicide),
                    multiple_involved = sum(multiple_involved),
                    female_involved = sum(female_involved),
                    male_involved = sum(male_involved),
                    child_participant = sum(child_participant),
                    teen_participant = sum(teen_participant),
                    adult_participant = sum(adult_participant),
                    led_to_arrest = sum(led_to_arrest),
                    led_to_death = sum(led_to_death),
                    led_to_injury = sum(led_to_injury),
                    robbery_related = sum(robbery_related),
                    domestic_related = sum(domestic_related),
                    gang_related = sum(domestic_related),
                    defense_related = sum(defense_related),
                    school_related = sum(school_related),
                    year = case_when(
                        grepl("^2014",date) ~ 2014,
                        grepl("^2015",date) ~ 2015,
                        grepl("^2016",date) ~ 2016,
                        grepl("^2017",date) ~ 2017,
                        TRUE ~ 12
                      )
                    )
# Animated Bubble Chart Persons Killed and Affected
ani_bubble_chart <- ggplot(saint_gv, aes(n_killed, n_affected, size = n_injured, frame = year)) +
  geom_point() +
  geom_smooth(aes(group = year), 
              method = "lm", 
              show.legend = FALSE) +
  scale_x_log10()

gganimate(ani_bubble_chart)

#ver N_affected, N_killed, N_injured Time Series
n_affected_over_time <- saint_gv %>%
  select(date, n_killed, n_injured, n_affected) %>%
  gather(key = "variable", value = "value", -date)

ggplot(n_affected_over_time, aes(x = date, y = value)) + 
  geom_line(aes(color = variable), size = 1) +
  scale_color_manual(values = c("#00AFBB", "#E7B800", "#FC4E07")) +
  theme_minimal()

#Incidents resulting in Suicide, Death, Arrest and Injury
g_b_s_result <- saint_gv %>% group_by(year) %>% 
   summarise(
     resulted_in_suicide = sum(resulted_in_suicide),
     led_to_death = sum(led_to_death),
     led_to_arrest = sum(led_to_arrest),
     led_to_injury = sum(led_to_injury)
     )
#Want to find the most complete data for graph visualization purposes
g_b_s_zero_values <- g_b_s_result[g_b_s_result$led_to_death == 0 | g_b_s_result$led_to_injury ==0 |g_b_s_result$led_to_arrest ==0,]
g_b_s_result <- anti_join(g_b_s_result,g_b_s_zero_values)
g_b_s_result <- g_b_s_result[order(g_b_s_result$led_to_death),]
g_b_s_result <- tail(g_b_s_result)
g_b_s_result <- g_b_s_result%>%
  select(year, resulted_in_suicide,led_to_death,led_to_arrest,led_to_injury) %>%
  gather(key = "variable", value = "value", -year)

ggplot(g_b_s_result, aes(x = year, y = value, fill=variable), xlab="State", ylab="Number of Incidents") + 
  geom_bar(stat="identity", width = .5, position="dodge")

#Which age group did the participant(s) for gun related incidents belong to
g_b_s_age_group <- saint_gv %>% group_by(year) %>% 
   summarise(
     child_participant = sum(child_participant),
     teen_participant = sum(teen_participant),
     adult_participant = sum(adult_participant)
     )
g_b_s_age_group <- g_b_s_age_group[order(g_b_s_age_group$child_participant),]
g_b_s_age_group <- tail(g_b_s_age_group)
g_b_s_age_group <- g_b_s_age_group %>%
  select(year, child_participant,teen_participant) %>%
  gather(key = "variable", value = "value", -year)

ggplot(g_b_s_age_group, aes(x = year, y = value, fill=variable), xlab="State", ylab="Number of Incidents") + 
  geom_bar(stat="identity", width = .5, position="dodge")
#Incidents that were school related, domestic in nature, or self defense related
g_b_s_related <- saint_gv %>% group_by(year) %>% 
   summarise(
     school_related = sum(school_related),
     domestic_related = sum(domestic_related),
     defense_related = sum(defense_related)
     )
g_b_s_related <- g_b_s_related[order(g_b_s_related$defense_related),]
g_b_s_related <- tail(g_b_s_related)
g_b_s_related <- g_b_s_related %>%
  select(year, school_related, domestic_related, defense_related) %>%
  gather(key = "variable", value = "value", -year)

ggplot(g_b_s_related, aes(x = year, y = value, fill=variable), xlab="State", ylab="Number of Incidents") + 
  geom_bar(stat="identity", width = .5, position="dodge")

penn_time_gv <- gun_2014_2017_data[grepl("Pennsylvania",gun_2014_2017_data$city_or_county),]
penn_gv <- penn_time_gv %>% group_by(date) %>% mutate(
                    n_killed = sum(n_killed),
                    n_injured = sum(n_injured), 
                    n_affected = n_killed + n_injured,
                    alcohol_related = sum(alcohol_related),
                    accidental = sum(accidental),
                    drug_related = sum(drug_related),
                    vehicular = sum(vehicular),
                    officer_related = sum(officer_related),
                    resulted_in_suicide = sum(resulted_in_suicide),
                    multiple_involved = sum(multiple_involved),
                    female_involved = sum(female_involved),
                    male_involved = sum(male_involved),
                    child_participant = sum(child_participant),
                    teen_participant = sum(teen_participant),
                    adult_participant = sum(adult_participant),
                    led_to_arrest = sum(led_to_arrest),
                    led_to_death = sum(led_to_death),
                    led_to_injury = sum(led_to_injury),
                    robbery_related = sum(robbery_related),
                    domestic_related = sum(domestic_related),
                    gang_related = sum(domestic_related),
                    defense_related = sum(defense_related),
                    school_related = sum(school_related),
                    year = case_when(
                        grepl("^2014",date) ~ 2014,
                        grepl("^2015",date) ~ 2015,
                        grepl("^2016",date) ~ 2016,
                        grepl("^2017",date) ~ 2017,
                        TRUE ~ 12
                      )
                    )
# Animated Bubble Chart Persons Killed and Affected
ani_bubble_chart <- ggplot(penn_gv, aes(n_killed, n_affected, size = n_injured, frame = year)) +
  geom_point() +
  geom_smooth(aes(group = year), 
              method = "lm", 
              show.legend = FALSE) +
  scale_x_log10()

gganimate(ani_bubble_chart)

#ver N_affected, N_killed, N_injured Time Series
n_affected_over_time <- penn_gv %>%
  select(date, n_killed, n_injured, n_affected) %>%
  gather(key = "variable", value = "value", -date)

ggplot(n_affected_over_time, aes(x = date, y = value)) + 
  geom_line(aes(color = variable), size = 1) +
  scale_color_manual(values = c("#00AFBB", "#E7B800", "#FC4E07")) +
  theme_minimal()

#Incidents resulting in Suicide, Death, Arrest and Injury
g_b_s_result <- penn_gv %>% group_by(year) %>% 
   summarise(
     resulted_in_suicide = sum(resulted_in_suicide),
     led_to_death = sum(led_to_death),
     led_to_arrest = sum(led_to_arrest),
     led_to_injury = sum(led_to_injury)
     )
#Want to find the most complete data for graph visualization purposes
g_b_s_zero_values <- g_b_s_result[g_b_s_result$led_to_death == 0 | g_b_s_result$led_to_injury ==0 |g_b_s_result$led_to_arrest ==0,]
g_b_s_result <- anti_join(g_b_s_result,g_b_s_zero_values)
g_b_s_result <- g_b_s_result[order(g_b_s_result$led_to_death),]
g_b_s_result <- tail(g_b_s_result)
g_b_s_result <- g_b_s_result%>%
  select(year, resulted_in_suicide,led_to_death,led_to_arrest,led_to_injury) %>%
  gather(key = "variable", value = "value", -year)

ggplot(g_b_s_result, aes(x = year, y = value, fill=variable), xlab="State", ylab="Number of Incidents") + 
  geom_bar(stat="identity", width = .5, position="dodge")

#Which age group did the participant(s) for gun related incidents belong to
g_b_s_age_group <- penn_gv %>% group_by(year) %>% 
   summarise(
     child_participant = sum(child_participant),
     teen_participant = sum(teen_participant),
     adult_participant = sum(adult_participant)
     )
g_b_s_age_group <- g_b_s_age_group[order(g_b_s_age_group$child_participant),]
g_b_s_age_group <- tail(g_b_s_age_group)
g_b_s_age_group <- g_b_s_age_group %>%
  select(year, child_participant,teen_participant) %>%
  gather(key = "variable", value = "value", -year)

ggplot(g_b_s_age_group, aes(x = year, y = value, fill=variable), xlab="State", ylab="Number of Incidents") + 
  geom_bar(stat="identity", width = .5, position="dodge")
#Incidents that were school related, domestic in nature, or self defense related
g_b_s_related <- penn_gv %>% group_by(year) %>% 
   summarise(
     school_related = sum(school_related),
     domestic_related = sum(domestic_related),
     defense_related = sum(defense_related)
     )
g_b_s_related <- g_b_s_related[order(g_b_s_related$defense_related),]
g_b_s_related <- tail(g_b_s_related)
g_b_s_related <- g_b_s_related %>%
  select(year, school_related, domestic_related, defense_related) %>%
  gather(key = "variable", value = "value", -year)

ggplot(g_b_s_related, aes(x = year, y = value, fill=variable), xlab="State", ylab="Number of Incidents") + 
  geom_bar(stat="identity", width = .5, position="dodge")

#Least Affected City/County Statistics
abbotsford_time_gv <- gun_2014_2017_data[grepl("Abbotsford",gun_2014_2017_data$city_or_county),]
abbotsford_gv <- abbotsford_time_gv %>% group_by(date) %>% mutate(
                    n_killed = sum(n_killed),
                    n_injured = sum(n_injured), 
                    n_affected = n_killed + n_injured,
                    alcohol_related = sum(alcohol_related),
                    accidental = sum(accidental),
                    drug_related = sum(drug_related),
                    vehicular = sum(vehicular),
                    officer_related = sum(officer_related),
                    resulted_in_suicide = sum(resulted_in_suicide),
                    multiple_involved = sum(multiple_involved),
                    female_involved = sum(female_involved),
                    male_involved = sum(male_involved),
                    child_participant = sum(child_participant),
                    teen_participant = sum(teen_participant),
                    adult_participant = sum(adult_participant),
                    led_to_arrest = sum(led_to_arrest),
                    led_to_death = sum(led_to_death),
                    led_to_injury = sum(led_to_injury),
                    robbery_related = sum(robbery_related),
                    domestic_related = sum(domestic_related),
                    gang_related = sum(domestic_related),
                    defense_related = sum(defense_related),
                    school_related = sum(school_related),
                    year = case_when(
                        grepl("^2014",date) ~ 2014,
                        grepl("^2015",date) ~ 2015,
                        grepl("^2016",date) ~ 2016,
                        grepl("^2017",date) ~ 2017,
                        TRUE ~ 12
                      )
                    )
# Animated Bubble Chart Persons Killed and Affected
ani_bubble_chart <- ggplot(abbotsford_gv, aes(n_killed, n_affected, size = n_injured, frame = year)) +
  geom_point() +
  geom_smooth(aes(group = year), 
              method = "lm", 
              show.legend = FALSE) +
  scale_x_log10()

gganimate(ani_bubble_chart)

#ver N_affected, N_killed, N_injured Time Series
n_affected_over_time <- abbotsford_gv %>%
  select(date, n_killed, n_injured, n_affected) %>%
  gather(key = "variable", value = "value", -date)

ggplot(n_affected_over_time, aes(x = date, y = value)) + 
  geom_line(aes(color = variable), size = 1) +
  scale_color_manual(values = c("#00AFBB", "#E7B800", "#FC4E07")) +
  theme_minimal()

#Incidents resulting in Suicide, Death, Arrest and Injury
g_b_s_result <- abbotsford_gv %>% group_by(year) %>% 
   summarise(
     resulted_in_suicide = sum(resulted_in_suicide),
     led_to_death = sum(led_to_death),
     led_to_arrest = sum(led_to_arrest),
     led_to_injury = sum(led_to_injury)
     )
#Want to find the most complete data for graph visualization purposes
g_b_s_zero_values <- g_b_s_result[g_b_s_result$led_to_death == 0 | g_b_s_result$led_to_injury ==0 |g_b_s_result$led_to_arrest ==0,]
g_b_s_result <- anti_join(g_b_s_result,g_b_s_zero_values)
g_b_s_result <- g_b_s_result[order(g_b_s_result$led_to_death),]
g_b_s_result <- tail(g_b_s_result)
g_b_s_result <- g_b_s_result%>%
  select(year, resulted_in_suicide,led_to_death,led_to_arrest,led_to_injury) %>%
  gather(key = "variable", value = "value", -year)

ggplot(g_b_s_result, aes(x = year, y = value, fill=variable), xlab="State", ylab="Number of Incidents") + 
  geom_bar(stat="identity", width = .5, position="dodge")

#Which age group did the participant(s) for gun related incidents belong to
g_b_s_age_group <- abbotsford_gv %>% group_by(year) %>% 
   summarise(
     child_participant = sum(child_participant),
     teen_participant = sum(teen_participant),
     adult_participant = sum(adult_participant)
     )
g_b_s_age_group <- g_b_s_age_group[order(g_b_s_age_group$child_participant),]
g_b_s_age_group <- tail(g_b_s_age_group)
g_b_s_age_group <- g_b_s_age_group %>%
  select(year, child_participant,teen_participant) %>%
  gather(key = "variable", value = "value", -year)

ggplot(g_b_s_age_group, aes(x = year, y = value, fill=variable), xlab="State", ylab="Number of Incidents") + 
  geom_bar(stat="identity", width = .5, position="dodge")
#Incidents that were school related, domestic in nature, or self defense related
g_b_s_related <- abbotsford_gv %>% group_by(year) %>% 
   summarise(
     school_related = sum(school_related),
     domestic_related = sum(domestic_related),
     defense_related = sum(defense_related)
     )
g_b_s_related <- g_b_s_related[order(g_b_s_related$defense_related),]
g_b_s_related <- tail(g_b_s_related)
g_b_s_related <- g_b_s_related %>%
  select(year, school_related, domestic_related, defense_related) %>%
  gather(key = "variable", value = "value", -year)

ggplot(g_b_s_related, aes(x = year, y = value, fill=variable), xlab="State", ylab="Number of Incidents") + 
  geom_bar(stat="identity", width = .5, position="dodge")

abbottstown_time_gv <- gun_2014_2017_data[grepl("Abbottstown",gun_2014_2017_data$city_or_county),]
abbottstown_gv <- abbottstown_time_gv %>% group_by(date) %>% mutate(
                    n_killed = sum(n_killed),
                    n_injured = sum(n_injured), 
                    n_affected = n_killed + n_injured,
                    alcohol_related = sum(alcohol_related),
                    accidental = sum(accidental),
                    drug_related = sum(drug_related),
                    vehicular = sum(vehicular),
                    officer_related = sum(officer_related),
                    resulted_in_suicide = sum(resulted_in_suicide),
                    multiple_involved = sum(multiple_involved),
                    female_involved = sum(female_involved),
                    male_involved = sum(male_involved),
                    child_participant = sum(child_participant),
                    teen_participant = sum(teen_participant),
                    adult_participant = sum(adult_participant),
                    led_to_arrest = sum(led_to_arrest),
                    led_to_death = sum(led_to_death),
                    led_to_injury = sum(led_to_injury),
                    robbery_related = sum(robbery_related),
                    domestic_related = sum(domestic_related),
                    gang_related = sum(domestic_related),
                    defense_related = sum(defense_related),
                    school_related = sum(school_related),
                    year = case_when(
                        grepl("^2014",date) ~ 2014,
                        grepl("^2015",date) ~ 2015,
                        grepl("^2016",date) ~ 2016,
                        grepl("^2017",date) ~ 2017,
                        TRUE ~ 12
                      )
                    )
# Animated Bubble Chart Persons Killed and Affected
ani_bubble_chart <- ggplot(abbottstown_gv, aes(n_killed, n_affected, size = n_injured, frame = year)) +
  geom_point() +
  geom_smooth(aes(group = year), 
              method = "lm", 
              show.legend = FALSE) +
  scale_x_log10()

gganimate(ani_bubble_chart)

#ver N_affected, N_killed, N_injured Time Series
n_affected_over_time <- abbottstown_gv %>%
  select(date, n_killed, n_injured, n_affected) %>%
  gather(key = "variable", value = "value", -date)

ggplot(n_affected_over_time, aes(x = date, y = value)) + 
  geom_line(aes(color = variable), size = 1) +
  scale_color_manual(values = c("#00AFBB", "#E7B800", "#FC4E07")) +
  theme_minimal()

#Incidents resulting in Suicide, Death, Arrest and Injury
g_b_s_result <- abbottstown_gv %>% group_by(year) %>% 
   summarise(
     resulted_in_suicide = sum(resulted_in_suicide),
     led_to_death = sum(led_to_death),
     led_to_arrest = sum(led_to_arrest),
     led_to_injury = sum(led_to_injury)
     )
#Want to find the most complete data for graph visualization purposes
g_b_s_zero_values <- g_b_s_result[g_b_s_result$led_to_death == 0 | g_b_s_result$led_to_injury ==0 |g_b_s_result$led_to_arrest ==0,]
g_b_s_result <- anti_join(g_b_s_result,g_b_s_zero_values)
g_b_s_result <- g_b_s_result[order(g_b_s_result$led_to_death),]
g_b_s_result <- tail(g_b_s_result)
g_b_s_result <- g_b_s_result%>%
  select(year, resulted_in_suicide,led_to_death,led_to_arrest,led_to_injury) %>%
  gather(key = "variable", value = "value", -year)

ggplot(g_b_s_result, aes(x = year, y = value, fill=variable), xlab="State", ylab="Number of Incidents") + 
  geom_bar(stat="identity", width = .5, position="dodge")

#Which age group did the participant(s) for gun related incidents belong to
g_b_s_age_group <- abbottstown_gv %>% group_by(year) %>% 
   summarise(
     child_participant = sum(child_participant),
     teen_participant = sum(teen_participant),
     adult_participant = sum(adult_participant)
     )
g_b_s_age_group <- g_b_s_age_group[order(g_b_s_age_group$child_participant),]
g_b_s_age_group <- tail(g_b_s_age_group)
g_b_s_age_group <- g_b_s_age_group %>%
  select(year, child_participant,teen_participant) %>%
  gather(key = "variable", value = "value", -year)

ggplot(g_b_s_age_group, aes(x = year, y = value, fill=variable), xlab="State", ylab="Number of Incidents") + 
  geom_bar(stat="identity", width = .5, position="dodge")
#Incidents that were school related, domestic in nature, or self defense related
g_b_s_related <- abbottstown_gv %>% group_by(year) %>% 
   summarise(
     school_related = sum(school_related),
     domestic_related = sum(domestic_related),
     defense_related = sum(defense_related)
     )
g_b_s_related <- g_b_s_related[order(g_b_s_related$defense_related),]
g_b_s_related <- tail(g_b_s_related)
g_b_s_related <- g_b_s_related %>%
  select(year, school_related, domestic_related, defense_related) %>%
  gather(key = "variable", value = "value", -year)

ggplot(g_b_s_related, aes(x = year, y = value, fill=variable), xlab="State", ylab="Number of Incidents") + 
  geom_bar(stat="identity", width = .5, position="dodge")

abita_springs_time_gv <- gun_2014_2017_data[grepl("Abita Springs",gun_2014_2017_data$city_or_county),]
abita_springs_gv <- abita_springs_time_gv %>% group_by(date) %>% mutate(
                    n_killed = sum(n_killed),
                    n_injured = sum(n_injured), 
                    n_affected = n_killed + n_injured,
                    alcohol_related = sum(alcohol_related),
                    accidental = sum(accidental),
                    drug_related = sum(drug_related),
                    vehicular = sum(vehicular),
                    officer_related = sum(officer_related),
                    resulted_in_suicide = sum(resulted_in_suicide),
                    multiple_involved = sum(multiple_involved),
                    female_involved = sum(female_involved),
                    male_involved = sum(male_involved),
                    child_participant = sum(child_participant),
                    teen_participant = sum(teen_participant),
                    adult_participant = sum(adult_participant),
                    led_to_arrest = sum(led_to_arrest),
                    led_to_death = sum(led_to_death),
                    led_to_injury = sum(led_to_injury),
                    robbery_related = sum(robbery_related),
                    domestic_related = sum(domestic_related),
                    gang_related = sum(domestic_related),
                    defense_related = sum(defense_related),
                    school_related = sum(school_related),
                    year = case_when(
                        grepl("^2014",date) ~ 2014,
                        grepl("^2015",date) ~ 2015,
                        grepl("^2016",date) ~ 2016,
                        grepl("^2017",date) ~ 2017,
                        TRUE ~ 12
                      )
                    )
# Animated Bubble Chart Persons Killed and Affected
ani_bubble_chart <- ggplot(abita_springs_gv, aes(n_killed, n_affected, size = n_injured, frame = year)) +
  geom_point() +
  geom_smooth(aes(group = year), 
              method = "lm", 
              show.legend = FALSE) +
  scale_x_log10()

gganimate(ani_bubble_chart)

#ver N_affected, N_killed, N_injured Time Series
n_affected_over_time <- abita_springs_gv %>%
  select(date, n_killed, n_injured, n_affected) %>%
  gather(key = "variable", value = "value", -date)

ggplot(n_affected_over_time, aes(x = date, y = value)) + 
  geom_line(aes(color = variable), size = 1) +
  scale_color_manual(values = c("#00AFBB", "#E7B800", "#FC4E07")) +
  theme_minimal()

#Incidents resulting in Suicide, Death, Arrest and Injury
g_b_s_result <- abita_springs_gv %>% group_by(year) %>% 
   summarise(
     resulted_in_suicide = sum(resulted_in_suicide),
     led_to_death = sum(led_to_death),
     led_to_arrest = sum(led_to_arrest),
     led_to_injury = sum(led_to_injury)
     )
#Want to find the most complete data for graph visualization purposes
g_b_s_zero_values <- g_b_s_result[g_b_s_result$led_to_death == 0 | g_b_s_result$led_to_injury ==0 |g_b_s_result$led_to_arrest ==0,]
g_b_s_result <- anti_join(g_b_s_result,g_b_s_zero_values)
g_b_s_result <- g_b_s_result[order(g_b_s_result$led_to_death),]
g_b_s_result <- tail(g_b_s_result)
g_b_s_result <- g_b_s_result%>%
  select(year, resulted_in_suicide,led_to_death,led_to_arrest,led_to_injury) %>%
  gather(key = "variable", value = "value", -year)

ggplot(g_b_s_result, aes(x = year, y = value, fill=variable), xlab="State", ylab="Number of Incidents") + 
  geom_bar(stat="identity", width = .5, position="dodge")

#Which age group did the participant(s) for gun related incidents belong to
g_b_s_age_group <- abita_springs_gv %>% group_by(year) %>% 
   summarise(
     child_participant = sum(child_participant),
     teen_participant = sum(teen_participant),
     adult_participant = sum(adult_participant)
     )
g_b_s_age_group <- g_b_s_age_group[order(g_b_s_age_group$child_participant),]
g_b_s_age_group <- tail(g_b_s_age_group)
g_b_s_age_group <- g_b_s_age_group %>%
  select(year, child_participant,teen_participant) %>%
  gather(key = "variable", value = "value", -year)

ggplot(g_b_s_age_group, aes(x = year, y = value, fill=variable), xlab="State", ylab="Number of Incidents") + 
  geom_bar(stat="identity", width = .5, position="dodge")
#Incidents that were school related, domestic in nature, or self defense related
g_b_s_related <- abita_springs_gv %>% group_by(year) %>% 
   summarise(
     school_related = sum(school_related),
     domestic_related = sum(domestic_related),
     defense_related = sum(defense_related)
     )
g_b_s_related <- g_b_s_related[order(g_b_s_related$defense_related),]
g_b_s_related <- tail(g_b_s_related)
g_b_s_related <- g_b_s_related %>%
  select(year, school_related, domestic_related, defense_related) %>%
  gather(key = "variable", value = "value", -year)

ggplot(g_b_s_related, aes(x = year, y = value, fill=variable), xlab="State", ylab="Number of Incidents") + 
  geom_bar(stat="identity", width = .5, position="dodge")

acadia_time_gv <- gun_2014_2017_data[grepl("Acadia",gun_2014_2017_data$city_or_county),]
acadia_gv <- acadia_time_gv %>% group_by(date) %>% mutate(
                    n_killed = sum(n_killed),
                    n_injured = sum(n_injured), 
                    n_affected = n_killed + n_injured,
                    alcohol_related = sum(alcohol_related),
                    accidental = sum(accidental),
                    drug_related = sum(drug_related),
                    vehicular = sum(vehicular),
                    officer_related = sum(officer_related),
                    resulted_in_suicide = sum(resulted_in_suicide),
                    multiple_involved = sum(multiple_involved),
                    female_involved = sum(female_involved),
                    male_involved = sum(male_involved),
                    child_participant = sum(child_participant),
                    teen_participant = sum(teen_participant),
                    adult_participant = sum(adult_participant),
                    led_to_arrest = sum(led_to_arrest),
                    led_to_death = sum(led_to_death),
                    led_to_injury = sum(led_to_injury),
                    robbery_related = sum(robbery_related),
                    domestic_related = sum(domestic_related),
                    gang_related = sum(domestic_related),
                    defense_related = sum(defense_related),
                    school_related = sum(school_related),
                    year = case_when(
                        grepl("^2014",date) ~ 2014,
                        grepl("^2015",date) ~ 2015,
                        grepl("^2016",date) ~ 2016,
                        grepl("^2017",date) ~ 2017,
                        TRUE ~ 12
                      )
                    )
# Animated Bubble Chart Persons Killed and Affected
ani_bubble_chart <- ggplot(acadia_gv, aes(n_killed, n_affected, size = n_injured, frame = year)) +
  geom_point() +
  geom_smooth(aes(group = year), 
              method = "lm", 
              show.legend = FALSE) +
  scale_x_log10()

gganimate(ani_bubble_chart)

#ver N_affected, N_killed, N_injured Time Series
n_affected_over_time <- acadia_gv %>%
  select(date, n_killed, n_injured, n_affected) %>%
  gather(key = "variable", value = "value", -date)

ggplot(n_affected_over_time, aes(x = date, y = value)) + 
  geom_line(aes(color = variable), size = 1) +
  scale_color_manual(values = c("#00AFBB", "#E7B800", "#FC4E07")) +
  theme_minimal()

#Incidents resulting in Suicide, Death, Arrest and Injury
g_b_s_result <- acadia_gv %>% group_by(year) %>% 
   summarise(
     resulted_in_suicide = sum(resulted_in_suicide),
     led_to_death = sum(led_to_death),
     led_to_arrest = sum(led_to_arrest),
     led_to_injury = sum(led_to_injury)
     )
#Want to find the most complete data for graph visualization purposes
g_b_s_zero_values <- g_b_s_result[g_b_s_result$led_to_death == 0 | g_b_s_result$led_to_injury ==0 |g_b_s_result$led_to_arrest ==0,]
g_b_s_result <- anti_join(g_b_s_result,g_b_s_zero_values)
g_b_s_result <- g_b_s_result[order(g_b_s_result$led_to_death),]
g_b_s_result <- tail(g_b_s_result)
g_b_s_result <- g_b_s_result%>%
  select(year, resulted_in_suicide,led_to_death,led_to_arrest,led_to_injury) %>%
  gather(key = "variable", value = "value", -year)

ggplot(g_b_s_result, aes(x = year, y = value, fill=variable), xlab="State", ylab="Number of Incidents") + 
  geom_bar(stat="identity", width = .5, position="dodge")

#Which age group did the participant(s) for gun related incidents belong to
g_b_s_age_group <- acadia_gv %>% group_by(year) %>% 
   summarise(
     child_participant = sum(child_participant),
     teen_participant = sum(teen_participant),
     adult_participant = sum(adult_participant)
     )
g_b_s_age_group <- g_b_s_age_group[order(g_b_s_age_group$child_participant),]
g_b_s_age_group <- tail(g_b_s_age_group)
g_b_s_age_group <- g_b_s_age_group %>%
  select(year, child_participant,teen_participant) %>%
  gather(key = "variable", value = "value", -year)

ggplot(g_b_s_age_group, aes(x = year, y = value, fill=variable), xlab="State", ylab="Number of Incidents") + 
  geom_bar(stat="identity", width = .5, position="dodge")
#Incidents that were school related, domestic in nature, or self defense related
g_b_s_related <- acadia_gv %>% group_by(year) %>% 
   summarise(
     school_related = sum(school_related),
     domestic_related = sum(domestic_related),
     defense_related = sum(defense_related)
     )
g_b_s_related <- g_b_s_related[order(g_b_s_related$defense_related),]
g_b_s_related <- tail(g_b_s_related)
g_b_s_related <- g_b_s_related %>%
  select(year, school_related, domestic_related, defense_related) %>%
  gather(key = "variable", value = "value", -year)

ggplot(g_b_s_related, aes(x = year, y = value, fill=variable), xlab="State", ylab="Number of Incidents") + 
  geom_bar(stat="identity", width = .5, position="dodge")

acushnet_time_gv <- gun_2014_2017_data[grepl("Acushnet",gun_2014_2017_data$city_or_county),]
acushnet_gv <- acushnet_time_gv %>% group_by(date) %>% mutate(
                    n_killed = sum(n_killed),
                    n_injured = sum(n_injured), 
                    n_affected = n_killed + n_injured,
                    alcohol_related = sum(alcohol_related),
                    accidental = sum(accidental),
                    drug_related = sum(drug_related),
                    vehicular = sum(vehicular),
                    officer_related = sum(officer_related),
                    resulted_in_suicide = sum(resulted_in_suicide),
                    multiple_involved = sum(multiple_involved),
                    female_involved = sum(female_involved),
                    male_involved = sum(male_involved),
                    child_participant = sum(child_participant),
                    teen_participant = sum(teen_participant),
                    adult_participant = sum(adult_participant),
                    led_to_arrest = sum(led_to_arrest),
                    led_to_death = sum(led_to_death),
                    led_to_injury = sum(led_to_injury),
                    robbery_related = sum(robbery_related),
                    domestic_related = sum(domestic_related),
                    gang_related = sum(domestic_related),
                    defense_related = sum(defense_related),
                    school_related = sum(school_related),
                    year = case_when(
                        grepl("^2014",date) ~ 2014,
                        grepl("^2015",date) ~ 2015,
                        grepl("^2016",date) ~ 2016,
                        grepl("^2017",date) ~ 2017,
                        TRUE ~ 12
                      )
                    )
# Animated Bubble Chart Persons Killed and Affected
ani_bubble_chart <- ggplot(acushnet_gv, aes(n_killed, n_affected, size = n_injured, frame = year)) +
  geom_point() +
  geom_smooth(aes(group = year), 
              method = "lm", 
              show.legend = FALSE) +
  scale_x_log10()

gganimate(ani_bubble_chart)

#ver N_affected, N_killed, N_injured Time Series
n_affected_over_time <- acushnet_gv %>%
  select(date, n_killed, n_injured, n_affected) %>%
  gather(key = "variable", value = "value", -date)

ggplot(n_affected_over_time, aes(x = date, y = value)) + 
  geom_line(aes(color = variable), size = 1) +
  scale_color_manual(values = c("#00AFBB", "#E7B800", "#FC4E07")) +
  theme_minimal()

#Incidents resulting in Suicide, Death, Arrest and Injury
g_b_s_result <- acushnet_gv %>% group_by(year) %>% 
   summarise(
     resulted_in_suicide = sum(resulted_in_suicide),
     led_to_death = sum(led_to_death),
     led_to_arrest = sum(led_to_arrest),
     led_to_injury = sum(led_to_injury)
     )
#Want to find the most complete data for graph visualization purposes
g_b_s_zero_values <- g_b_s_result[g_b_s_result$led_to_death == 0 | g_b_s_result$led_to_injury ==0 |g_b_s_result$led_to_arrest ==0,]
g_b_s_result <- anti_join(g_b_s_result,g_b_s_zero_values)
g_b_s_result <- g_b_s_result[order(g_b_s_result$led_to_death),]
g_b_s_result <- tail(g_b_s_result)
g_b_s_result <- g_b_s_result%>%
  select(year, resulted_in_suicide,led_to_death,led_to_arrest,led_to_injury) %>%
  gather(key = "variable", value = "value", -year)

ggplot(g_b_s_result, aes(x = year, y = value, fill=variable), xlab="State", ylab="Number of Incidents") + 
  geom_bar(stat="identity", width = .5, position="dodge")

#Which age group did the participant(s) for gun related incidents belong to
g_b_s_age_group <- acushnet_gv %>% group_by(year) %>% 
   summarise(
     child_participant = sum(child_participant),
     teen_participant = sum(teen_participant),
     adult_participant = sum(adult_participant)
     )
g_b_s_age_group <- g_b_s_age_group[order(g_b_s_age_group$child_participant),]
g_b_s_age_group <- tail(g_b_s_age_group)
g_b_s_age_group <- g_b_s_age_group %>%
  select(year, child_participant,teen_participant) %>%
  gather(key = "variable", value = "value", -year)

ggplot(g_b_s_age_group, aes(x = year, y = value, fill=variable), xlab="State", ylab="Number of Incidents") + 
  geom_bar(stat="identity", width = .5, position="dodge")
#Incidents that were school related, domestic in nature, or self defense related
g_b_s_related <- acushnet_gv %>% group_by(year) %>% 
   summarise(
     school_related = sum(school_related),
     domestic_related = sum(domestic_related),
     defense_related = sum(defense_related)
     )
g_b_s_related <- g_b_s_related[order(g_b_s_related$defense_related),]
g_b_s_related <- tail(g_b_s_related)
g_b_s_related <- g_b_s_related %>%
  select(year, school_related, domestic_related, defense_related) %>%
  gather(key = "variable", value = "value", -year)

ggplot(g_b_s_related, aes(x = year, y = value, fill=variable), xlab="State", ylab="Number of Incidents") + 
  geom_bar(stat="identity", width = .5, position="dodge")

#NEXT: 2. Honey Production in the USA