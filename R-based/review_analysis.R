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

#3.Grammar Product Reviews
review_data <- data.frame(read.csv("grammarandproductreviews.csv", header = TRUE, sep = ","))
summary(review_data)
categories_reviews <- review_data$categories
summary(categories_reviews)
review_data$date <- as.Date(review_data$reviews.date)
did_purchase <- review_data$reviews.didPurchase
summary(did_purchase)
do_recommend <- review_data$reviews.doRecommend
summary(do_recommend)
ratings <- review_data$reviews.rating
summary(ratings)
#Pessimistic imputation of DidPurchase
#Based on premise that even if you use/rate
#an item it doesnt mean you purchased it
#Pessimistic doesnt work so instead use random
#Imputation of do Reccomend based on rating
#If rating of 4/5 it is an unsafe assumption
#that the rater would reccomend the item
review_data_mutate <- review_data %>% mutate(
							categories = categories,
							didPurchase = ifelse(is.na(reviews.didPurchase),sample(0:1,1),reviews.didPurchase),
							doRecommend = case_when(
											reviews.rating == 5 ~ TRUE,
											reviews.rating == 4 ~ TRUE,
											TRUE ~ FALSE
											),
							title = reviews.title,
							text = reviews.text,
							rating = reviews.rating,
							date = date,
							id = id,
							brand = brand,
							name = name,
							manufacturer = manufacturer,
							positive_words_used = case_when(
													grepl("*good*",reviews.text, ignore.case = TRUE) ~ 1,
													grepl("*great*",reviews.text, ignore.case = TRUE) ~ 1,
													grepl("*best*",reviews.text, ignore.case = TRUE) ~ 1,
													grepl("*excellent*",reviews.text, ignore.case = TRUE) ~ 1,
													TRUE ~0
								),
							no_positive_words = case_when(
													positive_words_used == 1 ~ 0,
													TRUE ~ 1
								),
							des_category = case_when(
													grepl("*music*",categories, ignore.case = TRUE) ~ "Music",
													grepl("*personal*",categories, ignore.case = TRUE) ~ "Personal Care",
													grepl("*food*", categories, ignore.case = TRUE) ~ "Food",
													TRUE ~ "Other"
								)
	)
review_data <- review_data_mutate %>% select(
							categories = categories,
							didPurchase = didPurchase,
							doRecommend = doRecommend,
							title = title,
							text = text,
							rating = rating,
							date = date,
							id = id,
							brand = brand,
							name = name,
							manufacturer = manufacturer,
							positive_words_used = positive_words_used,
							no_positive_words = no_positive_words,
							des_category = des_category
	)
#Ratings
ratings_data <- review_data %>% group_by(rating) %>% summarise(
							didPurchase = sum(didPurchase),
							doRecommend = sum(doRecommend),
							positive_words_used = sum(positive_words_used),
							no_positive_words = sum(no_positive_words),
	)

ratings_result <- ratings_data %>%
  select(rating, didPurchase, doRecommend, positive_words_used) %>%
  gather(key = "variable", value = "value", -rating)

ggplot(ratings_result, aes(x = rating, y = value, fill=variable), xlab="Rating") + 
  geom_bar(stat="identity", width = .5, position="dodge")

#category
category_data <- review_data %>% group_by(des_category) %>% summarise(
							didPurchase = sum(didPurchase),
							doRecommend = sum(doRecommend),
							positive_words_used = sum(positive_words_used),
							no_positive_words = sum(no_positive_words),
							rating = mean(rating)
	)

category_result <- category_data %>%
  select(des_category, didPurchase, doRecommend, positive_words_used) %>%
  gather(key = "variable", value = "value", -des_category)

ggplot(category_result, aes(x = des_category, y = value, fill=variable), xlab="Rating") + 
  geom_bar(stat="identity", width = .5, position="dodge")

category_rating <- category_data %>%
  select(des_category, rating) %>%
  gather(key = "variable", value = "value", -des_category)

ggplot(category_rating, aes(x = des_category, y = value, fill=variable), xlab="Manufacturer") + 
  geom_bar(stat="identity", width = .5, position="dodge")

#name
name_data <- review_data %>% group_by(name) %>% summarise(
							didPurchase = sum(didPurchase),
							doRecommend = sum(doRecommend),
							positive_words_used = sum(positive_words_used),
							no_positive_words = sum(no_positive_words),
							rating = mean(rating)
	)
random_names <- name_data[sample(nrow(name_data), 5), ]
name_result <- random_names %>%
  select(name, didPurchase, doRecommend, positive_words_used) %>%
  gather(key = "variable", value = "value", -name)

ggplot(name_result, aes(x = name, y = value, fill=variable), xlab="Name") + 
  geom_bar(stat="identity", width = .5, position="dodge")

name_rating <- random_names %>%
  select(name, rating) %>%
  gather(key = "variable", value = "value", -name)

ggplot(name_rating, aes(x = name, y = value, fill=variable), xlab="Manufacturer") + 
  geom_bar(stat="identity", width = .5, position="dodge")

#manufacturer
manufacturer_data <- review_data %>% group_by(manufacturer) %>% summarise(
							didPurchase = sum(didPurchase),
							doRecommend = sum(doRecommend),
							positive_words_used = sum(positive_words_used),
							no_positive_words = sum(no_positive_words),
							rating = mean(rating)
	)

random_man <- manufacturer_data[sample(nrow(manufacturer_data), 5), ]
man_result <- random_man %>%
  select(manufacturer, didPurchase, doRecommend, positive_words_used) %>%
  gather(key = "variable", value = "value", -manufacturer)

ggplot(man_result, aes(x = manufacturer, y = value, fill=variable), xlab="Manufacturer") + 
  geom_bar(stat="identity", width = .5, position="dodge")

man_rating <- random_man %>%
  select(manufacturer, rating) %>%
  gather(key = "variable", value = "value", -manufacturer)

ggplot(man_rating, aes(x = manufacturer, y = value, fill=variable), xlab="Manufacturer") + 
  geom_bar(stat="identity", width = .5, position="dodge")

#brand
brand_data <- review_data %>% group_by(brand) %>% summarise(
							didPurchase = sum(didPurchase),
							doRecommend = sum(doRecommend),
							positive_words_used = sum(positive_words_used),
							no_positive_words = sum(no_positive_words),
							rating = mean(rating)
	)
random_brand <- brand_data[sample(nrow(brand_data), 5), ]
brand_result <- random_brand %>%
  select(brand, didPurchase, doRecommend, positive_words_used) %>%
  gather(key = "variable", value = "value", -brand)

ggplot(brand_result, aes(x = brand, y = value, fill=variable), xlab="Manufacturer") + 
  geom_bar(stat="identity", width = .5, position="dodge")

brand_rating <- random_brand %>%
  select(brand, rating) %>%
  gather(key = "variable", value = "value", -brand)

ggplot(brand_rating, aes(x = brand, y = value, fill=variable), xlab="Manufacturer") + 
  geom_bar(stat="identity", width = .5, position="dodge")

#NEXT: 4. Insightful and Vast USA Statistics 