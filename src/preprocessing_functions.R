library(tidyr)
library(reshape2)
library(dplyr)

split_timestamp <- function(data){
	data = separate(data, col = timestamp, into = c("date", "time"), sep = " ")
	data = separate(data, col = time, into = c("hours", "minutes", "seconds"), sep = ":")
	data$hours = as.numeric(data$hours)
	data$minutes = as.numeric(data$minutes)
	data$seconds = as.numeric(data$seconds)
	return(data)
}

load_app_labels <- function (){
	app_labels = fread("data/app_labels.csv")
	label_categories = fread("data/label_categories.csv")
	app_labels_categories = merge(app_labels, label_categories, by = "label_id", all.x = TRUE)
	app_labels_categories$label_id = NULL
	print("app_labels_categories created")
	rm(app_labels)
	rm(ids)
	rm(label_categories)
	gc()

	labels = unique(app_labels_categories$category)
	app_categories = data.table(app_id = unique(app_labels_categories$app_id))
	for(label in labels){
		app_categories[[label]] = ifelse(app_categories$app_id %in% app_labels_categories$app_id[app_labels_categories$category == label], 1, 0)
	}
	print("app_categories created")
	rm(app_labels_categories)
	gc()

	return(app_categories)
}

get_geo_statistics <- function(events){
	sum = events[, .(min_lon = min(longitude, na.rm = TRUE), 
					 max_lon = max(longitude, na.rm = TRUE), 
					 mean_lon =  mean(longitude, na.rm = TRUE), 
					 sd_lon = sd(longitude, na.rm = TRUE), 
					 median_lon = median(longitude, na.rm = TRUE), 
					 min_lat = min(latitude, na.rm = TRUE), 
					 max_lat = max(latitude, na.rm = TRUE), 
					 mean_lat =  mean(latitude, na.rm = TRUE), 
					 sd_lat = sd(latitude, na.rm = TRUE),
					 median_lat = median(latitude, na.rm = TRUE)), 
				 by = device_id]
	return(sum)
}