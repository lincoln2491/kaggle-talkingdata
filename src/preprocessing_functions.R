library(tidyr)
library(reshape2)
split_timestamp <- function(data){
	data = separate(data, col = timestamp, into = c("date", "time"), sep = " ")
	data = separate(data, col = time, into = c("hours", "minutes", "seconds"), sep = ":")
	data$hours = as.numeric(data$hours)
	data$minutes = as.numeric(data$minutes)
	data$seconds = as.numeric(data$seconds)
	return(data)
}

load_data <- function(is_train = TRUE){
	if(is_train){
		gender_age = fread("data/gender_age_train.csv")
	}
	else{
		gender_age = fread("data/gender_age_test.csv")
	}
	phone_brand_device_model = fread("data/phone_brand_device_model.csv")
	gender_age_devices = merge(x = gender_age, y = phone_brand_device_model, 
							   by = "device_id", all.x = TRUE)
	print("gender_age_devices created")
	rm(gender_age)
	rm(phone_brand_device_model)
	gc()
	
	events = fread("data/events.csv")
	ids = unique(gender_age_devices$device_id)
	events = events[device_id %in% ids]
	gender_events = merge(x = gender_age_devices, y = events, by = "device_id", all.x = TRUE)
	print("gender_events created")
	rm(ids)
	rm(events)
	rm(gender_age_devices)
	gc()
	
	app_events = fread("data/app_events.csv")
	ids = unique(gender_events$event_id)
	app_events = app_events[event_id %in% ids]
	gender_app_evets = merge(x = gender_events, y = app_events, by = "event_id", all.x = TRUE)
	print("gender_app_evets created")
	rm(ids)
	rm(app_events)
	rm(gender_events)
	gc()
	
	
	# ids = unique(gender_app_evets$app_id)
	# app_labels = fread("data/app_labels.csv")
	# app_labels = app_labels[app_id %in% ids]
	# label_categories = fread("data/label_categories.csv")
	# app_labels_categories = merge(app_labels, label_categories, by = "label_id", all.x = TRUE)
	# app_labels_categories$label_id = NULL
	# print("app_labels_categories created")
	# rm(app_labels)
	# rm(ids)
	# rm(label_categories)
	# gc()
	# 
	# labels = unique(app_labels_categories$category)
	# app_categories = data.table(app_id = unique(app_labels_categories$app_id))
	# for(label in labels){
	# 	app_categories[[label]] = ifelse(app_categories$app_id %in% app_labels_categories$app_id[app_labels_categories$category == label], 1, 0)
	# }
	# print("app_categories created")
	# rm(app_labels_categories)
	# gc()
	# 
	# df = merge(x = gender_app_evets, y = app_categories, by = "app_id", all.x =  TRUE)
	# print("df created")
	# rm(app_categories)
	# rm(gender_app_evets)
	# gc()
	# 
	return(gender_app_evets)
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

merge_data <- function(gender_age, phone_brand_device_model, app_labels, label_categories, app_events, events){
	app_labels_categories = merge(app_labels, label_categories, by = "label_id", all.x = TRUE)
	app_labels_categories$label_id = NULL
	print("app_labels_categories created")
	rm(app_labels)
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
	
	
	app_events_categories = merge(app_events, app_categories, by = "app_id", all.x = TRUE)
	print("app_events_categories created")
	rm(app_events)
	rm(app_categories)
	gc()
	
	gender_age_devices = merge(x = gender_age, y = phone_brand_device_model, 
				by = "device_id", all.x = TRUE)
	print("gender_age_devices created")
	rm(gender_age)
	rm(phone_brand_device_model)
	gc()
	
	events_app = merge(x =app_events_categories, y = events, by = "event_id", all.y = TRUE)
	print("events_app created")
	rm(app_events_categories)
	rm(events)
	gc()
	
	gender_events = merge(gender_age_devices, events_app, by = "device_id", all.x = TRUE)
	print("gender_events created")
	rm(gender_age_devices)
	rm(events_app)
	gc()
	
	
	return(gender_events)
}