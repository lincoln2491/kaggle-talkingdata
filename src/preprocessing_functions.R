library(tidyr)
library(reshape2)
library(dplyr)

split_timestamp <- function(data){
	data = separate(data, col = timestamp, into = c("date", "time"), sep = " ")
	data = separate(data, col = time, into = c("hour", "minute", "second"), sep = ":")
	data$hour = as.numeric(data$hour)
	data$minute = as.numeric(data$minute)
	data$second = as.numeric(data$second)
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
	
	sum[sum == Inf] = NA
	sum[sum == -Inf] = NA
	sum[sum == NaN] = NA
	return(sum)
}

get_time_statistics <- function(events){
	events$timestamp = events$hour * 3600 + events$minute * 60 + events$second
	
	sum = events[, .(min_hour = min(hour, na.rm = TRUE),
					 max_hour = max(hour, na.rm = TRUE),
					 mean_hour =  mean(hour, na.rm = TRUE),
					 sd_hour = sd(hour, na.rm = TRUE),
					 median_hour = median(hour, na.rm = TRUE),
					 min_min = min(minute, na.rm = TRUE),
					 max_min = max(minute, na.rm = TRUE),
					 mean_min =  mean(minute, na.rm = TRUE),
					 sd_min = sd(minute, na.rm = TRUE),
					 median_min = median(minute, na.rm = TRUE),
					 min_sec = min(second, na.rm = TRUE),
					 max_sec = max(second, na.rm = TRUE),
					 mean_sec =  mean(second, na.rm = TRUE),
					 sd_sec = sd(second, na.rm = TRUE),
					 median_sec = median(second, na.rm = TRUE),
					 min_tim = min(timestamp, na.rm = TRUE),
					 max_tim = max(timestamp, na.rm = TRUE),
					 mean_tim =  mean(timestamp, na.rm = TRUE),
					 sd_tim = sd(timestamp, na.rm = TRUE),
					 median_tim = median(timestamp, na.rm = TRUE)),
				 by = device_id]

	
	device_data = as.data.frame.matrix(table(events$device_id, events$date))
	device_data$device_id = rownames(device_data)
	device_data = as.data.table(device_data)
	
	device_hour = as.data.frame.matrix(table(events$device_id, events$hour))
	device_hour$device_id = rownames(device_hour)
	device_hour = as.data.table(device_hour)
	
	sum = merge(sum, device_data, by = "device_id")
	sum = merge(sum, device_hour, by = "device_id")
	
	sum[sum == Inf] = NA
	sum[sum == -Inf] = NA
	sum[sum == NaN] = NA
	
	return(sum)
}

load_data <-function(is_train = TRUE){
	phone_brand_device_model = fread("data/English_phone_brand_device_model.csv")
	phone_brand_device_model = phone_brand_device_model[duplicated(phone_brand_device_model, by = "device_id") == FALSE]
	phone_brand_device_model$phone_brand = as.factor(phone_brand_device_model$phone_brand)
	phone_brand_device_model$device_model = as.factor(phone_brand_device_model$device_model)
	phone_brand_device_model$device_id = as.character(phone_brand_device_model$device_id)
	
	events =  fread("data/events.csv")
	events$device_id = as.character(events$device_id)
	events = split_timestamp(events)
	events[(latitude < 10) & (latitude > -10) & (longitude > -10) & (longitude < 10), longitude := NA]
	events[(latitude < 10) & (latitude > -10), latitude := NA]
	
	events_count_table = table(events$device_id)
	events_count_table = as.data.table(events_count_table)
	colnames(events_count_table) = c("device_id", "count")
	events_na_count_table = table(events[is.na(latitude)]$device_id)
	events_na_count_table = as.data.table(events_na_count_table)
	colnames(events_na_count_table) = c("device_id", "na_count")
	
	if(is_train){
		gender_age = fread("data/gender_age_train.csv")
	}
	else{
		gender_age = fread("data/gender_age_test.csv")
	}
	gender_age$device_id = as.character(gender_age$device_id)
	gender_age = merge(x = gender_age, y = phone_brand_device_model,
							 by = "device_id", all.x = TRUE)
	rownames(gender_age) = gender_age$device_id
	
	gender_age = merge(gender_age, events_count_table, 
							 by = "device_id", all.x = TRUE)
	gender_age = merge(gender_age, events_na_count_table, 
					   by = "device_id", all.x = TRUE)
	geo_statistics = get_geo_statistics(events)
	gender_age = merge(gender_age, geo_statistics, by = "device_id", 
							 all.x = TRUE)
	
	time_statistics = get_time_statistics(events)
	gender_age = merge(gender_age, time_statistics, by = "device_id", all.x = TRUE)
	rm(events, phone_brand_device_model)
	gc()
	# gender_age = get_app_statistics(gender_age)
	return(gender_age)
}

rename_column_names2 = function(data){
	setnames(data,"X2016.04.30", "d20160430" )
	setnames(data,"X2016.05.01", "d20160501" )
	setnames(data,"X2016.05.02", "d20160502" )
	setnames(data,"X2016.05.03", "d20160503" )
	setnames(data,"X2016.05.04", "d20160504" )
	setnames(data,"X2016.05.05", "d20160505" )
	setnames(data,"X2016.05.06", "d20160506" )
	setnames(data,"X2016.05.07", "d20160507" )
	setnames(data,"X2016.05.08", "d20160508" )
	setnames(data,"X0", "h0" )
	setnames(data,"X1", "h1" )
	setnames(data,"X2", "h2" )
	setnames(data,"X3", "h3" )
	setnames(data,"X4", "h4" )
	setnames(data,"X5", "h5" )
	setnames(data,"X6", "h6" )
	setnames(data,"X7", "h7" )
	setnames(data,"X8", "h8" )
	setnames(data,"X9", "h9" )
	setnames(data,"X10", "h10" )
	setnames(data,"X11", "h11" )
	setnames(data,"X12", "h12" )
	setnames(data,"X13", "h13" )
	setnames(data,"X14", "h14" )
	setnames(data,"X15", "h15" )
	setnames(data,"X16", "h16" )
	setnames(data,"X17", "h17" )
	setnames(data,"X18", "h18" )
	setnames(data,"X19", "h19" )
	setnames(data,"X20", "h20" )
	setnames(data,"X21", "h21" )
	setnames(data,"X22", "h22" )
	setnames(data,"X23", "h23" )
	return(data)
}

rename_column_names = function(data){
	setnames(data,"2016-04-30", "d20160430" )
	setnames(data,"2016-05-01", "d20160501" )
	setnames(data,"2016-05-02", "d20160502" )
	setnames(data,"2016-05-03", "d20160503" )
	setnames(data,"2016-05-04", "d20160504" )
	setnames(data,"2016-05-05", "d20160505" )
	setnames(data,"2016-05-06", "d20160506" )
	setnames(data,"2016-05-07", "d20160507" )
	setnames(data,"2016-05-08", "d20160508" )
	setnames(data,"0", "h0" )
	setnames(data,"1", "h1" )
	setnames(data,"2", "h2" )
	setnames(data,"3", "h3" )
	setnames(data,"4", "h4" )
	setnames(data,"5", "h5" )
	setnames(data,"6", "h6" )
	setnames(data,"7", "h7" )
	setnames(data,"8", "h8" )
	setnames(data,"9", "h9" )
	setnames(data,"10", "h10" )
	setnames(data,"11", "h11" )
	setnames(data,"12", "h12" )
	setnames(data,"13", "h13" )
	setnames(data,"14", "h14" )
	setnames(data,"15", "h15" )
	setnames(data,"16", "h16" )
	setnames(data,"17", "h17" )
	setnames(data,"18", "h18" )
	setnames(data,"19", "h19" )
	setnames(data,"20", "h20" )
	setnames(data,"21", "h21" )
	setnames(data,"22", "h22" )
	setnames(data,"23", "h23" )
	return(data)
}

get_app_statistics <- function(gender_age){
	events = fread("data/events.csv")
	events$device_id = as.character(events$device_id)
	events = events[device_id %in% gender_age$device_id]
	app_events = fread("data/app_events.csv")
	app_labels = fread("data/app_labels.csv")
	app_labels$label_id = as.factor(paste("l", app_labels$label_id, sep = ""))
	ids = sort(unique(app_labels$label_id))
	for(id in ids){
		app_labels[[id]] = as.numeric(NA)
	}
	for (id in ids) {
		app_labels[label_id ==id, (id):=1]
	}
	app_labels[, label_id := NULL]
	app_labels = app_labels[, lapply(.SD, sum, na.rm = TRUE), by = app_id]
	app_labels = app_labels[app_id %in% app_labels$app_id]
	gc()
	# label_categories = fread("data/label_categories.csv")
	# labels = merge(app_labels, label_categories, by = "label_id")
	gender_age$number_of_apps = 0
	count =0
	print(length(unique(events$device_id)))
	app_stats = NULL
	for (device in unique(events$device_id)) {
		tmp_evets = events[device_id == device]
		tmp_app_events = app_events[event_id %in% tmp_evets$event_id]
		if(nrow(tmp_app_events) == 0){
			next
		}
		tmp_number_of_apps = length(unique(tmp_app_events$app_id))
		gender_age[device_id == device, number_of_apps := tmp_number_of_apps]
		tmp_app_labels = app_labels[app_id %in% tmp_app_events$app_id]
		tmp_app_labels[,app_id := NULL]
		app_sum = colSums(tmp_app_labels, na.rm = TRUE)
		app_sum = c(device_id = device, app_sum)
		if(is.null(app_stats)){
			app_stats = app_sum
		}
		else{
			app_stats = rbind(app_stats, app_sum)
		}
		count = count + 1
		if(count %% 100 == 0){
			print(count)
		}
	}
	rm(app_labels, events, app_events)
	gc()
	app_stats = as.data.table(app_stats)
	for(id in ids){
		app_stats[[id]] = as.numeric(app_stats[[id]])
	}
	gender_age = merge(gender_age, app_stats, by = "device_id", all.x = TRUE)
	for(id in ids){
		gender_age[is.na(gender_age[[id]]), (id) := 0]
	}
	# rm(app_labels)
	gc()
	return(gender_age)
}