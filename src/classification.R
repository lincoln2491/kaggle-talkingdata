library(xgboost)
library(Matrix)
library(MLmetrics)
source("src/params.R")

create_set <- function(data){
	dt = data[,list(phone_brand, device_model, count, 
					min_lon, max_lon, mean_lon, sd_lon, median_lon, 
					min_lat, max_lat, mean_lat, sd_lat, median_lat, 
					min_hour, max_hour, mean_hour, sd_hour,median_hour,
					min_min, max_min, mean_min, sd_min, median_min,
					min_sec, max_sec, mean_sec, sd_sec, median_sec,
					min_tim, max_tim, mean_tim, sd_tim, median_tim,
					d20160430, d20160501, d20160502, d20160503, d20160504,
					d20160505, d20160506, d20160507, d20160508,
					h0, h1, h2, h3, h4, h5, h6, h7, h8, h9, h10, h11, h12, h13, 
					h14, h15, h16, h17, h18, h19, h20, h21, h22, h23
	)]
	if ("group" %in% colnames(data)){
		dt$group = data$group
		dt = replace_clases_with_numbers(dt)
	}
	rownames(dt) = rownames(data)
	return(dt)
}

create_set_and_labels <- function(data){
	set = data[, !c("group"), with = FALSE]
	labels = data[, group]
	set = sparse.model.matrix(~.-1,data = set)
	return(list(set, labels))
}

replace_clases_with_numbers <- function(data){
	data$group = replace(data$group, data$group == "F23-", 0)
	data$group = replace(data$group, data$group == "F24-26", 1)
	data$group = replace(data$group, data$group == "F27-28", 2)
	data$group = replace(data$group, data$group == "F29-32", 3)
	data$group = replace(data$group, data$group == "F33-42", 4)
	data$group = replace(data$group, data$group == "F43+", 5)
	data$group = replace(data$group, data$group == "M22-", 6)
	data$group = replace(data$group, data$group == "M23-26", 7)
	data$group = replace(data$group, data$group == "M27-28", 8)
	data$group = replace(data$group, data$group == "M29-31", 9)
	data$group = replace(data$group, data$group == "M32-38", 10)
	data$group = replace(data$group, data$group == "M39+", 11)
	return(data)
}

create_model <- function(data, params = get_initial_params()){
	train = data[, !c("group"), with = FALSE]
	labels = data[, group]
	train = sparse.model.matrix(~.-1,data = train)
	model = xgboost(data = train, label = labels, params = params, 
					num_class = 12, nthread = 7, nrounds = 150,  
					objective = "multi:softprob", eval_metric = "mlogloss")
	return(model)
}

predict_model <- function(model, data){
	test = sparse.model.matrix(~.-1,data = data)
	predictions = predict(model, test)
	predictions = t(matrix(predictions, nrow=12, ncol=length(predictions)/12))
	predictions = as.data.table(predictions)
	predictions = cbind(rownames(test), predictions)
	# colnames(predictions) = c("device_id", "F23-", "F24-26", "F27-28", "F29-32", 
	# 						  "F33-42", "F43+", "M22-", "M23-26", "M27-28", 
	# 						  "M29-31", "M32-38", "M39+")
	predictions = predictions[,c(2:13), with = FALSE]
	colnames(predictions) = as.character( 0:11)
	# rownames(predictions) = rownames(test)
	return(predictions)
}


grid_search <- function(train, test){
	set.seed(666)
	start = Sys.time()
	print(start)
	tmp = create_set_and_labels(train)
	dtrain = xgb.DMatrix(data = tmp[[1]], label = tmp[[2]])
	
	tmp = create_set_and_labels(test)
	dtest = xgb.DMatrix(data = tmp[[1]], label = tmp[[2]])
	
	eta_vals = c(0.1, 0.01)
	num_rounds_vals = c(150)
	subsample_vals = c(0.3, 0.5, 0.7, 1)
	colsample_bytree_vals = c()
	max_depth_vals = c( 4, 6, 8, 10, 16)
	# early_stopping_rounds_vals = c()
	i = 0
	results = list()
	for( eta_val in eta_vals){
		for(num_rounds_val in num_rounds_vals){
			for(subsamle_val in subsample_vals){
				for(max_depth_val in max_depth_vals){
					print(i)
					set.seed(666)
					cv_model = xgb.cv(data = dtrain, #label = train_labels, 
									num_class = 12, nthread = 6, 
									eta = eta_val,
									max_depth = max_depth_val,
									nrounds = num_rounds_val,
									subsample = subsamle_val,
									objective = "multi:softprob",
									nfold = 5,
									eval_metric = "mlogloss")
					cv_results = list(eta = eta_val, num_rounds = num_rounds_val, 
									  subsamle = subsamle_val, max_depth = max_depth_val,
									  result = cv_model)
					
					set.seed(666)
					model = xgb.train(data = dtrain,
									num_class = 12, nthread = 6, 
								   	eta = eta_val,
									max_depth = max_depth_val,
									nrounds = num_rounds_val,
									subsample = subsamle_val,
									# watchlist = list(train=dtrain, test=dtest),
									objective = "multi:softprob",
									eval_metric = "mlogloss")
					pred = predict_model(model, test[, c(1:2), with = FALSE])
					mll = MultiLogLoss(pred, test$group)
					train_result = list(eta = eta_val, num_rounds = num_rounds_val,
										subsamle = subsamle_val, max_depth = max_depth_val,
										mll = mll)
					
					results[[length(results)+1]] <- train_result
					file_name = paste("out/cv2/", i, ".cv", sep = "")
					save(cv_results, file = file_name)
					i = i + 1
				}
			}
		}
		gc()
	}
	stop = Sys.time()
	print(start)
	print(stop)
	return(results)
}