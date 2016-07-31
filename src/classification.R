library(xgboost)
source("src/params.R")

create_set <-function(data){
	dt = data[,list(phone_brand, device_model)]
	if ("group" %in% colnames(data)){
		dt$group = data$group
		dt = replace_clases_with_numbers(dt)
	}
	rownames(dt) = rownames(data)
	return(dt)
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

create_model <-function(data, params = get_initial_params()){
	train = data[, !c("group"), with = FALSE]
	labels = data[, group]
	train = sparse.model.matrix(~.-1,data = train)
	model = xgboost(data = train, label = labels, params = params, 
					num_class = 12, nthread = 7, nrounds = 2,  
					objective = "multi:softmax", eval_metrics = "mlogloss")
	return(model)
}