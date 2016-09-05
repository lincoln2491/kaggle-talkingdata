get_initial_params = function(){
	params = list(
	)
	return(params)
}

model_2_params = function(){
	params = list(
		"eta" = 0.01,
		"subsample" = 1.0
	)
	return(params)
}

model_3_params = function(){
	params = list(
		"eta" = 0.01,
		"subsample" = 1.0,
		"max_depth" = 8
	)
	return(params)
}

model_4_params = function(){
	params = list(
		"eta" = 0.1,
		"subsample" = 1.0,
		"max_depth" = 8
	)
	return(params)
}

model_5_params = function(){
	params = list(
		"eta" = 0.1,
		"subsample" = 1.0,
		"max_depth" = 16
	)
	return(params)
}