library(xgboost)
library(Matrix)
library(MLmetrics)
source("src/params.R")

create_set <- function(data){
	dt = data[,list(device_id ,
					phone_brand, device_model , count, na_count,
					min_lon, max_lon, mean_lon , sd_lon,
					median_lon , min_lat, max_lat, mean_lat,
					sd_lat , median_lat , min_hour , max_hour,
					mean_hour, sd_hour, median_hour, min_min ,
					max_min, mean_min , sd_min , median_min,
					min_sec, max_sec, mean_sec , sd_sec,
					median_sec , min_tim, max_tim, mean_tim,
					sd_tim , median_tim , d20160430, d20160501 ,
					d20160502, d20160503, d20160504, d20160505 ,
					d20160506, d20160507, d20160508, h0,
					h1 , h2 , h3 , h4,
					h5 , h6 , h7 , h8,
					h9 , h10, h11, h12 ,
					h13, h14, h15, h16 ,
					h17, h18, h19, h20 ,
					h21, h22, h23, number_of_apps,
					l10, l100 , l1000, l1001 ,
					l1002, l1003, l1005, l1006 ,
					l1007, l1008, l1009, l101,
					l1010, l1011, l1012, l1013 ,
					l1014, l1015, l1016, l1017 ,
					l1018, l1019, l102 , l1020 ,
					l1021, l103 , l104 , l105,
					l11, l12, l128 , l129,
					l13, l130 , l131 , l132,
					l134 , l135 , l136 , l137,
					l138 , l139 , l14, l140,
					l141 , l142 , l144 , l146,
					l147 , l148 , l149 , l150,
					l151 , l152 , l153 , l154,
					l155 , l156 , l157 , l158,
					l159 , l16, l160 , l161,
					l162 , l163 , l164 , l166,
					l167 , l168 , l169 , l17 ,
					l170 , l172 , l173 , l174,
					l175 , l177 , l178 , l179,
					l18, l180 , l181 , l183,
					l184 , l185 , l186 , l187,
					l188 , l189 , l19, l190,
					l191 , l192 , l193 , l194,
					l195 , l197 , l198 , l199,
					l2 , l20, l200 , l201,
					l203 , l204 , l205 , l206,
					l207 , l209 , l21, l210,
					l211 , l212 , l213 , l214,
					l215 , l216 , l217 , l218,
					l219 , l22, l220 , l221,
					l222 , l223 , l224 , l225,
					l226 , l227 , l229 , l23 ,
					l230 , l231 , l232 , l233,
					l234 , l235 , l236 , l237,
					l238 , l24, l240 , l241,
					l242 , l244 , l245 , l246,
					l247 , l249 , l25, l250,
					l251 , l252 , l253 , l254,
					l255 , l256 , l257 , l258,
					l259 , l26, l260 , l261,
					l262 , l263 , l265 , l266,
					l267 , l268 , l269 , l27 ,
					l270 , l271 , l272 , l273,
					l274 , l275 , l276 , l277,
					l279 , l280 , l281 , l282,
					l283 , l284 , l29, l30 ,
					l302 , l303 , l306 , l31 ,
					l316 , l317 , l318 , l32 ,
					l326 , l33, l35, l36 ,
					l37, l38, l39, l4,
					l40, l405 , l406 , l407,
					l41, l42, l43, l44 ,
					l45, l46, l47, l48 ,
					l49, l5 , l50, l51 ,
					l52, l53, l54, l548,
					l549 , l55, l551 , l552,
					l553 , l555 , l558 , l559,
					l56, l562 , l563 , l564,
					l565 , l566 , l568 , l57 ,
					l58, l59, l6 , l60 ,
					l61, l62, l63, l64 ,
					l65, l66, l67, l68 ,
					l688 , l689 , l69, l690,
					l691 , l692 , l694 , l695,
					l696 , l7 , l70, l704,
					l705 , l706 , l707 , l708,
					l709 , l71, l710 , l711,
					l712 , l713 , l714 , l715,
					l716 , l717 , l718 , l719,
					l72, l720 , l721 , l722,
					l723 , l724 , l73, l730,
					l731 , l732 , l734 , l735,
					l736 , l737 , l738 , l74 ,
					l740 , l744 , l747 , l748,
					l749 , l75, l751 , l752,
					l755 , l756 , l757 , l758,
					l759 , l76, l760 , l761,
					l762 , l763 , l765 , l77 ,
					l770 , l771 , l772 , l773,
					l774 , l775 , l776 , l777,
					l778 , l779 , l78, l780,
					l781 , l782 , l783 , l785,
					l786 , l787 , l788 , l789,
					l79, l790 , l791 , l792,
					l793 , l794 , l795 , l796,
					l797 , l798 , l8 , l80 ,
					l800 , l801 , l802 , l803,
					l804 , l805 , l806 , l807,
					l808 , l809 , l81, l810,
					l811 , l812 , l813 , l814,
					l815 , l816 , l817 , l818,
					l819 , l82, l820 , l821,
					l822 , l823 , l824 , l825,
					l826 , l827 , l828 , l829,
					l83, l839 , l84, l840,
					l841 , l842 , l843 , l844,
					l845 , l846 , l847 , l85 ,
					l854 , l855 , l856 , l857,
					l858 , l859 , l86, l860,
					l861 , l862 , l863 , l864,
					l865 , l866 , l867 , l868,
					l869 , l87, l870 , l871,
					l872 , l873 , l874 , l88 ,
					l89, l9 , l90, l909,
					l91, l910 , l916 , l917,
					l918 , l919 , l92, l920,
					l921 , l922 , l923 , l924,
					l925 , l926 , l927 , l928,
					l929 , l93, l930 , l931,
					l932 , l933 , l934 , l935,
					l936 , l937 , l938 , l939,
					l94, l940 , l941 , l942,
					l943 , l944 , l945 , l946,
					l947 , l948 , l949 , l95 ,
					l950 , l951 , l952 , l953,
					l954 , l955 , l956 , l957,
					l958 , l959 , l96, l960,
					l968 , l969 , l97, l970,
					l971 , l972 , l973 , l974,
					l975 , l976 , l977 , l978,
					l979 , l98, l980 , l981,
					l982 , l983 , l984 , l985,
					l986 , l987 , l988 , l989,
					l99, l990 , l991 , l992,
					l993 , l994 , l995 , l996,
					l997 , l998 , l999 
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
	data_partition = createDataPartition(data$group, p = 0.8, list = FALSE, times = 1)
	train_set = data[data_partition]
	test_set = data[-data_partition]
	tmp = create_set_and_labels(train_set)
	dtrain = xgb.DMatrix(data = tmp[[1]], label = tmp[[2]])
	
	tmp = create_set_and_labels(test_set)
	dtest = xgb.DMatrix(data = tmp[[1]], label = tmp[[2]])
	
	
	#train = data[, !c("group"), with = FALSE]
	#labels = data[, group]
	#train = sparse.model.matrix(~.-1,data = train)
	model = xgboost(data = dtrain, params = params, 
					num_class = 12, nthread = 7, nrounds = 70, early.stop.round = 10,
					watchlist = list(train=dtrain, test=dtest),
					objective = "multi:softprob", eval_metric = "mlogloss")
	return(model)
}

test_cv <- function(data, params = get_initial_params()){
	train = data[, !c("group"), with = FALSE]
	labels = data[, group]
	train = sparse.model.matrix(~.-1,data = train)
	
	model = xgb.cv(data = train, label = labels, params = params, 
					num_class = 12, nthread = 7, nrounds = 1500, nfold = 5,  
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
	
	eta_vals = c(0.01)
	num_rounds_vals = c(500)
	subsample_vals = c(0.5, 1)
	colsample_bytree_vals = c(0.6, 0.8, 1)
	max_depth_vals = c( 4, 6, 8)
	early_stopping_rounds_vals = c(50)
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
									early.stop.round = 50,
									watchlist = list(train=dtrain, test=dtest),
									objective = "multi:softprob",
									eval_metric = "mlogloss")
					pred = predict_model(model, test[, c(1:2), with = FALSE])
					mll = MultiLogLoss(pred, test$group)
					train_result = list(eta = eta_val, num_rounds = num_rounds_val,
										subsamle = subsamle_val, max_depth = max_depth_val,
										mll = mll)
					
					results[[length(results)+1]] <- train_result
					file_name = paste("out/cv4/", i, ".cv", sep = "")
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
	file_name = paste("out/cv4/", "mll", ".cv", sep = "")
	save(results, file = file_name)
	return(results)
}