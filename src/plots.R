plot_cv_results <-function(cv_results){
	tmp = cv_results$result
	if(cv_results$max_depth != 4 || cv_results$eta == 0.1){
		return()
	}
	tmp$id = as.numeric(rownames(tmp))
	tmp = tmp[,c(5, 1, 3), with = FALSE]
	tmp = melt(tmp, id.vars = "id")
	colnames(tmp) = c("id", "type", "error")
	p = ggplot(tmp, aes(x = id, y = error, colour = type)) + geom_point() + 
		ggtitle(paste("eta " , cv_results$eta, "subsamle", cv_results$subsamle, 
					  "max_depth", cv_results$max_depth)) + scale_y_continuous(limits = c(1.5, 3)) 
	print(p)
}