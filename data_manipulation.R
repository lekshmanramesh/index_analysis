# x represents every numeric column in the data frame
logreturns=function(x){
	y=vector(length=length(x)-1)
	for (i in 1:(length(x)-1)){
		y[i]=log(x[i+1]/x[i])
	}
	return(y)
}
# run the sd returns function on the logreturns output data frame
sdreturns=function(x){
	y=vector(length=length(x)-59)
	for (i in 1:(length(x)-59)){
		y[i]=sd(x[i:(i+59)])
	}
	return(y)
}
# once the sd returns data(sd_data) frame is ready.. calculate 
# 1) sd in stressed scenarios(Row 1)
# 2) standard error of model(Row 2)
# 3) R squared of model(Row 3)

stressed_sd=function(x){
	mod1=lm(x~index,data=sd_data)
	y=predict(mod1,data.frame(index=0.046))
	sig=summary(mod1)$sigma
	rsq=summary(mod1)$r.squared
	return(c(y,sig,rsq))
}


# input numeric log returns data
# Outputs 3 matrices with predicted correlations, standard errors and r squared like stressed_sd
correlations=function(x){
	output=matrix(1,50,50)
	output_sig=matrix(1,50,50)
	output_rsq=matrix(1,50,50)
	for (i in 1:49){
		for (j in (i+1):50){
			corr=vector(length=nrow(x)-59)
			ind=sdreturns(x[,ncol(x)])
			for (k in 1:(nrow(x)-59)){
				var1=x[k:(k+59),i]
				var2=x[k:(k+59),j]
				corr[k]=cor(var1,var2)
			}
			mod1=lm(corr~ind)
			y=predict(mod1,data.frame(ind=0.046))
			sig=summary(mod1)$sigma
			rsq=summary(mod1)$r.squared
			output[i,j]=min(0.95,y) #Capped at 0.95(randomly chosen)
			output_sig[i,j]=sig
			output_rsq[i,j]=rsq
		}
	}
	return(list(output,output_sig,output_rsq))
}

#Enter correlation dataframe x and get repaired output
# Requires packages Matrix and matrixcalc
repair_matrix=function(x){
	for (i in 1:nrow(x)){
	  for (j in 1:i){
		x[i,j]=x[j,i]
	  }
	}
	
	if (is.positive.semi.definite(x)!=T){
		y=nearPD(x,corr=T)
		y=as.matrix(y$mat)
	}
	return(y)
}
