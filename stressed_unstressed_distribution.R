## Enter actual pd data

logreturns_data=read.csv("logreturns_data.csv",stringsAsFactors = F)

pd=rep(c(0.00069,0.00074,0.01176,0.023,0.0003),10)
stress_unstressed_ratio=function(pd,logreturns){
	a=NULL
	for (i in 1:50){
		default=qnorm(pd[i])
		correlation=cor(logreturns_data[i],logreturns_data$index)
		z1=rnorm(1000000)
		z2=correlation*z1+rnorm(1000000)*sqrt(1-correlation^2)
		df=data.frame(sec=z1,ind=z2)
		df=df[order(df$ind),]
		df$def=ifelse(df$sec<default,1,0)
		df_stressed=df[1:10000,]
		df_unstressed=df[10001:1000000,]
		def_stressed=sum(df_stressed$def)
		def_unstressed=sum(df_unstressed$def)
		b=data.frame(var=colnames(logreturns)[i], stressed_defaults=def_stressed, 
		unstressed_defaults=def_unstressed, correlation=correlation,sim_cor=cor(z1,z2), pd=default)
		a=rbind(a,b)
	}
	return(a)
}