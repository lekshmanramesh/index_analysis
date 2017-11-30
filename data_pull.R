#Ticker Names of stocks of the index
ticker_list=c('AI.PA',
              'ALV.DE',
              'ABI.BR',
              'MTS.MC',
              'ASML.AS',
              'G.MI',
              'CS.PA',
              'BBVA.MC',
              'BBVA.MC',
              'BAS.F',
              'BAYN.DE',
              'BMW.DE',
              'BNP.PA',
              'CA.PA',
              'CRH.L',
              'DAI.F',
              'BN.PA',
              'DBK.DE',
              'DBK.DE',
              'ENEL.MI',
              'ENGI.PA',
              'ENGI.PA',
              'ENI.MI',
              'EOAN.F',
              'EI.PA',
              'IBE.MC',
              'ITX.MC',
              'INGA.AS',
              'ISP.MI',
              'OR.PA',
              'MC.PA',
              'MUV2.DE',
              'ORA.PA',
              'PHIA.AS',
              'REP.F',
              'RWE.F',
              'SGO.PA',
              'SAN.PA',
              'SAP.F',
              'SU.PA',
              'SIE.F',
              'GLE.PA',
              'TEF.MC',
              'FP.PA',
              'UL.AS',
              'UCG.MI',
              'UNA.AS',
              'DG.PA',
              'VIV.PA',
              'VOW.F'
)


final_df=data.frame(matrix(nrow=1252,ncol=0))
# We will start with the last 1252 values available

for (i in (1:length(ticker_list))){
	security=ticker_list[i]
	getSymbols(security, src="yahoo", from="2013-01-01", to="2017-11-21")
	df=eval(parse(text=paste0(security,"[,6]")))
	df1=as.data.frame(df)
	df2=data.frame(dates=rownames(df1),values=df)
	if (i == 1){
		final_df=data.frame(dates=rownames(df1),values=df)	
	}
	else {
		final_df=merge(x=final_df,y=df2, all.x=T,by="dates")	
	}
}

dates=rownames(df)
collist=c("dates",ticker_list)
colnames(final_df)=collist