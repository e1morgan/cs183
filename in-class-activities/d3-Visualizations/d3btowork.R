library(choroplethr)
#api.key.install(key="76d2481df0d565b7a3d9c2090a1591639fe5f432")
df=as.data.frame(get_acs_data("B08101","county",column_idx=41))
final<-as.data.frame(cbind(df$df.region,df$df.value))
colnames(final)<-c("id","rate")
write.table(final,"towork.tsv",sep="\t",row.names=FALSE)
