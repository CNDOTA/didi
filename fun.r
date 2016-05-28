mape=function(district_id,timeslot,actual,pred){
  dt=data.table(district_id,timeslot,actual,pred)
  dt[,ape:=ifelse(actual==0,0,abs(pred-actual)/actual)]
  mean(dt[,mean(ape),by=.(district_id)]$V1)
}
