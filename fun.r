mape=function(district_id,timeslot,actual,pred){
  dt=data.table(district_id,timeslot,actual,pred)
  dt[,ape:=ifelse(actual==0,0,abs(pred-actual)/actual)]
  mean(dt[,mean(ape),by=.(district_id)]$V1)
}


mapeObj=function(preds,dtrain){
  gaps=getinfo(dtrain,'label')
  delta=(preds-gaps)/gaps
  k=6
  exp.delta=exp(k*delta)
  grad=(exp.delta-1/exp.delta)/(exp.delta+1/exp.delta)
  grad[which((delta)>1)]=1
  grad[which((delta)< -1)]=-1
  grad=grad/gaps
  hess=4*k/(exp.delta+1/exp.delta)^2/(gaps^2)
  hess[which(abs(delta)>1)]=0
  
  grad[which(gaps==0)]=0
  hess[which(gaps==0)]=0
  #browser()
  return(list(grad = grad, hess = hess))
}

evalMAPE = function(preds, dtrain) {
  gaps = getinfo(dtrain, "label")
  err <- abs(gaps-preds)/gaps
  err[which(gaps==0)]=0
  err=mean(err)
  return(list(metric = "error", value = err))
}
