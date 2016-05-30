mape=function(district_id,timeslot,actual,pred){
  dt=data.table(district_id,timeslot,actual,pred)
  dt[,ape:=ifelse(actual==0,0,abs(pred-actual)/actual)]
  mean(dt[,mean(ape),by=.(district_id)]$V1)
}

sample.timeslot=function(fixed=T){
train.all.days=seq(from = as.Date('2016-01-02',tz='GMT'),to=as.Date('2016-01-21',tz='GMT'),by = 'day')
train.all.weekdays=as.numeric(factor(weekdays(train.all.days),levels = weekdays(seq(from = as.Date('2016-01-01'),length.out = 7,by = 'day')),ordered=T))
fri=which(train.all.weekdays==1)
sat=which(train.all.weekdays==2)
sun=which(train.all.weekdays==3)
others=which(train.all.weekdays>3)
train.index=c(sample(fri,1),sample(sat,1),sample(sun,1),sample(others,2,replace = F))
train.days=train.all.days[-train.index]
test.days=train.all.days[train.index]

slices=c(46,  58 , 70 , 82 , 94 ,106, 118 ,130, 142)
if(!fixed){slices=round(slices+runif(9,-1,1))}
list(train.days,test.days,slices)
}
mapeObj1=function(preds,dtrain){
  gaps=getinfo(dtrain,'label')
  grad=sign(preds-gaps)/gaps
  hess=1/gaps
  grad[which(gaps==0)]=0
  hess[which(gaps==0)]=0
  return(list(grad = grad, hess = hess))
}

mapeObj2=function(preds,dtrain){
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
  err = abs(gaps-preds)/gaps
  err[which(gaps==0)]=0
  err=mean(err)
  return(list(metric = "error", value = err))
}

write.sub=function(id,day,timeslice,pred,filename='sub1.csv'){
    timeslot=paste0(day,'-',timeslice)
    sub=data.frame(id,timeslot,pred)
    write.table(sub,row.names = F,file = filename,sep = ',',col.names = F,quote = F)
}
