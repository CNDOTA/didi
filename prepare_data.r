library(data.table)
library(stringr)
dir="your_dir"

####### cluster mapping data #######
mapping=read.table(file =paste0(dir,"cluster_map\\cluster_map"))
colnames(mapping)=c('cluster','id')
mapping=data.table(mapping)

poi=readLines(file(paste0(dir,"poi_data\\poi_data")))
colnames(mapping)=c('cluster','id')
mapping=data.table(mapping)

####### poi data #######
parse.poi=function(poi){
  l1=c()
  l2=c()
  l3=c()
  for (poi.line in poi){
    line=strsplit(poi.line,'\t')[[1]]
    
    for (i in 2:length(line))
    {
      e=strsplit(line[i],':')[[1]]
      if(str_count(e[1],"#")==0){
        # level 1, append 0 for level 2
        l1=union(l1,e[1])
        l2=union(l2,paste0(strsplit(e[1],'#')[[1]],"#0"))
      }else if (str_count(e[1],"#")==1){
        # level 2
        l1=union(l1,strsplit(e[1],'#')[[1]][1])
        l2=union(l2,e[1])
      }else if (str_count(e[1],"#")==2){
        l1=union(l1,strsplit(e[1],'#')[[1]])
        l2=union(l2,paste0(strsplit(e[1],'#')[[1]][1],"#",strsplit(e[1],'#')[[1]][2]))
        l3=union(l3,e[1])
      }
    }
  }
  l1=sort(l1)
  l2=sort(l2)
  l3=sort(l3)
  #return(list("l1"=l1,"l2"=l2,"l3"=l3))
  m=length(l1)+length(l2)+length(l3)
  poi.matrix=array(0,c(length(poi),m))
  poi.names=c(l1,l2,l3)
  colnames(poi.matrix)=paste0("poi_",poi.names)
  poi.matrix
  for(i in 1:length(poi)){
    
    line=strsplit(poi[i],'\t')[[1]]
    rowId=mapping[cluster==line[1],id]
    for (e in line[2:length(line)]){
      e.split=strsplit(e,':')[[1]]
      e.name=e.split[1]
      if(str_count(e.name,"#")==0){
        e.name=paste0("poi_",e.name,"#0")
      }else{
        e.name=paste0("poi_",e.name)
      }
      poi.matrix[rowId,e.name]=as.numeric(e.split[2])
    }
    for (e in paste0("poi_",c(l2,l3))){
      l1.name=strsplit(e,'#')[[1]][1]
      poi.matrix[rowId,l1.name]=poi.matrix[rowId,l1.name]+poi.matrix[rowId,e]
    }
  }
  
  poi.matrix
}

####### weather data #######
weather.files=list.files(paste0(dir,"weather_data\\"),full.names=T)
weather.dat=data.frame()

for(wf in weather.files){
  weather.dat=rbind(weather.dat,read.table(wf))
}
colnames(weather.dat)=c('day','timeinday','weather_condition','weather_temp','weather_pm25')
weather.dat=data.table(weather.dat)
weather.dat[,day:=as.Date(as.character(day))]
weather.dat=cbind(weather.dat,datetime=weather.dat[,strptime(paste(as.character(day),timeinday),'%Y-%m-%d %H:%M:%S',tz = "GMT")])

weather.dat[,timeslice:=ceiling(as.numeric(difftime(datetime,as.POSIXct(as.character(day),tz="GMT"),units = 'mins'))/10)]

####### traffic data #######
traffic.files=list.files(paste0(dir,"traffic_data\\"),full.names=T)
traffic.dat=data.frame()

for(tf in traffic.files){
  traffic.dat=rbind(traffic.dat,read.table(tf))
}
traffic.dat=data.table(traffic.dat)
setkey(traffic.dat,'V1')
setkey(mapping,'cluster')
traffic.dat=traffic.dat[mapping]
traffic.dat[,V1:=NULL]
helper1=function(x){
  res=double(length(x))
  for(i in 1:length(x)) res[i]=strsplit(as.character(x[i]),':')[[1]][2]
  res
}
traffic.dat[,traffic_l1:=helper1(V2)]
traffic.dat[,traffic_l2:=helper1(V3)]
traffic.dat[,traffic_l3:=helper1(V4)]
traffic.dat[,traffic_l4:=helper1(V5)]
traffic.dat[,day:=as.Date(as.character(V6))]
traffic.dat=cbind(traffic.dat,datetime=traffic.dat[,strptime(paste(as.character(day),V7),'%Y-%m-%d %H:%M:%S',tz = "GMT")])
traffic.dat[,timeslice:=ceiling(as.numeric(difftime(datetime,as.POSIXct(as.character(day),tz="GMT"),units = 'mins'))/10)]
traffic.dat[,V2:=NULL]
traffic.dat[,V3:=NULL]
traffic.dat[,V4:=NULL]
traffic.dat[,V5:=NULL]
traffic.dat[,V6:=NULL]
traffic.dat[,V7:=NULL]

####### order data #######
order.files=list.files(paste0(dir,"order_data\\"),full.names=T)
order.dat=data.frame()

for(of in order.files){
  print(of)
  order.dat=rbind(order.dat,fread(of,na.strings="NULL"))
  gc()
}

setnames(order.dat,c('order_id','driver_id','passenger_id','start_district_hash','dest_district_hash','price','datetime'))
setkey(order.dat,'start_district_hash')
order.dat=order.dat[mapping]
colnames(order.dat)[8]='start_district_id'
setkey(order.dat,'dest_district_hash')
order.dat=order.dat[mapping]
colnames(order.dat)[9]='dest_district_id'
order.dat[,start_district_hash:=NULL]
order.dat[,dest_district_hash:=NULL]
order.dat[,datetime:=as.POSIXct(datetime,tz='GMT')]
order.dat[,day:=as.Date(datetime,tz='GMT')]
order.dat[,timeslice:=ceiling(as.numeric(difftime(datetime,day,units = 'mins'))/10)]
