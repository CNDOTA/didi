library(data.table)
library(stringr)
prepare_train_data=function(dir){
  ####### cluster mapping data #######
  mapping=read.table(file =paste0(dir,"cluster_map//cluster_map"))
  colnames(mapping)=c('cluster','id')
  mapping=data.table(mapping)
  
  poi=readLines(file(paste0(dir,"poi_data//poi_data")))
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
  poi.dat=parse.poi(poi)
  ####### weather data #######
  weather.files=list.files(paste0(dir,"weather_data//"),full.names=T)
  weather.dat=data.frame()
  
  for(wf in weather.files){
    weather.dat=rbind(weather.dat,read.table(wf))
  }
  colnames(weather.dat)=c('day','timeinday','weather_condition','weather_temp','weather_pm25')
  weather.dat=data.table(weather.dat)
  weather.dat[,day:=as.Date(as.character(day))]
  weather.dat=cbind(weather.dat,datetime=weather.dat[,strptime(paste(as.character(day),timeinday),'%Y-%m-%d %H:%M:%S',tz = "GMT")])
  
  weather.dat[,timeslice:=ceiling(as.numeric(difftime(datetime,as.POSIXct(as.character(day),tz="GMT"),units = 'mins'))/10)]
  # keep o1
  weather.dat[,o:=rank(datetime),by=.(day,timeslice)]
  weather.dat=weather.dat[o==1,]
  weather.dat[,o:=NULL]
  
  ####### traffic data #######
  traffic.files=list.files(paste0(dir,"traffic_data//"),full.names=T)
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
  order.files=list.files(paste0(dir,"order_data//"),full.names=T)
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
  order.dat[,gap:=sum(is.na(driver_id)),by=.(start_district_id,day,timeslice)]
  
  gap=order.dat[,sum(is.na(driver_id)),by=.(start_district_id,day,timeslice)]
  colnames(gap)[4]='gap'
  gap[,weekday:=as.numeric(factor(weekdays(day),levels = weekdays(seq(from = as.Date('2016-01-01'),length.out = 7,by = 'day')),ordered=T))]
  setkeyv(gap,c('start_district_id','day','timeslice'))
  
  
  gap.temp=copy(gap)
  gap.temp[,timeslice1:=timeslice+1]
  gap.temp[,timeslice2:=timeslice+2]
  gap.temp[,timeslice3:=timeslice+3]
  gap.temp[,timeslice4:=timeslice+4]
  
  gap.temp[,timeslice:=NULL]
  gap.temp[,weekday:=NULL]
  colnames(gap.temp)[3]='gap_past'
  # join gap and gap 10, 20, 30 and 40 minutes before
  setkeyv(gap.temp,c('start_district_id','day','timeslice1'))
  gap=gap.temp[gap][,.(start_district_id,day, gap_past,gap, weekday,timeslice1)]
  colnames(gap)[c(3,6)]=c('gap_past_1','timeslice')
  
  setkeyv(gap,c('start_district_id','day','timeslice'))
  setkeyv(gap.temp,c('start_district_id','day','timeslice2'))
  gap=gap.temp[gap][,.(start_district_id,day, gap_past,gap, weekday,timeslice2,gap_past_1)]
  colnames(gap)[c(3,6)]=c('gap_past_2','timeslice')
  
  setkeyv(gap,c('start_district_id','day','timeslice'))
  setkeyv(gap.temp,c('start_district_id','day','timeslice3'))
  gap=gap.temp[gap][,.(start_district_id,day, gap_past,gap, weekday,timeslice3,gap_past_1,gap_past_2)]
  colnames(gap)[c(3,6)]=c('gap_past_3','timeslice')
  
  setkeyv(gap,c('start_district_id','day','timeslice'))
  setkeyv(gap.temp,c('start_district_id','day','timeslice4'))
  gap=gap.temp[gap][,.(start_district_id,day, gap_past,gap, weekday,timeslice4,gap_past_1,gap_past_2,gap_past_3)]
  colnames(gap)[c(3,6)]=c('gap_past_4','timeslice')
  
  gap=gap[,.( start_district_id,day,timeslice,weekday,gap,gap_past_1 ,gap_past_2,gap_past_3,gap_past_4)]
  
  
  traffic.temp=traffic.dat[,.(id,traffic_l1,traffic_l2,traffic_l3,traffic_l4,day,timeslice)]
  traffic.temp[,timeslice1:=timeslice+1]
  traffic.temp[,timeslice2:=timeslice+2]
  traffic.temp[,timeslice3:=timeslice+3]
  traffic.temp[,timeslice:=NULL]
  colnames(traffic.temp)[c(2:5)]=c('traffic_l1_past1','traffic_l1_past2','traffic_l1_past3','traffic_l1_past4')
  setkeyv(traffic.temp,c('id','day','timeslice1'))
  
  gap=traffic.temp[gap][,.(id, traffic_l1_past1, traffic_l1_past2 ,traffic_l1_past3,traffic_l1_past4,day,timeslice1,weekday, gap, gap_past_1,gap_past_2,gap_past_3)]
  colnames(gap)[c(7)]='timeslice'
  
  setkeyv(gap,c('id','day','timeslice'))
  colnames(traffic.temp)[c(2:5)]=c('traffic_l2_past1','traffic_l2_past2','traffic_l2_past3','traffic_l2_past4')
  setkeyv(traffic.temp,c('id','day','timeslice2'))
  gap=traffic.temp[gap]
  gap[,timeslice1:=NULL]
  gap[,timeslice3:=NULL]
  colnames(gap)[c(7)]='timeslice'
  
  setkeyv(gap,c('id','day','timeslice'))
  colnames(traffic.temp)[c(2:5)]=c('traffic_l3_past1','traffic_l3_past2','traffic_l3_past3','traffic_l3_past4')
  setkeyv(traffic.temp,c('id','day','timeslice3'))
  gap=traffic.temp[gap]
  gap[,timeslice1:=NULL]
  gap[,timeslice2:=NULL]
  colnames(gap)[c(7)]='timeslice'
  gap=gap[,.(id,day,timeslice,weekday,gap,gap_past_1,gap_past_2,gap_past_3,gap_past_4,traffic_l1_past1 ,traffic_l1_past2, traffic_l1_past3, traffic_l1_past4,traffic_l2_past1 ,traffic_l2_past2, traffic_l2_past3, traffic_l2_past4,traffic_l3_past1 ,traffic_l3_past2, traffic_l3_past3, traffic_l3_past4)]
  
  # 
  placed=order.dat[,sum(!is.na(driver_id)),by=.(start_district_id,day,timeslice)]
  total=order.dat[,sum(1+0*is.na(driver_id)),by=.(start_district_id,day,timeslice)]
  colnames(placed)[4]='placed'
  colnames(total)[4]='total'
  
  setkeyv(placed,c('start_district_id','day','timeslice'))

  placed.temp=copy(placed)
  placed.temp[,timeslice1:=timeslice+1]
  placed.temp[,timeslice2:=timeslice+2]
  placed.temp[,timeslice3:=timeslice+3]
  placed.temp[,timeslice4:=timeslice+4]
  
  placed.temp[,timeslice:=NULL]
  
  colnames(placed.temp)[3]='placed_past'
  # join placed and placed 10, 20, 30 minutes before
  setkeyv(placed.temp,c('start_district_id','day','timeslice1'))
  placed=placed.temp[placed][,.(start_district_id,day, placed_past,placed,timeslice1)]
  colnames(placed)[c(3,5)]=c('placed_past_1','timeslice')
  
  setkeyv(placed,c('start_district_id','day','timeslice'))
  setkeyv(placed.temp,c('start_district_id','day','timeslice2'))
  placed=placed.temp[placed][,.(start_district_id,day, placed_past,placed,timeslice2,placed_past_1)]
  colnames(placed)[c(3,5)]=c('placed_past_2','timeslice')
  
  setkeyv(placed,c('start_district_id','day','timeslice'))
  setkeyv(placed.temp,c('start_district_id','day','timeslice3'))
  placed=placed.temp[placed][,.(start_district_id,day, placed_past,placed, timeslice3,placed_past_1,placed_past_2)]
  colnames(placed)[c(3,5)]=c('placed_past_3','timeslice')
  placed=placed[,.( start_district_id,day,timeslice,placed,placed_past_1 ,placed_past_2,placed_past_3)]
  
  setkeyv(placed,c('start_district_id','day','timeslice'))
  setkeyv(placed.temp,c('start_district_id','day','timeslice4'))
  placed=placed.temp[placed][,.(start_district_id,day, placed_past,placed, timeslice4,placed_past_1,placed_past_2,placed_past_3)]
  colnames(placed)[c(3,5)]=c('placed_past_4','timeslice')
  placed=placed[,.( start_district_id,day,timeslice,placed,placed_past_1 ,placed_past_2,placed_past_3,placed_past_4)]
  
  setkeyv(total,c('start_district_id','day','timeslice'))
  
  total.temp=copy(total)
  total.temp[,timeslice1:=timeslice+1]
  total.temp[,timeslice2:=timeslice+2]
  total.temp[,timeslice3:=timeslice+3]
  total.temp[,timeslice4:=timeslice+4]
  
  total.temp[,timeslice:=NULL]
  
  colnames(total.temp)[3]='total_past'
  # join total and total 10, 20, 30 minutes before
  setkeyv(total.temp,c('start_district_id','day','timeslice1'))
  total=total.temp[total][,.(start_district_id,day, total_past,total,timeslice1)]
  colnames(total)[c(3,5)]=c('total_past_1','timeslice')
  
  setkeyv(total,c('start_district_id','day','timeslice'))
  setkeyv(total.temp,c('start_district_id','day','timeslice2'))
  total=total.temp[total][,.(start_district_id,day, total_past,total,timeslice2,total_past_1)]
  colnames(total)[c(3,5)]=c('total_past_2','timeslice')
  
  setkeyv(total,c('start_district_id','day','timeslice'))
  setkeyv(total.temp,c('start_district_id','day','timeslice3'))
  total=total.temp[total][,.(start_district_id,day, total_past,total, timeslice3,total_past_1,total_past_2)]
  colnames(total)[c(3,5)]=c('total_past_3','timeslice')
  total=total[,.( start_district_id,day,timeslice,total,total_past_1 ,total_past_2,total_past_3)]

  setkeyv(total,c('start_district_id','day','timeslice'))
  setkeyv(total.temp,c('start_district_id','day','timeslice4'))
  total=total.temp[total][,.(start_district_id,day, total_past,total, timeslice4,total_past_1,total_past_2,total_past_3)]
  colnames(total)[c(3,5)]=c('total_past_4','timeslice')
  total=total[,.( start_district_id,day,timeslice,total,total_past_1 ,total_past_2,total_past_3,total_past_4)]
  
  setkeyv(gap,c('id','day','timeslice'))
  setkeyv(placed,c('start_district_id','day','timeslice'))
  setkeyv(total,c('start_district_id','day','timeslice'))
  gap=gap[placed]
  rm(placed)
  gc()
  setkeyv(gap,c('id','day','timeslice'))
  gap=gap[total]
  rm(total)
  gc()
  
  # weather
  weather.temp=weather.dat[,.(weather_condition, weather_temp, weather_pm25,day,timeslice)]
  weather.temp[,timeslice1:=timeslice+1]
  weather.temp[,timeslice2:=timeslice+2]
  weather.temp[,timeslice3:=timeslice+3]
  weather.temp[,timeslice:=NULL]
  colnames(weather.temp)[c(1:3)]=c('weather_condition_past1','weather_temp_past1','weather_pm25_past1')
  setkeyv(weather.temp,c('day','timeslice1'))
  setkeyv(gap,c('day','timeslice'))
  gap=weather.temp[gap]
  gap[,timeslice2:=NULL]
  gap[,timeslice3:=NULL]
  colnames(gap)[c(5)]='timeslice'
  
  setkeyv(gap,c('day','timeslice'))
  colnames(weather.temp)[c(1:3)]=c('weather_condition_past2','weather_temp_past2','weather_pm25_past2')
  setkeyv(weather.temp,c('day','timeslice2'))
  gap=weather.temp[gap]
  gap[,timeslice1:=NULL]
  gap[,timeslice3:=NULL]
  colnames(gap)[c(5)]='timeslice'
  
  setkeyv(gap,c('day','timeslice'))
  colnames(weather.temp)[c(1:3)]=c('weather_condition_past3','weather_temp_past3','weather_pm25_past3')
  setkeyv(weather.temp,c('day','timeslice3'))
  gap=weather.temp[gap]
  gap[,timeslice1:=NULL]
  gap[,timeslice2:=NULL]
  colnames(gap)[c(5)]='timeslice'
    
  return(list('gap'=gap[,sort(colnames(gap)),with=F],'mapping'=mapping,'poi.dat'=poi.dat,'weather.dat'=weather.dat,'traffic.dat'=traffic.dat,'order.dat'=order.dat))
}

prepare_test_data=function(dir){
  to.test=read.table(file=paste0(dir,"read_me_1.txt"),skip = 1,as.is = T)
  to.test$date=as.Date(substring(to.test$V1,first = 1,last = 10))
  to.test$slice=as.integer(substring(to.test$V1,first = 12,last = nchar(to.test$V1)))
  to.test$V1=NULL
  to.test=data.table(to.test)
  setnames(to.test,c('day','timeslice'))
  ####### cluster mapping data #######
  mapping=read.table(file =paste0(dir,"cluster_map//cluster_map"))
  colnames(mapping)=c('cluster','id')
  mapping=data.table(mapping)
  
  poi=readLines(file(paste0(dir,"poi_data//poi_data")))
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
  poi.dat=parse.poi(poi)
  ####### weather data #######
  weather.files=list.files(paste0(dir,"weather_data//"),full.names=T)
  weather.dat=data.frame()
  
  for(wf in weather.files){
    weather.dat=rbind(weather.dat,read.table(wf))
  }
  colnames(weather.dat)=c('day','timeinday','weather_condition','weather_temp','weather_pm25')
  weather.dat=data.table(weather.dat)
  weather.dat[,day:=as.Date(as.character(day))]
  weather.dat=cbind(weather.dat,datetime=weather.dat[,strptime(paste(as.character(day),timeinday),'%Y-%m-%d %H:%M:%S',tz = "GMT")])
  
  weather.dat[,timeslice:=ceiling(as.numeric(difftime(datetime,as.POSIXct(as.character(day),tz="GMT"),units = 'mins'))/10)]
  # keep o1
  weather.dat[,o:=rank(datetime),by=.(day,timeslice)]
  weather.dat=weather.dat[o==1,]
  weather.dat[,o:=NULL]
  
  # add new slice
  weather.dat=rbind(weather.dat,to.test,fill=T)
  
  # expand to.test by id
  to.test.expand=data.frame()
  for(i in 1:nrow(mapping)){
    temp=copy(to.test)
    temp[,id:=i]
    to.test.expand=rbind(to.test.expand,temp)
  }
  
  ####### traffic data #######
  traffic.files=list.files(paste0(dir,"traffic_data//"),full.names=T)
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
  # add new slice
  traffic.dat=rbind(traffic.dat,to.test.expand,fill=T)
  ####### order data #######
  order.files=list.files(paste0(dir,"order_data//"),full.names=T)
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
  order.dat[,gap:=sum(is.na(driver_id)),by=.(start_district_id,day,timeslice)]
  
  gap=order.dat[,sum(is.na(driver_id)),by=.(start_district_id,day,timeslice)]
  colnames(gap)[4]='gap'
  gap[,weekday:=as.numeric(factor(weekdays(day),levels = weekdays(seq(from = as.Date('2016-01-01'),length.out = 7,by = 'day')),ordered=T))]
  
  setkeyv(gap,c('start_district_id','day','timeslice'))
  
  # add new slice
  colnames(to.test.expand)[3]='start_district_id'
  gap=rbind(gap,to.test.expand,fill=T)
  
  
  gap.temp=copy(gap)
  gap.temp[,timeslice1:=timeslice+1]
  gap.temp[,timeslice2:=timeslice+2]
  gap.temp[,timeslice3:=timeslice+3]
  gap.temp[,timeslice4:=timeslice+4]
  
  gap.temp[,timeslice:=NULL]
  gap.temp[,weekday:=NULL]
  colnames(gap.temp)[3]='gap_past'
  # join gap and gap 10, 20, 30 minutes before
  setkeyv(gap.temp,c('start_district_id','day','timeslice1'))
  gap=gap.temp[gap][,.(start_district_id,day, gap_past,gap, weekday,timeslice1)]
  colnames(gap)[c(3,6)]=c('gap_past_1','timeslice')
  
  setkeyv(gap,c('start_district_id','day','timeslice'))
  setkeyv(gap.temp,c('start_district_id','day','timeslice2'))
  gap=gap.temp[gap][,.(start_district_id,day, gap_past,gap, weekday,timeslice2,gap_past_1)]
  colnames(gap)[c(3,6)]=c('gap_past_2','timeslice')
  
  setkeyv(gap,c('start_district_id','day','timeslice'))
  setkeyv(gap.temp,c('start_district_id','day','timeslice3'))
  gap=gap.temp[gap][,.(start_district_id,day, gap_past,gap, weekday,timeslice3,gap_past_1,gap_past_2)]
  colnames(gap)[c(3,6)]=c('gap_past_3','timeslice')
  gap=gap[,.( start_district_id,day,timeslice,weekday,gap,gap_past_1 ,gap_past_2,gap_past_3)]
  
  setkeyv(gap,c('start_district_id','day','timeslice'))
  setkeyv(gap.temp,c('start_district_id','day','timeslice4'))
  gap=gap.temp[gap][,.(start_district_id,day, gap_past,gap, weekday,timeslice4,gap_past_1,gap_past_2,gap_past_3)]
  colnames(gap)[c(3,6)]=c('gap_past_4','timeslice')
  
  traffic.temp=traffic.dat[,.(id,traffic_l1,traffic_l2,traffic_l3,traffic_l4,day,timeslice)]
  traffic.temp[,timeslice1:=timeslice+1]
  traffic.temp[,timeslice2:=timeslice+2]
  traffic.temp[,timeslice3:=timeslice+3]
  traffic.temp[,timeslice:=NULL]
  colnames(traffic.temp)[c(2:5)]=c('traffic_l1_past1','traffic_l1_past2','traffic_l1_past3','traffic_l1_past4')
  setkeyv(traffic.temp,c('id','day','timeslice1'))
  
  gap=traffic.temp[gap][,.(id, traffic_l1_past1, traffic_l1_past2 ,traffic_l1_past3,traffic_l1_past4,day,timeslice1,weekday, gap, gap_past_1,gap_past_2,gap_past_3)]
  colnames(gap)[c(7)]='timeslice'
  
  setkeyv(gap,c('id','day','timeslice'))
  colnames(traffic.temp)[c(2:5)]=c('traffic_l2_past1','traffic_l2_past2','traffic_l2_past3','traffic_l2_past4')
  setkeyv(traffic.temp,c('id','day','timeslice2'))
  gap=traffic.temp[gap]
  gap[,timeslice1:=NULL]
  gap[,timeslice3:=NULL]
  colnames(gap)[c(7)]='timeslice'
  
  setkeyv(gap,c('id','day','timeslice'))
  colnames(traffic.temp)[c(2:5)]=c('traffic_l3_past1','traffic_l3_past2','traffic_l3_past3','traffic_l3_past4')
  setkeyv(traffic.temp,c('id','day','timeslice3'))
  gap=traffic.temp[gap]
  gap[,timeslice1:=NULL]
  gap[,timeslice2:=NULL]
  colnames(gap)[c(7)]='timeslice'
  gap=gap[,.(id,day,timeslice,weekday,gap,gap_past_1,gap_past_2,gap_past_3,traffic_l1_past1 ,traffic_l1_past2, traffic_l1_past3, traffic_l1_past4,traffic_l2_past1 ,traffic_l2_past2, traffic_l2_past3, traffic_l2_past4,traffic_l3_past1 ,traffic_l3_past2, traffic_l3_past3, traffic_l3_past4)]
  
  # 
  placed=order.dat[,sum(!is.na(driver_id)),by=.(start_district_id,day,timeslice)]
  total=order.dat[,sum(1+0*is.na(driver_id)),by=.(start_district_id,day,timeslice)]
  colnames(placed)[4]='placed'
  colnames(total)[4]='total'
  # 
  placed=rbind(placed,to.test.expand,fill=T)
  total=rbind(total,to.test.expand,fill=T)
  
  setkeyv(placed,c('start_district_id','day','timeslice'))
  
  placed.temp=copy(placed)
  placed.temp[,timeslice1:=timeslice+1]
  placed.temp[,timeslice2:=timeslice+2]
  placed.temp[,timeslice3:=timeslice+3]
  placed.temp[,timeslice4:=timeslice+4]
  
  placed.temp[,timeslice:=NULL]
  
  colnames(placed.temp)[3]='placed_past'
  # join placed and placed 10, 20, 30 minutes before
  setkeyv(placed.temp,c('start_district_id','day','timeslice1'))
  placed=placed.temp[placed][,.(start_district_id,day, placed_past,placed,timeslice1)]
  colnames(placed)[c(3,5)]=c('placed_past_1','timeslice')
  
  setkeyv(placed,c('start_district_id','day','timeslice'))
  setkeyv(placed.temp,c('start_district_id','day','timeslice2'))
  placed=placed.temp[placed][,.(start_district_id,day, placed_past,placed,timeslice2,placed_past_1)]
  colnames(placed)[c(3,5)]=c('placed_past_2','timeslice')
  
  setkeyv(placed,c('start_district_id','day','timeslice'))
  setkeyv(placed.temp,c('start_district_id','day','timeslice3'))
  placed=placed.temp[placed][,.(start_district_id,day, placed_past,placed, timeslice3,placed_past_1,placed_past_2)]
  colnames(placed)[c(3,5)]=c('placed_past_3','timeslice')
  placed=placed[,.( start_district_id,day,timeslice,placed,placed_past_1 ,placed_past_2,placed_past_3)]
  
  setkeyv(placed,c('start_district_id','day','timeslice'))
  setkeyv(placed.temp,c('start_district_id','day','timeslice4'))
  placed=placed.temp[placed][,.(start_district_id,day, placed_past,placed, timeslice4,placed_past_1,placed_past_2,placed_past_3)]
  colnames(placed)[c(3,5)]=c('placed_past_4','timeslice')
  placed=placed[,.( start_district_id,day,timeslice,placed,placed_past_1 ,placed_past_2,placed_past_3,placed_past_4)]
  
  setkeyv(total,c('start_district_id','day','timeslice'))
  
  total.temp=copy(total)
  total.temp[,timeslice1:=timeslice+1]
  total.temp[,timeslice2:=timeslice+2]
  total.temp[,timeslice3:=timeslice+3]
  total.temp[,timeslice4:=timeslice+4]
  
  total.temp[,timeslice:=NULL]
  
  colnames(total.temp)[3]='total_past'
  # join total and total 10, 20, 30 minutes before
  setkeyv(total.temp,c('start_district_id','day','timeslice1'))
  total=total.temp[total][,.(start_district_id,day, total_past,total,timeslice1)]
  colnames(total)[c(3,5)]=c('total_past_1','timeslice')
  
  setkeyv(total,c('start_district_id','day','timeslice'))
  setkeyv(total.temp,c('start_district_id','day','timeslice2'))
  total=total.temp[total][,.(start_district_id,day, total_past,total,timeslice2,total_past_1)]
  colnames(total)[c(3,5)]=c('total_past_2','timeslice')
  
  setkeyv(total,c('start_district_id','day','timeslice'))
  setkeyv(total.temp,c('start_district_id','day','timeslice3'))
  total=total.temp[total][,.(start_district_id,day, total_past,total, timeslice3,total_past_1,total_past_2)]
  colnames(total)[c(3,5)]=c('total_past_3','timeslice')
  total=total[,.( start_district_id,day,timeslice,total,total_past_1 ,total_past_2,total_past_3)]
  
  setkeyv(total,c('start_district_id','day','timeslice'))
  setkeyv(total.temp,c('start_district_id','day','timeslice4'))
  total=total.temp[total][,.(start_district_id,day, total_past,total, timeslice4,total_past_1,total_past_2,total_past_3)]
  colnames(total)[c(3,5)]=c('total_past_4','timeslice')
  total=total[,.( start_district_id,day,timeslice,total,total_past_1 ,total_past_2,total_past_3,total_past_4)]
  
  
  setkeyv(gap,c('id','day','timeslice'))
  setkeyv(placed,c('start_district_id','day','timeslice'))
  setkeyv(total,c('start_district_id','day','timeslice'))
  gap=gap[placed]
  rm(placed)
  gc()
  setkeyv(gap,c('id','day','timeslice'))
  gap=gap[total]
  rm(total)
  gc()
  
  # weather
  weather.temp=weather.dat[,.(weather_condition, weather_temp, weather_pm25,day,timeslice)]
  weather.temp[,timeslice1:=timeslice+1]
  weather.temp[,timeslice2:=timeslice+2]
  weather.temp[,timeslice3:=timeslice+3]
  weather.temp[,timeslice:=NULL]
  colnames(weather.temp)[c(1:3)]=c('weather_condition_past1','weather_temp_past1','weather_pm25_past1')
  setkeyv(weather.temp,c('day','timeslice1'))
  setkeyv(gap,c('day','timeslice'))
  gap=weather.temp[gap]
  gap[,timeslice2:=NULL]
  gap[,timeslice3:=NULL]
  colnames(gap)[c(5)]='timeslice'
  
  setkeyv(gap,c('day','timeslice'))
  colnames(weather.temp)[c(1:3)]=c('weather_condition_past2','weather_temp_past2','weather_pm25_past2')
  setkeyv(weather.temp,c('day','timeslice2'))
  gap=weather.temp[gap]
  gap[,timeslice1:=NULL]
  gap[,timeslice3:=NULL]
  colnames(gap)[c(5)]='timeslice'
  
  setkeyv(gap,c('day','timeslice'))
  colnames(weather.temp)[c(1:3)]=c('weather_condition_past3','weather_temp_past3','weather_pm25_past3')
  setkeyv(weather.temp,c('day','timeslice3'))
  gap=weather.temp[gap]
  gap[,timeslice1:=NULL]
  gap[,timeslice2:=NULL]
  colnames(gap)[c(5)]='timeslice'
  setkeyv(to.test,c('day','timeslice'))
  setkeyv(gap,c('day','timeslice'))
  gap=gap[to.test]
  gap[,placed:=NULL]
  gap[,total:=NULL]
  gap[,gap:=NULL]
  gap[,weekday:=as.numeric(factor(weekdays(day),levels = weekdays(seq(from = as.Date('2016-01-01'),length.out = 7,by = 'day')),ordered=T))]
  return(list('gap'=gap[,sort(colnames(gap)),with=F],'mapping'=mapping,'poi.dat'=poi.dat,'weather.dat'=weather.dat,'traffic.dat'=traffic.dat,'order.dat'=order.dat))
}
