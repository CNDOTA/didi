# a simple guess gives a leaderboard score 0.29+

source('prepare_data.r')
source('fun.r')

test_dir="/Downloads/season_1/test_set_1/"
test.dat=prepare_test_data(test_dir)

test.gap=test.dat$gap

guess.p=function(test){
  p1=test$gap_past_1
  p2=test$gap_past_2
  p3=test$gap_past_3
  p1[is.na(p1)]=0
  p2[is.na(p2)]=0
  p3[is.na(p3)]=0
  p=pmax(1,(p1*0.65+p2*0.25+p3*0.15)/2)
  return(p)
}

pred=guess.p(test.gap)
write.sub(id=test.dat$gap$id,day = test.dat$gap$day,timeslice = test.dat$gap$timeslice,pred,'myguess.csv')
