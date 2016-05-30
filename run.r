# run this script from will prepare data and train model and then generate a submission file.
source('prepare_data.r')
source('fun.r')
train_dir="/Downloads/season_1/training_data/"
test_dir="/Downloads/season_1/test_set_1/"
train.dat=prepare_train_data(train_dir)
test.dat=prepare_test_data(test_dir)
save(train.dat,test.dat,file='didi.dat')# for future use
