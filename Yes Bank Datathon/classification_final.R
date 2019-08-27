#<---------Data Loading----------------->
#Master_Test & Train
train_master <- read.csv("Yes_Bank_Train.csv")
test_master <- read.csv("Yes_Bank_Test_int.csv")

#Test  & Train from master
cutoff = round(0.7*nrow(train_master)) #70:30 split for train and test respectively
train <- train_master[1:cutoff,]
test <- train_master[-(1:cutoff),]

sub <- read.csv("submission_19.csv")
one <- subset(sub,credit_amount > 4000 & credit_amount < 20000)
two <- subset(sub,credit_amount < 4000 & credit_amount > 1500)
three <- subset(sub,credit_amount < 1500)
one$cluster_number=1
two$cluster_number=2
three$cluster_number=3
ans2 <- rbind(one,two,three)
write.csv(ans2,"submission_c13.csv")