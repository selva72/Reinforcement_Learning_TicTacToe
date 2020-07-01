
getwd()
rm(list=ls())
setwd("F:/R")


library("ReinforcementLearning")
library("dplyr")
library("tidyverse")
n=100 # df row count, length.
learningCount=0
data_unseen=0
model=0
modelY=0
accuracy=0
roundcount=0
endRound=TRUE
RandomSet=sample(1:9, 9)     

Player_1_Wins=0
Player_2_Wins=0
GamesPlayed=1
Player1winrate=0
punish=-10
reward=3
draw=0
starttime=""
endtime=""
randomNumber=5


t1=data.frame(State1 = ".",
              State2 = ".",
              State3 = ".",
              State4 = ".",
              State5 = ".",
              State6 = ".",
              State7 = ".",
              State8 = ".",
              State9 = ".",
              StateX=".........",
              ActionX="1",
              NextStateX=".........",
              RewardX=0,
              StateY=".........",
              ActionY="1",
              NextStateY=".........",
              RewardY=0,    
              row=1:n,
              stringsAsFactors = FALSE,
              roundcount=0,
              status=""
)


############################################player x move

starttime=Sys.time()
for (i in (1:9)) {
  
  t1[i,10:12]=as.numeric(i)
  t1[i,14:16]=as.numeric(i) 
}
control <- list(alpha = 0.2, gamma = 0.4, epsilon = 0.1)
modelX<- ReinforcementLearning(t1[1:9,], s = "StateX", a = "ActionX", r = "RewardX",  s_new = "NextStateX", iter = 5, control = control)

for (i in 1:n) {
  t1[i,10]=paste(t1[i,1],t1[i,2],t1[i,3],
                 t1[i,4],t1[i,5],t1[i,6],
                 t1[i,7],t1[i,8],t1[i,9],sep="")
  
  if(i>=10){                                                                                 
    
    if(endRound==TRUE){
      control <- list(alpha = 0.2, gamma = 0.4, epsilon = 0.1)
      model<- ReinforcementLearning(t1[1:i-1,], s = "StateX", a = "ActionX", r = "RewardX", s_new = "NextStateX",iter = 5, control = control)                              
    }
    
    
    TestAvailablity=TRUE                                                                    
    while (TestAvailablity) {                                                               
      data_unseen <- data.frame(State = t1[i,10], stringsAsFactors = FALSE)                 
      options(show.error.messages = FALSE)                                                  
      mtry=try(predict(model, data_unseen$State))                                           
      
      if(inherits(mtry, "try-error")){                                                      
        count1=0
        for (count1 in 1:9) {                                                               
          ifelse(t1[i,randomNumber]!=".",(randomNumber=RandomSet[count1]),break)}
      }else{randomNumber=mtry}                                                            
      
      randomNumber=as.numeric(randomNumber)                                               
      
      if(t1[i,randomNumber]!="."){                                                        
        model$Q_hash[[t1[i,10]]][[paste(randomNumber)]]=-999
        model$Q[t1[i,10],paste(randomNumber)]=-999                                        
      }else{TestAvailablity=FALSE}                                                        
    }
    
  }else{                                                                                  
    count1=0
    for (count1 in 1:9) {                                                                 
      ifelse(t1[i,randomNumber]!=".",(randomNumber=RandomSet[count1]),break)
    }
  }
  
  t1[i,as.numeric(randomNumber)]="X"                                      
  t1[i,11]= as.character(randomNumber)                                    
  ##################################fill nextstate for player Y
  t1[i,12]=paste(t1[i,1],t1[i,2],t1[i,3],                                 
                 t1[i,4],t1[i,5],t1[i,6],
                 t1[i,7],t1[i,8],t1[i,9],sep="")
  
  
  if(!endRound){t1[i-1,16]=t1[i,12]}                                      
  endRound=FALSE                                                          
  
  
  if(t1[i,1]=="X" && t1[i,2]=="X" && t1[i,3]=="X" ||                      
     t1[i,4]=="X" && t1[i,5]=="X" && t1[i,6]=="X" ||
     t1[i,7]=="X" && t1[i,8]=="X" && t1[i,9]=="X" ||
     t1[i,1]=="X" && t1[i,4]=="X" && t1[i,7]=="X" ||
     t1[i,2]=="X" && t1[i,5]=="X" && t1[i,8]=="X" ||
     t1[i,3]=="X" && t1[i,6]=="X" && t1[i,9]=="X" ||
     t1[i,1]=="X" && t1[i,5]=="X" && t1[i,9]=="X" ||
     t1[i,3]=="X" && t1[i,5]=="X" && t1[i,7]=="X"){ 
     t1[i,13]=reward                                                    
     t1[i-1,17]=punish                                               
     Player_1_Wins=Player_1_Wins+1
     for (penalty in ((1:roundcount))) {
       t1[(i-penalty),17]=punish
     }
     roundcount=0                                                  
     Player1winrate=round((Player_1_Wins/GamesPlayed)*100,digit=2)
     t1[i,20]="Win"
     print(paste0("Game -> ",GamesPlayed, " Win @ step # ",i,", winning % -> ",Player1winrate," %"))
     GamesPlayed=GamesPlayed+1
     endRound=TRUE
     next(i)
     
     
  }

  
  
  if(t1[i,1]!="." &&                                               
     t1[i,2]!="." && 
     t1[i,3]!="." && 
     t1[i,4]!="." && 
     t1[i,5]!="." && 
     t1[i,6]!="." && 
     t1[i,7]!="." && 
     t1[i,8]!="." && 
     t1[i,9]!="."){ 
    
    endRound=TRUE
    t1[i,13]=punish
    Player1winrate=round((Player_1_Wins/GamesPlayed)*100,digit=2)
    t1[i,20]="Draw"
    draw=draw+1
    print(paste0("Game -> ",GamesPlayed, " Draw @ step # ",i,", winning % -> ",Player1winrate," %"))
    GamesPlayed=GamesPlayed+1
    for (penalty in (1:(roundcount-1))) {t1[(i-penalty),13]=(punish+2)}       
    for (penalty in (1:roundcount)) {t1[(i-penalty),17]=punish}     
    
    roundcount=0
    
    next(i)
  } 
  
  
  
  ########################################player Y move
  ##########learning 
  ######################################################## 
   randomNumber=sample(1:9, 1)
  
   count1=0
   for (count1 in 1:9) {
     ifelse(t1[i,randomNumber]!=".",(randomNumber=RandomSet[count1]),break)
   }
  ########################################################
  
  
   
   t1[i,14]=paste(t1[i,1],t1[i,2],t1[i,3],
                  t1[i,4],t1[i,5],t1[i,6],
                  t1[i,7],t1[i,8],t1[i,9],sep="")
   t1[i,randomNumber]="O"
   t1[i,15]=as.character(randomNumber)
   
   t1[i,12]=paste(t1[i,1],t1[i,2],t1[i,3],
                  t1[i,4],t1[i,5],t1[i,6],
                  t1[i,7],t1[i,8],t1[i,9],sep="")
   t1[i,16]=t1[i,12]
   
   
   if(t1[i,1]=="O" && t1[i,2]=="O" && t1[i,3]=="O" ||
      t1[i,4]=="O" && t1[i,5]=="O" && t1[i,6]=="O" ||
      t1[i,7]=="O" && t1[i,8]=="O" && t1[i,9]=="O" ||
      t1[i,1]=="O" && t1[i,4]=="O" && t1[i,7]=="O" ||
      t1[i,2]=="O" && t1[i,5]=="O" && t1[i,8]=="O" ||
      t1[i,3]=="O" && t1[i,6]=="O" && t1[i,9]=="O" ||
      t1[i,1]=="O" && t1[i,5]=="O" && t1[i,9]=="O" ||
      t1[i,3]=="O" && t1[i,5]=="O" && t1[i,7]=="O"){ 
     t1[i,13]=punish
     t1[i,17]=reward
     Player_2_Wins=Player_2_Wins+1
     for (penalty in ((roundcount:1))) {
       t1[(i-penalty),13]=punish
       
     }
     
     Player1winrate=round((Player_1_Wins/GamesPlayed)*100,digit=2)
     roundcount=0
     t1[i,20]="Loss"
     print(paste0("Game -> ",GamesPlayed, " Loss @ step # ",i,", winning % -> ",Player1winrate," %"))
     GamesPlayed=GamesPlayed+1
     endRound=TRUE
     next(i)                                                           
   }
   
   
 
  endtime=Sys.time()
  t1[i,19]=roundcount

  
  roundcount=roundcount+1                                             
  t1[i+1,1:9]=t1[i,1:9]                                               
  
}
GamesPlayed=GamesPlayed-1

timetaken=endtime-starttime
timetaken

print(paste0("Player 1 wins ---> ", Player_1_Wins))
print(paste0("Player 2 wins ---> ", Player_2_Wins))
print(paste0("Draw ------------> ", draw))
print(paste0("Total games -----> ", GamesPlayed))
t1
summary(t1)
summary(t1$RewardX)
summary(model)
filename=paste0("data_3.1v_",n,"rows.Rda")
filename
save(t1,file=filename)




data_unseen <- data.frame(State = c("........."), 
                          stringsAsFactors = FALSE)
data_unseen$OptimalAction <- predict(model, data_unseen$State)
data_unseen

summary(model)
print(model)
t_qhash=model$Q_hash




# play demo
control <- list(alpha = 0.2, gamma = 0.4, epsilon = 0.1)                                                                                      # parameters
model<- ReinforcementLearning(t1[1:10000,], s = "StateX", a = "ActionX", r = "RewardX",  s_new = "NextStateX", iter = 5, control = control)
model$Q_hash$X...XO..O

df=t1
df=unique(df$NextStateX)
length(df)
view(df)
