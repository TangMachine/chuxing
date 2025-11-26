library(foreign)
library(haven)

setwd("c:/Users/admin/OneDrive/Machine/yan/出行行为分析/作业3/Data_Codes")

#################### Load Original Trip Data #############

trips = as.data.frame(read.spss("trips_anting.sav"))

hh_id = trips$HomeID
per_id = trips$hp_id
trip_id = trips$trip_id

BegTime = trips$begmin
EndTime = trips$endmin

O_pur = trips$O_pur 
D_pur = trips$D_pur
Mode = trips$mode

#time = EndTime - BegTime
#which(time < 0)

################### Travel Generation #####################

travel = data.frame(hh_id,per_id,
                    at_id =  2*trip_id, 
                    act_id = -2,
                    trip_id,
                    BegTime,
                    EndTime,
                    ActType = -2, 
                    O_pur,
                    D_pur,
                    Mode)

########## Generation of Activities except the last one #####################

acts = data.frame(hh_id,per_id,
                        at_id =  2*trip_id-1, 
                        act_id = trip_id,
                        trip_id = -2,
                        BegTime = c(0, EndTime[1:length(BegTime)-1]),
                        EndTime = BegTime,
                        ActType = O_pur, 
                        O_pur = -2,
                        D_pur = -2,
                        Mode = -2 )

acts$BegTime[acts$act_id == 1] = 0

############## Last Activity Generation ##############

id = 100*hh_id + per_id
temp = data.frame(id, trip_id)
temp1 = aggregate(temp, by = list(id), FUN = "max")
temp2 = temp1[,2:3]
colnames(temp2) = c("id", "trip_id_max")
temp3 = merge(temp, temp2, by = "id" , all.x = T)
act_last_id = which(temp3$trip_id == temp3$trip_id_max) 

act_last = acts[act_last_id,]
travel_last = travel[act_last_id,]

act_last$BegTime = travel_last$EndTime 
act_last$EndTime = 1440

act_last$ActType = travel_last$D_pur 

act_last$act_id = act_last$act_id + 1 

act_last$at_id = 2*travel_last$trip_id + 1  

############ Combine, Sort and Save  ###############

at = rbind(acts, act_last, travel)

at = at[order(at$hh_id,at$per_id,at$at_id),]

AT_Dur = at$EndTime - at$BegTime

at = data.frame(at, AT_Dur)

write_sav(at, "at.sav")


