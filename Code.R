#######################################################################
###  Below is the code that can be used to analyze PL effect using  ###
###  Kim and Rhee (1997) method. Differences in Differences method  ###
###  is done simply using Stata after exporting the cleaned data.   ###
#######################################################################

###############################
### Load required libraries ###
###############################

library(dplyr)
library(ggplot2)
library(readxl)
library(pastecs)
library(psych)
library(stats)


####################################
### Import and clean up the data ###
####################################

tadawul_raw <- read_excel("~/R/ecmt678/tadawul_data.xlsx", sheet = 1, col_names = TRUE, col_types = NULL, na = "", skip = 0)

tadawul_raw1 <- mutate(tadawul_raw[1:nrow(tadawul_raw)-1,], 
                       hit_up = floor(tadawul_raw$Close[2:length(tadawul_raw$Close)]*1.1/tadawul_raw$Tic[2:length(tadawul_raw$Tic)])*tadawul_raw$Tic[2:length(tadawul_raw$Tic)]-0.01, #margin of error
                       hit_down = ceiling(tadawul_raw$Close[2:length(tadawul_raw$Close)]*0.9/tadawul_raw$Tic[2:length(tadawul_raw$Tic)])*tadawul_raw$Tic[2:length(tadawul_raw$Tic)]+0.01, 
                       hit_90_up = floor(tadawul_raw$Close[2:length(tadawul_raw$Close)]*1.09/tadawul_raw$Tic[2:length(tadawul_raw$Tic)])*tadawul_raw$Tic[2:length(tadawul_raw$Tic)]-0.01, 
                       hit_90_down = ceiling(tadawul_raw$Close[2:length(tadawul_raw$Close)]*0.91/tadawul_raw$Tic[2:length(tadawul_raw$Tic)])*tadawul_raw$Tic[2:length(tadawul_raw$Tic)]+0.01,
                       hit_80_up = floor(tadawul_raw$Close[2:length(tadawul_raw$Close)]*1.08/tadawul_raw$Tic[2:length(tadawul_raw$Tic)])*tadawul_raw$Tic[2:length(tadawul_raw$Tic)]-0.01,
                       hit_80_down = ceiling(tadawul_raw$Close[2:length(tadawul_raw$Close)]*0.92/tadawul_raw$Tic[2:length(tadawul_raw$Tic)])*tadawul_raw$Tic[2:length(tadawul_raw$Tic)]+0.01,
                       hit_70_up = floor(tadawul_raw$Close[2:length(tadawul_raw$Close)]*1.07/tadawul_raw$Tic[2:length(tadawul_raw$Tic)])*tadawul_raw$Tic[2:length(tadawul_raw$Tic)]-0.01,
                       hit_70_down = ceiling(tadawul_raw$Close[2:length(tadawul_raw$Close)]*0.93/tadawul_raw$Tic[2:length(tadawul_raw$Tic)])*tadawul_raw$Tic[2:length(tadawul_raw$Tic)]+0.01)
tadawul_raw11 <- mutate(tadawul_raw1,
                       hitdown = ifelse((Close <= hit_down & Per_Chn < -5), 1, 0),
                       low_hitdown = ifelse((Low <= hit_down), 1, 0),
                       open_hitdown = ifelse((Open <= hit_down), 1, 0),
                       High_hitdown = ifelse((High <= hit_down & Per_Chn < -5), 1, 0),
                       hitup = ifelse((Close >= hit_up & Per_Chn > 5), 1, 0),
                       high_hitup = ifelse((High >= hit_up), 1, 0),
                       open_hitup = ifelse((Open >= hit_up), 1, 0),
                       low_hitup = ifelse((Low >= hit_up & Per_Chn > 5), 1, 0),
                       hit90down = ifelse((Close <= hit_90_down & Per_Chn < -5 & Close > hit_down), 1, 0),
                       hit90up = ifelse((Close >= hit_90_up & Per_Chn > 5 & Close < hit_up), 1, 0),
                       hit80down = ifelse((Close <= hit_80_down & Per_Chn < -5 & Close > hit_90_down), 1, 0),
                       hit80up = ifelse((Close >= hit_80_up & Per_Chn > 5 & Close < hit_90_up), 1, 0),
                       hit70down = ifelse((Close <= hit_70_down & Per_Chn < -5 & Close > hit_80_down), 1, 0),
                       hit70up = ifelse((Close >= hit_70_up & Per_Chn > 5 & Close < hit_80_up), 1, 0))

tadawul_raw111 <- mutate(tadawul_raw11, hitdown_before = ifelse(c(tadawul_raw11$hitdown[2:length(tadawul_raw11$hitdown)],1) == 1, 1, 0))

tadawul_raw111 <- mutate(tadawul_raw11,
                        hitdown_after = ifelse(c(tadawul_raw11$hitdown[2:length(tadawul_raw11$hitdown)],0) == 1, 1, 0),
                        low_hitdown_after = ifelse(c(tadawul_raw11$low_hitdown[2:length(tadawul_raw11$hitdown)],0) == 1, 1, 0),
                        open_hitdown_after = ifelse(c(tadawul_raw11$open_hitdown[2:length(tadawul_raw11$hitdown)],0) == 1, 1, 0),
                        High_hitdown_after = ifelse(c(tadawul_raw11$High_hitdown[2:length(tadawul_raw11$hitdown)],0) == 1, 1, 0),
                        hitup_after = ifelse(c(tadawul_raw11$hitup[2:length(tadawul_raw11$hitdown)],0) == 1, 1, 0),
                        high_hitup_after = ifelse(c(tadawul_raw11$high_hitup[2:length(tadawul_raw11$hitdown)],0) == 1, 1, 0),
                        open_hitup_after = ifelse(c(tadawul_raw11$open_hitup[2:length(tadawul_raw11$hitdown)],0) == 1, 1, 0),
                        low_hitup_after = ifelse(c(tadawul_raw11$low_hitup[2:length(tadawul_raw11$hitdown)],0) == 1, 1, 0),
                        hit90down_after = ifelse(c(tadawul_raw11$hit90down[2:length(tadawul_raw11$hitdown)],0) == 1, 1, 0),
                        hit90up_after = ifelse(c(tadawul_raw11$hit90up[2:length(tadawul_raw11$hitdown)],0) == 1, 1, 0),
                        hit80down_after = ifelse(c(tadawul_raw11$hit80down[2:length(tadawul_raw11$hitdown)],0) == 1, 1, 0),
                        hit80up_after = ifelse(c(tadawul_raw11$hit80up[2:length(tadawul_raw11$hitdown)],0) == 1, 1, 0),
                        hit70down_after = ifelse(c(tadawul_raw11$hit70down[2:length(tadawul_raw11$hitdown)],0) == 1, 1, 0),
                        hit70up_after = ifelse(c(tadawul_raw11$hit70up[2:length(tadawul_raw11$hitdown)],0) == 1, 1, 0),
                        hitdown_before = ifelse(c(0,tadawul_raw11$hitdown[1:length(tadawul_raw11$hitdown)-1]) == 1, 1, 0),
                        low_hitdown_before = ifelse(c(0,tadawul_raw11$low_hitdown[1:length(tadawul_raw11$hitdown)-1]) == 1, 1, 0),
                        open_hitdown_before = ifelse(c(0,tadawul_raw11$open_hitdown[1:length(tadawul_raw11$hitdown)-1]) == 1, 1, 0),
                        High_hitdown_before = ifelse(c(0,tadawul_raw11$High_hitdown[1:length(tadawul_raw11$hitdown)-1]) == 1, 1, 0),
                        hitup_before = ifelse(c(0,tadawul_raw11$hitup[1:length(tadawul_raw11$hitdown)-1]) == 1, 1, 0),
                        high_hitup_before = ifelse(c(0,tadawul_raw11$high_hitup[1:length(tadawul_raw11$hitdown)-1]) == 1, 1, 0),
                        open_hitup_before = ifelse(c(0,tadawul_raw11$open_hitup[1:length(tadawul_raw11$hitdown)-1]) == 1, 1, 0),
                        low_hitup_before = ifelse(c(0,tadawul_raw11$low_hitup[1:length(tadawul_raw11$hitdown)-1]) == 1, 1, 0),
                        hit90down_before = ifelse(c(0,tadawul_raw11$hit90down[1:length(tadawul_raw11$hitdown)-1]) == 1, 1, 0),
                        hit90up_before = ifelse(c(0,tadawul_raw11$hit90up[1:length(tadawul_raw11$hitdown)-1]) == 1, 1, 0),
                        hit80down_before = ifelse(c(0,tadawul_raw11$hit80down[1:length(tadawul_raw11$hitdown)-1]) == 1, 1, 0),
                        hit80up_before = ifelse(c(0,tadawul_raw11$hit80up[1:length(tadawul_raw11$hitdown)-1]) == 1, 1, 0),
                        hit70down_before = ifelse(c(0,tadawul_raw11$hit70down[1:length(tadawul_raw11$hitdown)-1]) == 1, 1, 0),
                        hit70up_before = ifelse(c(0,tadawul_raw11$hit70up[1:length(tadawul_raw11$hitdown)-1]) == 1, 1, 0),
                        trading_activity = tadawul_raw11$Volume/tadawul_raw11$shares_float)


tadawul_raw2 <- filter(tadawul_raw111, Close > 0 & High != 0 & Open != 0)
tadawul_raw3 <- tadawul_raw2[tadawul_raw2[,1] >= "2013-05-26" ,]

### export the data to do the Differences in Differences in Stata
mkt_ret <- lm(data = tadawul_raw3, Log_Ret~log_Tasi)
finale <- mutate(tadawul_raw3, volatility = (tadawul_raw3$Log_Ret-(as.numeric(mkt_ret$coefficients[1])+as.numeric(mkt_ret$coefficients[2])*tadawul_raw3$log_Tasi))^2)
write.csv(finale, file = "finale.csv" )
summary(lm(data = finale[which(finale$hitdown == 1 | finale$hitdown_after == 1),], volatility~hitdown_before))


###############################
### Find limit hitting days ###
###############################

hitdown <- filter(tadawul_raw3, Close <= hit_down & Per_Chn < -5)
low_hitdown <- filter(tadawul_raw3, Low <= hit_down)
open_hitdown <- filter(tadawul_raw3, Open <= hit_down)
High_hitdown <- filter(tadawul_raw3, High <= hit_down & Per_Chn < -5)

hitup <- filter(tadawul_raw3, Close >= hit_up & Per_Chn > 5)
high_hitup <- filter(tadawul_raw3, High >= hit_up)
open_hitup <- filter(tadawul_raw3, Open >= hit_up)
low_hitup <- filter(tadawul_raw3, Low >= hit_up & Per_Chn > 5)

hit90down <- filter(tadawul_raw3, Close <= hit_90_down & Per_Chn < -5 & Close > hit_down) 
hit90up <- filter(tadawul_raw3, Close >= hit_90_up & Per_Chn > 5 & Close < hit_up)

hit80down <- filter(tadawul_raw3, Close <= hit_80_down & Per_Chn < -5 & Close > hit_90_down) 
hit80up <- filter(tadawul_raw3, Close >= hit_80_up & Per_Chn > 5 & Close < hit_90_up)

hit70down <- filter(tadawul_raw3, Close <= hit_70_down & Per_Chn < -5 & Close > hit_80_down) 
hit70up <- filter(tadawul_raw3, Close >= hit_70_up & Per_Chn > 5 & Close < hit_80_up)

###################


#########################################################
### Plot & statistics to describe return distribution ###
#########################################################

ggplot(hitdown, aes(x = Date, y = sum(hit_down))) + geom_count() + labs(x = "Date", y = "", title = "Limit Down per Date") + 
     scale_y_discrete(expand=c(0,0)) 

ggplot(hitup, aes(x = Date, y = sum(hit_up))) + geom_count() + labs(x = "Date", y = "", title = "Limit Up per Date") + 
     scale_y_discrete(expand=c(0,0)) 

ggplot(tadawul_raw3, aes(Per_Chn)) +geom_histogram(bins = 30) + labs(x = "Return", y = "", title = "Return Distribution")

describe(tadawul_raw3$Per_Chn)


#####################################################################################
### create tables for returns on limit hitting days and ten days before and after ###
#####################################################################################

### 1. for limit down ###

# create vector to hold rows number (number of the row in the main data frame were limit was hit)
rvec <- vector(mode = "numeric", length = NROW(hitdown))

# find row number
for (i in 1:NROW(hitdown)){
     options(warn=-1)
     rn <- as.numeric(which(tadawul_raw3$Date == hitdown[i,1] & tadawul_raw3$Per_Chn == cbind(rep(hitdown[i,7],length(tadawul_raw3)))[,1] & 
                                 tadawul_raw3$Code == cbind(rep(hitdown[i,15],length(tadawul_raw3)))[,1]))
     rvec[i] <- rn
}

# creat matrix to hold the indecies and data (ignore the warning)
rmat <- matrix(, nrow = 21, ncol = NROW(rvec))
dmat <- matrix(, nrow = 21, ncol = NROW(rvec))

# assign indecies for days before and after the limit (data are in descending order)
rmat[1,] <- rvec+10
rmat[2,] <- rvec+9
rmat[3,] <- rvec+8
rmat[4,] <- rvec+7
rmat[5,] <- rvec+6
rmat[6,] <- rvec+5
rmat[7,] <- rvec+4
rmat[8,] <- rvec+3
rmat[9,] <- rvec+2
rmat[10,] <- rvec+1
rmat[11,] <- rvec
rmat[12,] <- rvec-1
rmat[13,] <- rvec-2
rmat[14,] <- rvec-3
rmat[15,] <- rvec-4
rmat[16,] <- rvec-5
rmat[17,] <- rvec-6
rmat[18,] <- rvec-7
rmat[19,] <- rvec-8
rmat[20,] <- rvec-9
rmat[21,] <- rvec-10

# loop over the main data frame to extract data and save them properly in the data matrix (dmat)
for (i in 1:21){
     for (j in 1:NROW(hitdown)){
          dmat[i,j] <- as.numeric((tadawul_raw3[rmat[i,j],8])^2*1000)
     }
}

# find the mean of the variances
meanvec <- vector(mode = "numeric", length = 21)
for (i in 1:21){
     meanvec[i] <- median(dmat[i,])
}

#############################################################################################
### Same will be done again for limit up, 9% and 8% limits. No comments will be presented ###
#############################################################################################

### 2. for hit up days ###
rvecup <- vector(mode = "numeric", length = NROW(hitup))
for (i in 1:NROW(hitup)){
     options(warn=-1)
     rnu <- as.numeric(which(tadawul_raw3$Date == hitup[i,1] & tadawul_raw3$Per_Chn == cbind(rep(hitup[i,7],length(tadawul_raw3)))[,1] & 
                                  tadawul_raw3$Code == cbind(rep(hitup[i,15],length(tadawul_raw3)))[,1]))
     rvecup[i] <- rnu
}

rmatup <- matrix(, nrow = 21, ncol = NROW(rvecup))
dmatup <- matrix(, nrow = 21, ncol = NROW(rvecup))

rmatup[1,] <- rvecup+10
rmatup[2,] <- rvecup+9
rmatup[3,] <- rvecup+8
rmatup[4,] <- rvecup+7
rmatup[5,] <- rvecup+6
rmatup[6,] <- rvecup+5
rmatup[7,] <- rvecup+4
rmatup[8,] <- rvecup+3
rmatup[9,] <- rvecup+2
rmatup[10,] <- rvecup+1
rmatup[11,] <- rvecup
rmatup[12,] <- rvecup-1
rmatup[13,] <- rvecup-2
rmatup[14,] <- rvecup-3
rmatup[15,] <- rvecup-4
rmatup[16,] <- rvecup-5
rmatup[17,] <- rvecup-6
rmatup[18,] <- rvecup-7
rmatup[19,] <- rvecup-8
rmatup[20,] <- rvecup-9
rmatup[21,] <- rvecup-10

# loop over the main data frame to extract data and save them properly in the data matrix (dmat)
for (i in 1:21){
     for (j in 1:NROW(hitup)){
          dmatup[i,j] <- as.numeric((tadawul_raw3[rmatup[i,j],8])^2*1000)
     }
}

# find the mean of the variances
meanvecup <- vector(mode = "numeric", length = 21)
for (i in 1:21){
     meanvecup[i] <- median(dmatup[i,])
}

### 3. for 9% down ###
rvec9 <- vector(mode = "numeric", length = NROW(hit90down))

for (i in 1:NROW(hit90down)){
     options(warn=-1)
     rn9 <- as.numeric(which(tadawul_raw3$Date == hit90down[i,1] & tadawul_raw3$Per_Chn == cbind(rep(hit90down[i,7],length(tadawul_raw3)))[,1] & 
                                  tadawul_raw3$Code == cbind(rep(hit90down[i,15],length(tadawul_raw3)))[,1]))
     #rvec[[length(rvec)+1]] <- rn
     rvec9[i] <- rn9
}

rmat9 <- matrix(, nrow = 21, ncol = NROW(rvec9))
dmat9 <- matrix(, nrow = 21, ncol = NROW(rvec9))

rmat9[1,] <- rvec9+10
rmat9[2,] <- rvec9+9
rmat9[3,] <- rvec9+8
rmat9[4,] <- rvec9+7
rmat9[5,] <- rvec9+6
rmat9[6,] <- rvec9+5
rmat9[7,] <- rvec9+4
rmat9[8,] <- rvec9+3
rmat9[9,] <- rvec9+2
rmat9[10,] <- rvec9+1
rmat9[11,] <- rvec9
rmat9[12,] <- rvec9-1
rmat9[13,] <- rvec9-2
rmat9[14,] <- rvec9-3
rmat9[15,] <- rvec9-4
rmat9[16,] <- rvec9-5
rmat9[17,] <- rvec9-6
rmat9[18,] <- rvec9-7
rmat9[19,] <- rvec9-8
rmat9[20,] <- rvec9-9
rmat9[21,] <- rvec9-10

# loop over the main data frame to extract data and save them properly in the data matrix (dmat)
for (i in 1:21){
     for (j in 1:NROW(hit90down)){
          dmat9[i,j] <- as.numeric((tadawul_raw3[rmat9[i,j],8])^2*1000)
     }
}

# find the mean of the variances
meanvec9 <- vector(mode = "numeric", length = 21)
for (i in 1:21){
     meanvec9[i] <- median(dmat9[i,])
}

### 4. for 9% up ###
rvec9up <- vector(mode = "numeric", length = NROW(hit90up))

for (i in 1:NROW(hit90up)){
     options(warn=-1)
     rn9up <- as.numeric(which(tadawul_raw3$Date == hit90up[i,1] & tadawul_raw3$Per_Chn == cbind(rep(hit90up[i,7],length(tadawul_raw3)))[,1] & 
                                    tadawul_raw3$Code == cbind(rep(hit90up[i,15],length(tadawul_raw3)))[,1]))
     rvec9up[i] <- rn9up
}

rmat9up <- matrix(, nrow = 21, ncol = NROW(rvec9up))
dmat9up <- matrix(, nrow = 21, ncol = NROW(rvec9up))

rmat9up[1,] <- rvec9up+10
rmat9up[2,] <- rvec9up+9
rmat9up[3,] <- rvec9up+8
rmat9up[4,] <- rvec9up+7
rmat9up[5,] <- rvec9up+6
rmat9up[6,] <- rvec9up+5
rmat9up[7,] <- rvec9up+4
rmat9up[8,] <- rvec9up+3
rmat9up[9,] <- rvec9up+2
rmat9up[10,] <- rvec9up+1
rmat9up[11,] <- rvec9up
rmat9up[12,] <- rvec9up-1
rmat9up[13,] <- rvec9up-2
rmat9up[14,] <- rvec9up-3
rmat9up[15,] <- rvec9up-4
rmat9up[16,] <- rvec9up-5
rmat9up[17,] <- rvec9up-6
rmat9up[18,] <- rvec9up-7
rmat9up[19,] <- rvec9up-8
rmat9up[20,] <- rvec9up-9
rmat9up[21,] <- rvec9up-10

# loop over the main data frame to extract data and save them properly in the data matrix (dmat)
for (i in 1:21){
     for (j in 1:NROW(hit90up)){
          dmat9up[i,j] <- as.numeric((tadawul_raw3[rmat9up[i,j],8])^2*1000)
     }
}

# find the mean of the variances
meanvec9up <- vector(mode = "numeric", length = 21)
for (i in 1:21){
     meanvec9up[i] <- median(dmat9up[i,])
}

### 5. for 8 hit down ###
rvec8 <- vector(mode = "numeric", length = NROW(hit80down))

for (i in 1:NROW(hit80down)){
     options(warn=-1)
     rn8 <- as.numeric(which(tadawul_raw3$Date == hit80down[i,1] & tadawul_raw3$Per_Chn == cbind(rep(hit80down[i,7],length(tadawul_raw3)))[,1] & 
                                  tadawul_raw3$Code == cbind(rep(hit80down[i,15],length(tadawul_raw3)))[,1]))
     rvec8[i] <- rn8
}

rmat8 <- matrix(, nrow = 21, ncol = NROW(rvec8))
dmat8 <- matrix(, nrow = 21, ncol = NROW(rvec8))

rmat8[1,] <- rvec8+10
rmat8[2,] <- rvec8+9
rmat8[3,] <- rvec8+8
rmat8[4,] <- rvec8+7
rmat8[5,] <- rvec8+6
rmat8[6,] <- rvec8+5
rmat8[7,] <- rvec8+4
rmat8[8,] <- rvec8+3
rmat8[9,] <- rvec8+2
rmat8[10,] <- rvec8+1
rmat8[11,] <- rvec8
rmat8[12,] <- rvec8-1
rmat8[13,] <- rvec8-2
rmat8[14,] <- rvec8-3
rmat8[15,] <- rvec8-4
rmat8[16,] <- rvec8-5
rmat8[17,] <- rvec8-6
rmat8[18,] <- rvec8-7
rmat8[19,] <- rvec8-8
rmat8[20,] <- rvec8-9
rmat8[21,] <- rvec8-10


# loop over the main data frame to extract data and save them properly in the data matrix (dmat)
for (i in 1:21){
     for (j in 1:NROW(hit80down)){
          dmat8[i,j] <- as.numeric((tadawul_raw3[rmat8[i,j],8])^2*1000)
     }
}

# find the mean of the variances
meanvec8 <- vector(mode = "numeric", length = 21)
for (i in 1:21){
     meanvec8[i] <- median(dmat8[i,],na.rm = T)
}

### 6. for 8 hit up ###
rvec8up <- vector(mode = "numeric", length = NROW(hit80up))

for (i in 1:NROW(hit80up)){
     options(warn=-1)
     rn8up <- as.numeric(which(tadawul_raw3$Date == hit80up[i,1] & tadawul_raw3$Per_Chn == cbind(rep(hit80up[i,7],length(tadawul_raw3)))[,1] & 
                                    tadawul_raw3$Code == cbind(rep(hit80up[i,15],length(tadawul_raw3)))[,1]))
     rvec8up[i] <- rn8up
}

rmat8up <- matrix(, nrow = 21, ncol = NROW(rvec8up))
dmat8up <- matrix(, nrow = 21, ncol = NROW(rvec8up))

rmat8up[1,] <- rvec8up+10
rmat8up[2,] <- rvec8up+9
rmat8up[3,] <- rvec8up+8
rmat8up[4,] <- rvec8up+7
rmat8up[5,] <- rvec8up+6
rmat8up[6,] <- rvec8up+5
rmat8up[7,] <- rvec8up+4
rmat8up[8,] <- rvec8up+3
rmat8up[9,] <- rvec8up+2
rmat8up[10,] <- rvec8up+1
rmat8up[11,] <- rvec8up
rmat8up[12,] <- rvec8up-1
rmat8up[13,] <- rvec8up-2
rmat8up[14,] <- rvec8up-3
rmat8up[15,] <- rvec8up-4
rmat8up[16,] <- rvec8up-5
rmat8up[17,] <- rvec8up-6
rmat8up[18,] <- rvec8up-7
rmat8up[19,] <- rvec8up-8
rmat8up[20,] <- rvec8up-9
rmat8up[21,] <- rvec8up-10

# loop over the main data frame to extract data and save them properly in the data matrix (dmat)
for (i in 1:21){
     for (j in 1:NROW(hit80up)){
          dmat8up[i,j] <- as.numeric((tadawul_raw3[rmat8up[i,j],8])^2*1000)
     }
}

# find the mean of the variances
meanvec8up <- vector(mode = "numeric", length = 21)
for (i in 1:21){
     meanvec8up[i] <- median(dmat8up[i,])
}

###################
### wilcox test ###
###################

### 1. limit down and 9% down ###
alltl9 <- matrix(, nrow = 21, ncol = 1)
for (j in 1:21){
     alltl9[j] <- round(wilcox.test(dmat[j,],dmat9[j,],paired = F, alternative = "greater")$p.value,3)
}

### 2. 9% and 8% down ###
allt98 <- matrix(, nrow = 21, ncol = 1)
for (j in 1:21){
     allt98[j] <- round(wilcox.test(dmat9[j,],dmat8[j,],paired = F, alternative = "greater")$p.value,3)
}


### 3. limit up and 9% up ###

alltl9up <- matrix(, nrow = 21, ncol = 1)
for (j in 1:21){
     alltl9up[j] <- round(wilcox.test(dmatup[j,],dmat9up[j,],paired = F, alternative = "greater")$p.value,3)
}


### 4. 9% and 8% up ###

allt98up <- matrix(, nrow = 21, ncol = 1)
for (j in 1:21){
     allt98up[j] <- round(wilcox.test(dmat9up[j,],dmat8up[j,],paired = F, alternative = "greater")$p.value,3)
}

### before and after the limit ###
ball <- round(wilcox.test(dmat[10,],dmat[12,],paired = F, alternative = "greater")$p.value,3)
ba99 <- round(wilcox.test(dmat9[10,],dmat9[12,],paired = F, alternative = "less")$p.value,3)
ba88 <- round(wilcox.test(dmat8[10,],dmat8[12,],paired = F, alternative = "less")$p.value,3)

################################################################################################
### Now we will look at periods of instability in the market and do what we done above again ###
################################################################################################


### find periods of market instability ###

tadawul_rawmi <- filter(tadawul_raw2, Tasi_R <= -1.28 | Tasi_R >= 1.28 & Date >= "2013-05-20" & High != 0 & Open != 0)

hitdownmi <- filter(tadawul_rawmi, Close <= hit_down & Per_Chn < -5)
low_hitdownmi <- filter(tadawul_rawmi, Low <= hit_down)
open_hitdownmi <- filter(tadawul_rawmi, Open <= hit_down)
High_hitdownmi <- filter(tadawul_rawmi, High <= hit_down & Per_Chn < -5)

hitupmi <- filter(tadawul_rawmi, Close >= hit_up & Per_Chn > 5)
high_hitupmi <- filter(tadawul_rawmi, High >= hit_up)
open_hitupmi <- filter(tadawul_rawmi, Open >= hit_up)
low_hitupmi <- filter(tadawul_rawmi, Low >= hit_up & Per_Chn > 5)

hit90downmi <- filter(tadawul_rawmi, Close <= hit_90_down & Per_Chn < -5 & Close > hit_down) 
hit90upmi <- filter(tadawul_rawmi, Close >= hit_90_up & Per_Chn > 5 & Close < hit_up)

hit80downmi <- filter(tadawul_rawmi, Close <= hit_80_down & Per_Chn < -5 & Close > hit_90_down) 
hit80upmi <- filter(tadawul_rawmi, Close >= hit_80_up & Per_Chn > 5 & Close < hit_90_up)

hit70downmi <- filter(tadawul_rawmi, Close <= hit_70_down & Per_Chn < -5 & Close > hit_80_down) 
hit70upmi <- filter(tadawul_rawmi, Close >= hit_70_up & Per_Chn > 5 & Close < hit_80_up)

### plots and statistics for returns in unstable periods ###
ggplot(tadawul_rawmi, aes(Per_Chn)) +geom_histogram(bins = 44) + labs(title = "Returns in Periods of Instability", x = "Returns")

describe(tadawul_rawmi$Per_Chn)

######################################################################################
### create tables for returns on limit hitting days and ten days before and after. ###
######################################################################################

### 1. for hit down ###
rvecmi <- vector(mode = "numeric", length = NROW(hitdownmi))

for (i in 1:NROW(hitdownmi)){
     options(warn=-1)
     rnmi <- as.numeric(which(tadawul_raw3$Date == hitdownmi[i,1] & tadawul_raw3$Per_Chn == cbind(rep(hitdownmi[i,7],length(tadawul_raw3)))[,1] & 
                                   tadawul_raw3$Code == cbind(rep(hitdownmi[i,15],length(tadawul_raw3)))[,1]))
     rvecmi[i] <- rnmi
}

rmatmi <- matrix(, nrow = 21, ncol = NROW(rvecmi))
dmatmi <- matrix(, nrow = 21, ncol = NROW(rvecmi))

rmatmi[1,] <- rvecmi+10
rmatmi[2,] <- rvecmi+9
rmatmi[3,] <- rvecmi+8
rmatmi[4,] <- rvecmi+7
rmatmi[5,] <- rvecmi+6
rmatmi[6,] <- rvecmi+5
rmatmi[7,] <- rvecmi+4
rmatmi[8,] <- rvecmi+3
rmatmi[9,] <- rvecmi+2
rmatmi[10,] <- rvecmi+1
rmatmi[11,] <- rvecmi
rmatmi[12,] <- rvecmi-1
rmatmi[13,] <- rvecmi-2
rmatmi[14,] <- rvecmi-3
rmatmi[15,] <- rvecmi-4
rmatmi[16,] <- rvecmi-5
rmatmi[17,] <- rvecmi-6
rmatmi[18,] <- rvecmi-7
rmatmi[19,] <- rvecmi-8
rmatmi[20,] <- rvecmi-9
rmatmi[21,] <- rvecmi-10


# loop over the main data frame to extract data and save them properly in the data matrix (dmat)
for (i in 1:21){
     for (j in 1:NROW(hitdownmi)){
          dmatmi[i,j] <- as.numeric((tadawul_raw3[rmatmi[i,j],8])^2*1000)
     }
}

# find the mean of the variances
meanvecmi <- vector(mode = "numeric", length = 21)
for (i in 1:21){
     meanvecmi[i] <- median(dmatmi[i,])
}

### 2. for hit up days ###
rvecupmi <- vector(mode = "numeric", length = NROW(hitupmi))

for (i in 1:NROW(hitupmi)){
     options(warn=-1)
     rnumi <- as.numeric(which(tadawul_raw3$Date == hitupmi[i,1] & tadawul_raw3$Per_Chn == cbind(rep(hitupmi[i,7],length(tadawul_raw3)))[,1] & 
                                    tadawul_raw3$Code == cbind(rep(hitupmi[i,15],length(tadawul_raw3)))[,1]))
     rvecupmi[i] <- rnumi
}

rmatupmi <- matrix(, nrow = 21, ncol = NROW(rvecupmi))
dmatupmi <- matrix(, nrow = 21, ncol = NROW(rvecupmi))

rmatupmi[1,] <- rvecupmi+10
rmatupmi[2,] <- rvecupmi+9
rmatupmi[3,] <- rvecupmi+8
rmatupmi[4,] <- rvecupmi+7
rmatupmi[5,] <- rvecupmi+6
rmatupmi[6,] <- rvecupmi+5
rmatupmi[7,] <- rvecupmi+4
rmatupmi[8,] <- rvecupmi+3
rmatupmi[9,] <- rvecupmi+2
rmatupmi[10,] <- rvecupmi+1
rmatupmi[11,] <- rvecupmi
rmatupmi[12,] <- rvecupmi-1
rmatupmi[13,] <- rvecupmi-2
rmatupmi[14,] <- rvecupmi-3
rmatupmi[15,] <- rvecupmi-4
rmatupmi[16,] <- rvecupmi-5
rmatupmi[17,] <- rvecupmi-6
rmatupmi[18,] <- rvecupmi-7
rmatupmi[19,] <- rvecupmi-8
rmatupmi[20,] <- rvecupmi-9
rmatupmi[21,] <- rvecupmi-10


# loop over the main data frame to extract data and save them properly in the data matrix (dmat)
for (i in 1:21){
     for (j in 1:NROW(hitupmi)){
          dmatupmi[i,j] <- as.numeric((tadawul_raw3[rmatupmi[i,j],8])^2*1000)
     }
}

# find the mean of the variances
meanvecupmi <- vector(mode = "numeric", length = 21)
for (i in 1:21){
     meanvecupmi[i] <- median(dmatupmi[i,])
}

### 3. for 9% down ###
rvec9mi <- vector(mode = "numeric", length = NROW(hit90downmi))

for (i in 1:NROW(hit90downmi)){
     options(warn=-1)
     rn9mi <- as.numeric(which(tadawul_raw3$Date == hit90downmi[i,1] & tadawul_raw3$Per_Chn == cbind(rep(hit90downmi[i,7],length(tadawul_raw3)))[,1] & 
                                    tadawul_raw3$Code == cbind(rep(hit90downmi[i,15],length(tadawul_raw3)))[,1]))
     rvec9mi[i] <- rn9mi
}

rmat9mi <- matrix(, nrow = 21, ncol = NROW(rvec9mi))
dmat9mi <- matrix(, nrow = 21, ncol = NROW(rvec9mi))

rmat9mi[1,] <- rvec9mi+10
rmat9mi[2,] <- rvec9mi+9
rmat9mi[3,] <- rvec9mi+8
rmat9mi[4,] <- rvec9mi+7
rmat9mi[5,] <- rvec9mi+6
rmat9mi[6,] <- rvec9mi+5
rmat9mi[7,] <- rvec9mi+4
rmat9mi[8,] <- rvec9mi+3
rmat9mi[9,] <- rvec9mi+2
rmat9mi[10,] <- rvec9mi+1
rmat9mi[11,] <- rvec9mi
rmat9mi[12,] <- rvec9mi-1
rmat9mi[13,] <- rvec9mi-2
rmat9mi[14,] <- rvec9mi-3
rmat9mi[15,] <- rvec9mi-4
rmat9mi[16,] <- rvec9mi-5
rmat9mi[17,] <- rvec9mi-6
rmat9mi[18,] <- rvec9mi-7
rmat9mi[19,] <- rvec9mi-8
rmat9mi[20,] <- rvec9mi-9
rmat9mi[21,] <- rvec9mi-10

# loop over the main data frame to extract data and save them properly in the data matrix (dmat)
for (i in 1:21){
     for (j in 1:NROW(hit90downmi)){
          dmat9mi[i,j] <- as.numeric((tadawul_raw3[rmat9mi[i,j],8])^2*1000)
     }
}

# find the mean of the variances
meanvec9mi <- vector(mode = "numeric", length = 21)
for (i in 1:21){
     meanvec9mi[i] <- median(dmat9mi[i,])
}

### 4. for 9% up ###
rvec9upmi <- vector(mode = "numeric", length = NROW(hit90upmi))

for (i in 1:NROW(hit90upmi)){
     options(warn=-1)
     rn9upmi <- as.numeric(which(tadawul_raw3$Date == hit90upmi[i,1] & tadawul_raw3$Per_Chn == cbind(rep(hit90upmi[i,7],length(tadawul_raw3)))[,1] & 
                                      tadawul_raw3$Code == cbind(rep(hit90upmi[i,15],length(tadawul_raw3)))[,1]))
     rvec9upmi[i] <- rn9upmi
}

rmat9upmi <- matrix(, nrow = 21, ncol = NROW(rvec9upmi))
dmat9upmi <- matrix(, nrow = 21, ncol = NROW(rvec9upmi))

rmat9upmi[1,] <- rvec9upmi+10
rmat9upmi[2,] <- rvec9upmi+9
rmat9upmi[3,] <- rvec9upmi+8
rmat9upmi[4,] <- rvec9upmi+7
rmat9upmi[5,] <- rvec9upmi+6
rmat9upmi[6,] <- rvec9upmi+5
rmat9upmi[7,] <- rvec9upmi+4
rmat9upmi[8,] <- rvec9upmi+3
rmat9upmi[9,] <- rvec9upmi+2
rmat9upmi[10,] <- rvec9upmi+1
rmat9upmi[11,] <- rvec9upmi
rmat9upmi[12,] <- rvec9upmi-1
rmat9upmi[13,] <- rvec9upmi-2
rmat9upmi[14,] <- rvec9upmi-3
rmat9upmi[15,] <- rvec9upmi-4
rmat9upmi[16,] <- rvec9upmi-5
rmat9upmi[17,] <- rvec9upmi-6
rmat9upmi[18,] <- rvec9upmi-7
rmat9upmi[19,] <- rvec9upmi-8
rmat9upmi[20,] <- rvec9upmi-9
rmat9upmi[21,] <- rvec9upmi-10

# loop over the main data frame to extract data and save them properly in the data matrix (dmat)
for (i in 1:21){
     for (j in 1:NROW(hit90upmi)){
          dmat9upmi[i,j] <- as.numeric((tadawul_raw3[rmat9upmi[i,j],8])^2*1000)
     }
}

# find the mean of the variances
meanvec9upmi <- vector(mode = "numeric", length = 21)
for (i in 1:21){
     meanvec9upmi[i] <- median(dmat9upmi[i,])
}

### 5. for 8 hit down ###
rvec8mi <- vector(mode = "numeric", length = NROW(hit80downmi))

for (i in 1:NROW(hit80downmi)){
     options(warn=-1)
     rn8mi <- as.numeric(which(tadawul_raw3$Date == hit80downmi[i,1] & tadawul_raw3$Per_Chn == cbind(rep(hit80downmi[i,7],length(tadawul_raw3)))[,1] & 
                                    tadawul_raw3$Code == cbind(rep(hit80downmi[i,15],length(tadawul_raw3)))[,1]))
     rvec8mi[i] <- rn8mi
}

rmat8mi <- matrix(, nrow = 21, ncol = NROW(rvec8mi))
dmat8mi <- matrix(, nrow = 21, ncol = NROW(rvec8mi))

rmat8mi[1,] <- rvec8mi+10
rmat8mi[2,] <- rvec8mi+9
rmat8mi[3,] <- rvec8mi+8
rmat8mi[4,] <- rvec8mi+7
rmat8mi[5,] <- rvec8mi+6
rmat8mi[6,] <- rvec8mi+5
rmat8mi[7,] <- rvec8mi+4
rmat8mi[8,] <- rvec8mi+3
rmat8mi[9,] <- rvec8mi+2
rmat8mi[10,] <- rvec8mi+1
rmat8mi[11,] <- rvec8mi
rmat8mi[12,] <- rvec8mi-1
rmat8mi[13,] <- rvec8mi-2
rmat8mi[14,] <- rvec8mi-3
rmat8mi[15,] <- rvec8mi-4
rmat8mi[16,] <- rvec8mi-5
rmat8mi[17,] <- rvec8mi-6
rmat8mi[18,] <- rvec8mi-7
rmat8mi[19,] <- rvec8mi-8
rmat8mi[20,] <- rvec8mi-9
rmat8mi[21,] <- rvec8mi-10

# loop over the main data frame to extract data and save them properly in the data matrix (dmat)
for (i in 1:21){
     for (j in 1:NROW(hit80downmi)){
          dmat8mi[i,j] <- as.numeric((tadawul_raw3[rmat8mi[i,j],8])^2*1000)
     }
}

# find the mean of the variances
meanvec8mi <- vector(mode = "numeric", length = 21)
for (i in 1:21){
     meanvec8mi[i] <- median(dmat8mi[i,])
}

### 6. for 8 hit up ###
rvec8upmi <- vector(mode = "numeric", length = NROW(hit80upmi))

for (i in 1:NROW(hit80upmi)){
     options(warn=-1)
     rn8upmi <- as.numeric(which(tadawul_raw3$Date == hit80upmi[i,1] & tadawul_raw3$Per_Chn == cbind(rep(hit80upmi[i,7],length(tadawul_raw3)))[,1] & 
                                      tadawul_raw3$Code == cbind(rep(hit80upmi[i,15],length(tadawul_raw3)))[,1]))
     rvec8upmi[i] <- rn8upmi
}

rmat8upmi <- matrix(, nrow = 21, ncol = NROW(rvec8upmi))
dmat8upmi <- matrix(, nrow = 21, ncol = NROW(rvec8upmi))

rmat8upmi[1,] <- rvec8upmi+10
rmat8upmi[2,] <- rvec8upmi+9
rmat8upmi[3,] <- rvec8upmi+8
rmat8upmi[4,] <- rvec8upmi+7
rmat8upmi[5,] <- rvec8upmi+6
rmat8upmi[6,] <- rvec8upmi+5
rmat8upmi[7,] <- rvec8upmi+4
rmat8upmi[8,] <- rvec8upmi+3
rmat8upmi[9,] <- rvec8upmi+2
rmat8upmi[10,] <- rvec8upmi+1
rmat8upmi[11,] <- rvec8upmi
rmat8upmi[12,] <- rvec8upmi-1
rmat8upmi[13,] <- rvec8upmi-2
rmat8upmi[14,] <- rvec8upmi-3
rmat8upmi[15,] <- rvec8upmi-4
rmat8upmi[16,] <- rvec8upmi-5
rmat8upmi[17,] <- rvec8upmi-6
rmat8upmi[18,] <- rvec8upmi-7
rmat8upmi[19,] <- rvec8upmi-8
rmat8upmi[20,] <- rvec8upmi-9
rmat8upmi[21,] <- rvec8upmi-10

# loop over the main data frame to extract data and save them properly in the data matrix (dmat)
for (i in 1:21){
     for (j in 1:NROW(hit80upmi)){
          dmat8upmi[i,j] <- as.numeric((tadawul_raw3[rmat8upmi[i,j],8])^2*1000)
     }
}

# find the mean of the variances
meanvec8upmi <- vector(mode = "numeric", length = 21)
for (i in 1:21){
     meanvec8upmi[i] <- median(dmat8upmi[i,])
}
###################
### Wilcox test ###
###################

### 1. limit down and 9% down instable periods (run twice) ###

alltl9mi <- matrix(, nrow = 21, ncol = 1)
for (j in 1:21){
     alltl9mi[j] <- round(wilcox.test(dmatmi[j,], dmat9mi[j,], paired = F, alternative = "less")$p.value,3)
}

alltl9mig <- matrix(, nrow = 21, ncol = 1)
for (j in 1:21){
     alltl9mig[j] <- round(wilcox.test(dmatmi[j,], dmat9mi[j,], paired = F, alternative = "greater")$p.value,3)
}

### 2. 9% and 8% down ###

allt98mi <- matrix(, nrow = 21, ncol = 1)
for (j in 1:21){
     allt98mi[j] <- round(wilcox.test(dmat9mi[j,], dmat8mi[j,], paired = F, alternative = "less")$p.value,3)
}

### 3. limit up and 9% up instable periods ###

alltl9upmi <- matrix(, nrow = 21, ncol = 1)
for (j in 1:21){
     alltl9upmi[j] <- round(wilcox.test(dmatupmi[j,], dmat9upmi[j,], paired = F, alternative = "less")$p.value,3)
}

### 4. 9% and 8% up ###

allt98upmi <- matrix(, nrow = 21, ncol = 1)
for (j in 1:21){
     allt98upmi[j] <- round(wilcox.test(dmat9upmi[j,], dmat8upmi[j,], paired = F, alternative = "less")$p.value,3)
}

#######################################
### periods of normal market return ###
#######################################

### find periods of upnormal market return ###

tadawul_rawms <- filter(tadawul_raw2, Tasi_R > -1.28 & Tasi_R < 1.28 & Date >= "2013-05-20" & High != 0 & Open != 0)

hitdownms <- filter(tadawul_rawms, Close <= hit_down & Per_Chn < -5)
low_hitdownms <- filter(tadawul_rawms, Low <= hit_down)
open_hitdownms <- filter(tadawul_rawms, Open <= hit_down)
High_hitdownms <- filter(tadawul_rawms, High <= hit_down & Per_Chn < -5)

hitupms <- filter(tadawul_rawms, Close >= hit_up & Per_Chn > 5)
high_hitupms <- filter(tadawul_rawms, High >= hit_up)
open_hitupms <- filter(tadawul_rawms, Open >= hit_up)
low_hitupms <- filter(tadawul_rawms, Low >= hit_up & Per_Chn > 5)

hit90downms <- filter(tadawul_rawms, Close <= hit_90_down & Per_Chn < -5 & Close > hit_down) 
hit90upms <- filter(tadawul_rawms, Close >= hit_90_up & Per_Chn > 5 & Close < hit_up)

hit80downms <- filter(tadawul_rawms, Close <= hit_80_down & Per_Chn < -5 & Close > hit_90_down) 
hit80upms <- filter(tadawul_rawms, Close >= hit_80_up & Per_Chn > 5 & Close < hit_90_up)

hit70downms <- filter(tadawul_rawms, Close <= hit_70_down & Per_Chn < -5 & Close > hit_80_down) 
hit70upms <- filter(tadawul_rawms, Close >= hit_70_up & Per_Chn > 5 & Close < hit_80_up)

### plots and statistics for return in this period ###
ggplot(tadawul_rawms, aes(Per_Chn)) +geom_histogram(bins = 30)
describe(tadawul_rawms$Per_Chn)

######################################################################################
### create tables for returns on limit hitting days and ten days before and after. ###
######################################################################################

### 1. for hit down ###
rvecms <- vector(mode = "numeric", length = NROW(hitdownms))

for (i in 1:NROW(hitdownms)){
     options(warn=-1)
     rnms <- as.numeric(which(tadawul_raw3$Date == hitdownms[i,1] & tadawul_raw3$Per_Chn == cbind(rep(hitdownms[i,7],length(tadawul_raw3)))[,1] & 
                                   tadawul_raw3$Code == cbind(rep(hitdownms[i,15],length(tadawul_raw3)))[,1]))
     rvecms[i] <- rnms
}

rmatms <- matrix(, nrow = 21, ncol = NROW(rvecms))
dmatms <- matrix(, nrow = 21, ncol = NROW(rvecms))

rmatms[1,] <- rvecms+10
rmatms[2,] <- rvecms+9
rmatms[3,] <- rvecms+8
rmatms[4,] <- rvecms+7
rmatms[5,] <- rvecms+6
rmatms[6,] <- rvecms+5
rmatms[7,] <- rvecms+4
rmatms[8,] <- rvecms+3
rmatms[9,] <- rvecms+2
rmatms[10,] <- rvecms+1
rmatms[11,] <- rvecms
rmatms[12,] <- rvecms-1
rmatms[13,] <- rvecms-2
rmatms[14,] <- rvecms-3
rmatms[15,] <- rvecms-4
rmatms[16,] <- rvecms-5
rmatms[17,] <- rvecms-6
rmatms[18,] <- rvecms-7
rmatms[19,] <- rvecms-8
rmatms[20,] <- rvecms-9
rmatms[21,] <- rvecms-10


# loop over the main data frame to extract data and save them properly in the data matrix (dmat)
for (i in 1:21){
     for (j in 1:NROW(hitdownms)){
          dmatms[i,j] <- as.numeric((tadawul_raw3[rmatms[i,j],8])^2*1000)
     }
}

# find the mean of the variances
meanvecms <- vector(mode = "numeric", length = 21)
for (i in 1:21){
     meanvecms[i] <- median(dmatms[i,])
}
### 2. Limit up ###
rvecupms <- vector(mode = "numeric", length = NROW(hitupmi))

for (i in 1:NROW(hitupms)){
     options(warn=-1)
     rnums <- as.numeric(which(tadawul_raw3$Date == hitupms[i,1] & tadawul_raw3$Per_Chn == cbind(rep(hitupms[i,7],length(tadawul_raw3)))[,1] & 
                                    tadawul_raw3$Code == cbind(rep(hitupms[i,15],length(tadawul_raw3)))[,1]))
     rvecupms[i] <- rnums
}

rmatupms <- matrix(, nrow = 21, ncol = NROW(rvecupms))
dmatupms <- matrix(, nrow = 21, ncol = NROW(rvecupms))

rmatupms[1,] <- rvecupms+10
rmatupms[2,] <- rvecupms+9
rmatupms[3,] <- rvecupms+8
rmatupms[4,] <- rvecupms+7
rmatupms[5,] <- rvecupms+6
rmatupms[6,] <- rvecupms+5
rmatupms[7,] <- rvecupms+4
rmatupms[8,] <- rvecupms+3
rmatupms[9,] <- rvecupms+2
rmatupms[10,] <- rvecupms+1
rmatupms[11,] <- rvecupms
rmatupms[12,] <- rvecupms-1
rmatupms[13,] <- rvecupms-2
rmatupms[14,] <- rvecupms-3
rmatupms[15,] <- rvecupms-4
rmatupms[16,] <- rvecupms-5
rmatupms[17,] <- rvecupms-6
rmatupms[18,] <- rvecupms-7
rmatupms[19,] <- rvecupms-8
rmatupms[20,] <- rvecupms-9
rmatupms[21,] <- rvecupms-10

# loop over the main data frame to extract data and save them properly in the data matrix (dmat)
for (i in 1:21){
     for (j in 1:NROW(hitupms)){
          dmatupms[i,j] <- as.numeric((tadawul_raw3[rmatupms[i,j],8])^2*1000)
     }
}

# find the mean of the variances
meanvecupms <- vector(mode = "numeric", length = 21)
for (i in 1:21){
     meanvecupms[i] <- median(dmatupms[i,])
}
### 3. for 9% down ###
rvec9ms <- vector(mode = "numeric", length = NROW(hit90downms))

for (i in 1:NROW(hit90downms)){
     options(warn=-1)
     rn9ms <- as.numeric(which(tadawul_raw3$Date == hit90downms[i,1] & tadawul_raw3$Per_Chn == cbind(rep(hit90downms[i,7],length(tadawul_raw3)))[,1] & 
                                    tadawul_raw3$Code == cbind(rep(hit90downms[i,15],length(tadawul_raw3)))[,1]))
     rvec9ms[i] <- rn9ms
}

rmat9ms <- matrix(, nrow = 21, ncol = NROW(rvec9ms))
dmat9ms <- matrix(, nrow = 21, ncol = NROW(rvec9ms))

rmat9ms[1,] <- rvec9ms+10
rmat9ms[2,] <- rvec9ms+9
rmat9ms[3,] <- rvec9ms+8
rmat9ms[4,] <- rvec9ms+7
rmat9ms[5,] <- rvec9ms+6
rmat9ms[6,] <- rvec9ms+5
rmat9ms[7,] <- rvec9ms+4
rmat9ms[8,] <- rvec9ms+3
rmat9ms[9,] <- rvec9ms+2
rmat9ms[10,] <- rvec9ms+1
rmat9ms[11,] <- rvec9ms
rmat9ms[12,] <- rvec9ms-1
rmat9ms[13,] <- rvec9ms-2
rmat9ms[14,] <- rvec9ms-3
rmat9ms[15,] <- rvec9ms-4
rmat9ms[16,] <- rvec9ms-5
rmat9ms[17,] <- rvec9ms-6
rmat9ms[18,] <- rvec9ms-7
rmat9ms[19,] <- rvec9ms-8
rmat9ms[20,] <- rvec9ms-9
rmat9ms[21,] <- rvec9ms-10

# loop over the main data frame to extract data and save them properly in the data matrix (dmat)
for (i in 1:21){
     for (j in 1:NROW(hit90downms)){
          dmat9ms[i,j] <- as.numeric((tadawul_raw3[rmat9ms[i,j],8])^2*1000)
     }
}

# find the mean of the variances
meanvec9ms <- vector(mode = "numeric", length = 21)
for (i in 1:21){
     meanvec9ms[i] <- median(dmat9ms[i,])
}

### 4. for 9% up ###
rvec9upms <- vector(mode = "numeric", length = NROW(hit90upms))

for (i in 1:NROW(hit90upms)){
     options(warn=-1)
     rn9upms <- as.numeric(which(tadawul_raw3$Date == hit90upms[i,1] & tadawul_raw3$Per_Chn == cbind(rep(hit90upms[i,7],length(tadawul_raw3)))[,1] & 
                                      tadawul_raw3$Code == cbind(rep(hit90upms[i,15],length(tadawul_raw3)))[,1]))
     rvec9upms[i] <- rn9upms
}

rmat9upms <- matrix(, nrow = 21, ncol = NROW(rvec9upms))
dmat9upms <- matrix(, nrow = 21, ncol = NROW(rvec9upms))

rmat9upms[1,] <- rvec9upms+10
rmat9upms[2,] <- rvec9upms+9
rmat9upms[3,] <- rvec9upms+8
rmat9upms[4,] <- rvec9upms+7
rmat9upms[5,] <- rvec9upms+6
rmat9upms[6,] <- rvec9upms+5
rmat9upms[7,] <- rvec9upms+4
rmat9upms[8,] <- rvec9upms+3
rmat9upms[9,] <- rvec9upms+2
rmat9upms[10,] <- rvec9upms+1
rmat9upms[11,] <- rvec9upms
rmat9upms[12,] <- rvec9upms-1
rmat9upms[13,] <- rvec9upms-2
rmat9upms[14,] <- rvec9upms-3
rmat9upms[15,] <- rvec9upms-4
rmat9upms[16,] <- rvec9upms-5
rmat9upms[17,] <- rvec9upms-6
rmat9upms[18,] <- rvec9upms-7
rmat9upms[19,] <- rvec9upms-8
rmat9upms[20,] <- rvec9upms-9
rmat9upms[21,] <- rvec9upms-10

# loop over the main data frame to extract data and save them properly in the data matrix (dmat)
for (i in 1:21){
     for (j in 1:NROW(hit90upms)){
          dmat9upms[i,j] <- as.numeric((tadawul_raw3[rmat9upms[i,j],8])^2*1000)
     }
}

# find the mean of the variances
meanvec9upms <- vector(mode = "numeric", length = 21)
for (i in 1:21){
     meanvec9upms[i] <- median(dmat9upms[i,])
}


################################################################
### wilcoxtest for normal vs upnormal market return periods. ###
################################################################

### 1. limit down upnormal vs normal###
alltllmsmi <- matrix(, nrow = 21, ncol = 1)
for (j in 1:21){
     alltllmsmi[j] <- round(wilcox.test(dmatmi[j,],dmatms[j,],paired = F, alternative = "less")$p.value,3)
}

### 2. limit down vs 9% down upnormal ###
alltl9mi <- matrix(, nrow = 21, ncol = 1)
for (j in 1:21){
     alltl9[j] <- round(wilcox.test(dmatmi[j,], dmat9mi[j,], paired = F, alternative = "less")$p.value,3)
}

### 3. limit down vs 9% down normal ###
alltl9ms <- matrix(, nrow = 21, ncol = 1)
for (j in 1:21){
     alltl9ms[j] <- round(wilcox.test(dmatms[j,], dmat9ms[j,], paired = F, alternative = "less")$p.value,3)
}

### 4. limit up upnormal vs normal ###
alltllupmsmi <- matrix(, nrow = 21, ncol = 1)
for (j in 1:21){
     alltllupmsmi[j] <- round(wilcox.test(dmatupmi[j,], dmatupms[j,], paired = F, alternative = "less")$p.value,3)
}

#alltllupmsmig <- matrix(, nrow = 21, ncol = 1)
#for (j in 1:21){
#     alltllupmsmig[j] <- round(wilcox.test(mdmatupmi[j,], mdmatupms[j,], paired = F, alternative = "greater")$p.value,3)
#}

###################################
### Second method (probability) ###
###################################
### full period ###
ever_hitdown <- filter(tadawul_raw3, Low <= hit_down)
ever_hitdown_NC <- filter(tadawul_raw3, Low <= hit_down & Close > hit_down)
ever_hitdown_C <- filter(tadawul_raw3, Close <= hit_down & Per_Chn < -5)

ever_hit90down <- filter(tadawul_raw3, Low <= hit_90_down & Low > hit_down)
ever_hit90down_NC <- filter(tadawul_raw3, Low <= hit_90_down & Low > hit_down & Close > hit_90_down)

ever_hitup <- filter(tadawul_raw3, High >= hit_up)
ever_hitup_NC <- filter(tadawul_raw3, High >= hit_up & Close < hit_up)

ever_hit90up <- filter(tadawul_raw3, High >= hit_90_up & High < hit_up)
ever_hit90up_NC <- filter(tadawul_raw3, High >= hit_90_up & Close < hit_90_up & High < hit_up)

### abnormal ###
MI_ever_hitdown <- filter(tadawul_rawmi, Low <= hit_down)
MI_ever_hitdown_NC <- filter(tadawul_rawmi, Low <= hit_down & Close > hit_down)

MI_ever_hit90down <- filter(tadawul_rawmi, Low <= hit_90_down & Low > hit_down)
MI_ever_hit90down_NC <- filter(tadawul_rawmi, Low <= hit_90_down & Low > hit_down & Close > hit_90_down)

MI_ever_hitup <- filter(tadawul_rawmi, High >= hit_up)
MI_ever_hitup_NC <- filter(tadawul_rawmi, High >= hit_up & Close < hit_up)

MI_ever_hit90up <- filter(tadawul_rawmi, High >= hit_90_up & High < hit_up)
MI_ever_hit90up_NC <- filter(tadawul_rawmi, High >= hit_90_up & Close < hit_90_up & High < hit_up)

### Normal ###
MS_ever_hitdown <- filter(tadawul_rawms, Low <= hit_down)
MS_ever_hitdown_NC <- filter(tadawul_rawms, Low <= hit_down & Close > hit_down)

MS_ever_hit90down <- filter(tadawul_rawms, Low <= hit_90_down & Low > hit_down)
MS_ever_hit90down_NC <- filter(tadawul_rawms, Low <= hit_90_down & Low > hit_down & Close > hit_90_down)

MS_ever_hitup <- filter(tadawul_rawms, High >= hit_up)
MS_ever_hitup_NC <- filter(tadawul_rawms, High >= hit_up & Close < hit_up)

MS_ever_hit90up <- filter(tadawul_rawms, High >= hit_90_up & High < hit_up)
MS_ever_hit90up_NC <- filter(tadawul_rawms, High >= hit_90_up & Close < hit_90_up & High < hit_up)


### Histograms ###
### down ###
p1 <- ggplot() + aes(Per_Chn) + geom_histogram(bins = 23, data = ever_hitdown, colour="black", fill="white", alpha = 1) + 
     geom_histogram(bins = 23, data = ever_hitdown_NC, colour="black", fill = "black", alpha = 0.75) +
     labs(title = "Returns Reached Limit Down", x = "Returns (black: close > limit)") + coord_cartesian(ylim=c(0,900))

p2 <- ggplot() + aes(Per_Chn) + geom_histogram(bins = 23, data = ever_hit90down, colour="black", fill="white", alpha = 1) + 
     geom_histogram(bins = 23, data = ever_hit90down_NC, colour="black", fill = "black", alpha = 0.75) +
     labs(title = "Returns Reached 9% Down", x = "Returns (black: close > -9%)") + coord_cartesian(ylim=c(0,900))

### UP ###
p3 <- ggplot() + aes(Per_Chn) + geom_histogram(bins = 23, data = ever_hitup, colour="black", fill="white", alpha = 1) + 
     geom_histogram(bins = 23, data = ever_hitup_NC, colour="black", fill = "black", alpha = 0.75) +
     labs(title = "Returns Reached Limit Up", x = "Returns (black: close < limit)") + coord_cartesian(ylim=c(0,900))

p4 <- ggplot() + aes(Per_Chn) + geom_histogram(bins = 23, data = ever_hit90up, colour="black", fill="white", alpha = 1) + 
     geom_histogram(bins = 23, data = ever_hit90up_NC, colour="black", fill = "black", alpha = 0.75) +
     labs(title = "Returns Reached 9% Up", x = "Returns (black: close < 9%)") + coord_cartesian(ylim=c(0,900))

### Combine plots (!!!run the function at the end of this code first!!!)
multiplot(p1, p2, p3, p4, cols=2)

### Do Test Statistics ###
### full period ###
### Down ###
set.seed(4321)
HT_TOTAL <- vector(mode = "numeric", length = 10000)
for (i in 1:10000){
     sample_LD <- ever_hitdown[sample(nrow(ever_hitdown),size=NROW(ever_hitdown),replace=TRUE),]
     sample_9D <- ever_hit90down[sample(nrow(ever_hit90down),size=NROW(ever_hit90down),replace=TRUE),]
     pooled_T <- (sum(sample_9D$Close <= sample_9D$hit_90_down) + sum(sample_LD$Close <= sample_LD$hit_down)) / (NROW(sample_9D) + NROW(sample_LD))
     se_pooled_T <- sqrt((pooled_T*(1-pooled_T)/NROW(sample_LD))+(pooled_T*(1-pooled_T)/NROW(sample_9D)))
     HT_TOTAL[i] <- (sum(sample_LD$Close <= sample_LD$hit_down)/NROW(sample_LD) - sum(sample_9D$Close <= sample_9D$hit_90_down)/NROW(sample_9D))/se_pooled_T
}
ggplot() + aes(HT_TOTAL)+ geom_histogram(bins = 50, colour="black", fill="white")

round(sum(HT_TOTAL < 1.645)/NROW(HT_TOTAL),2)

### Up ###
set.seed(4321)
HT_TOTAL_UP <- vector(mode = "numeric", length = 10000)
for (i in 1:10000){
     sample_LD_UP <- ever_hitup[sample(nrow(ever_hitup),size=NROW(ever_hitup),replace=TRUE),]
     sample_9D_UP <- ever_hit90up[sample(nrow(ever_hit90up),size=NROW(ever_hit90up),replace=TRUE),]
     pooled_T_UP <- (sum(sample_9D_UP$close >= sample_9D_UP$hit_90_up) + sum(sample_LD_UP$Close >= sample_LD_UP$hit_up)) / (NROW(sample_9D_UP) + NROW(sample_LD_UP))
     se_pooled_T_UP <- sqrt((pooled_T_UP*(1-pooled_T_UP)/NROW(sample_LD_UP))+(pooled_T_UP*(1-pooled_T_UP)/NROW(sample_9D_UP)))
     HT_TOTAL_UP[i] <- (sum(sample_LD_UP$Close >= sample_LD_UP$hit_up)/NROW(sample_LD_UP) - sum(sample_9D_UP$Close >= sample_9D_UP$hit_90_up)/NROW(sample_9D_UP))/se_pooled_T_UP
}
ggplot() + aes(HT_TOTAL_UP)+ geom_histogram(bins = 50, colour="black", fill="white")

round(sum(HT_TOTAL_UP < 1.645)/NROW(HT_TOTAL_UP),2)


### upnormal period ###
set.seed(4321)
HT_MI <- vector(mode = "numeric", length = 10000)
for (i in 1:10000){
     sample_LD_MI <- MI_ever_hitdown[sample(nrow(MI_ever_hitdown),size=NROW(MI_ever_hitdown),replace=TRUE),]
     sample_9D_MI <- MI_ever_hit90down[sample(nrow(MI_ever_hit90down),size=NROW(MI_ever_hit90down),replace=TRUE),]
     pooled_MI <- (sum(sample_9D_MI$Close <= sample_9D_MI$hit_90_down) + sum(sample_LD_MI$Close <= sample_LD_MI$hit_down)) / (NROW(sample_9D_MI) + NROW(sample_LD_MI))
     se_pooled_MI <- sqrt((pooled_MI*(1-pooled_MI)/NROW(sample_LD_MI))+(pooled_MI*(1-pooled_MI)/NROW(sample_9D_MI)))
     HT_MI[i] <- (sum(sample_LD_MI$Close <= sample_LD_MI$hit_down)/NROW(sample_LD_MI) - sum(sample_9D_MI$Close <= sample_9D_MI$hit_90_down)/NROW(sample_9D_MI))/se_pooled_MI
}
ggplot() + aes(HT_MI)+ geom_histogram(bins = 50, colour="black", fill="white")

round(sum(HT_MI < 1.645)/NROW(HT_MI),2)

### up ###
set.seed(4321)
HT_MI_UP <- vector(mode = "numeric", length = 10000)
for (i in 1:10000){
     sample_LD_MI_UP <- MI_ever_hitup[sample(nrow(MI_ever_hitup),size=NROW(MI_ever_hitup),replace=TRUE),]
     sample_9D_MI_UP <- MI_ever_hit90up[sample(nrow(MI_ever_hit90up),size=NROW(MI_ever_hit90up),replace=TRUE),]
     pooled_MI_UP <- (sum(sample_9D_MI_UP$Close >= sample_9D_MI_UP$hit_90_up) + sum(sample_LD_MI_UP$Close >= sample_LD_MI_UP$hit_up)) / (NROW(sample_9D_MI_UP) + NROW(sample_LD_MI_UP))
     se_pooled_MI_UP <- sqrt((pooled_MI_UP*(1-pooled_MI_UP)/NROW(sample_LD_MI_UP))+(pooled_MI_UP*(1-pooled_MI_UP)/NROW(sample_9D_MI_UP)))
     HT_MI_UP[i] <- (sum(sample_LD_MI_UP$Close >= sample_LD_MI_UP$hit_up)/NROW(sample_LD_MI_UP) - sum(sample_9D_MI_UP$Close >= sample_9D_MI_UP$hit_90_up)/NROW(sample_9D_MI_UP))/se_pooled_MI_UP
}
ggplot() + aes(HT_MI_UP)+ geom_histogram(bins = 50, colour="black", fill="white")

round(sum(HT_MI_UP < 1.645)/NROW(HT_MI_UP),2)

### normal vs abnormal ###
### down ###
set.seed(4321)
HT_MIMS <- vector(mode = "numeric", length = 10000)
for (i in 1:10000){
     sample_LD_MI <- MI_ever_hitdown[sample(nrow(MI_ever_hitdown),size=NROW(MI_ever_hitdown),replace=TRUE),]
     sample_LD_MS <- MS_ever_hitdown[sample(nrow(MS_ever_hitdown),size=NROW(MS_ever_hitdown),replace=TRUE),]
     pooled_MIMS <- (sum(sample_LD_MS$Close <= sample_LD_MS$hit_down) + sum(sample_LD_MI$Close <= sample_LD_MI$hit_down)) / (NROW(sample_LD_MS) + NROW(sample_LD_MI))
     se_pooled_MIMS <- sqrt((pooled_MIMS*(1-pooled_MIMS)/NROW(sample_LD_MI))+(pooled_MIMS*(1-pooled_MIMS)/NROW(sample_LD_MS)))
     HT_MIMS[i] <- (sum(sample_LD_MI$Close <= sample_LD_MI$hit_down)/NROW(sample_LD_MI) - sum(sample_LD_MS$Close <= sample_LD_MS$hit_down)/NROW(sample_LD_MS))/se_pooled_MIMS
}
ggplot() + aes(HT_MIMS)+ geom_histogram(bins = 50, colour="black", fill="white")

round(sum(HT_MIMS < 1.645)/NROW(HT_MIMS),2)

### up ###
set.seed(4321)
HT_MIMS_UP <- vector(mode = "numeric", length = 10000)
for (i in 1:10000){
     sample_LD_MI_UP <- MI_ever_hitup[sample(nrow(MI_ever_hitup),size=NROW(MI_ever_hitup),replace=TRUE),]
     sample_LD_MS_UP <- MS_ever_hitup[sample(nrow(MS_ever_hitup),size=NROW(MS_ever_hitup),replace=TRUE),]
     pooled_MIMS_UP <- (sum(sample_LD_MS_UP$Close >= sample_LD_MS_UP$hit_up) + sum(sample_LD_MI_UP$Close >= sample_LD_MI_UP$hit_up)) / (NROW(sample_LD_MS_UP) + NROW(sample_LD_MI_UP))
     se_pooled_MIMS_UP <- sqrt((pooled_MIMS_UP*(1-pooled_MIMS_UP)/NROW(sample_LD_MI_UP))+(pooled_MIMS_UP*(1-pooled_MIMS_UP)/NROW(sample_LD_MS_UP)))
     HT_MIMS_UP[i] <- (sum(sample_LD_MI_UP$Close >= sample_LD_MI_UP$hit_up)/NROW(sample_LD_MI_UP) - sum(sample_LD_MS_UP$Close >= sample_LD_MS_UP$hit_up)/NROW(sample_LD_MS_UP))/se_pooled_MIMS_UP
}
ggplot() + aes(HT_MIMS_UP)+ geom_histogram(bins = 50, colour="black", fill="white")

round(sum(HT_MIMS_UP < 1.645)/NROW(HT_MIMS_UP),2)

########################################################################################
################################ IPO initail return ####################################
########################################################################################

### import and clean data ###
ipo_data <- read_excel("~/R/ecmt678/IPO_Data.xlsx", sheet = 1, col_names = TRUE, col_types = NULL, na = "", skip = 0)
before_limit <- ipo_data[ipo_data[,1] < "2013-05-26" ,]
after_limit <- ipo_data[ipo_data[,1] >= "2013-05-26" ,]

### plot some graphs and calculate some statistics ###
ggplot() + aes(return_no_log) + geom_histogram(bins = 40, data = before_limit, colour="black", fill="white", alpha = 1) + 
     geom_histogram(bins = 40, data = after_limit, colour="black", fill = "black", alpha = 0.75) +
     labs(title = "Initial Returns Before and After Imposing Price Limit", x = "Returns")

ggplot() + aes(ret_diff) + geom_histogram(bins = 40, data = before_limit, colour="black", fill="white", alpha = 1) + 
     geom_histogram(bins = 40, data = after_limit, colour="black", fill = "black", alpha = 0.75) +
     labs(title = "Difference between high and close", x = "Returns")

describe(ipo_data$return_no_log)
describe(before_limit$return_no_log)
describe(after_limit$return_no_log)

### wilcox test ###
wilcox.test(before_limit$return_no_log, after_limit$return_no_log)



#############################################################
### This function can be used to combine plots            ###
### It was copied from "R Cookbook" (wounderful book!)    ###
#############################################################



# Multiple plot function
#
# ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects)
# - cols:   Number of columns in layout
# - layout: A matrix specifying the layout. If present, 'cols' is ignored.
#
# If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
# then plot 1 will go in the upper left, 2 will go in the upper right, and
# 3 will go all the way across the bottom.
#
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
     library(grid)
     
     # Make a list from the ... arguments and plotlist
     plots <- c(list(...), plotlist)
     
     numPlots = length(plots)
     
     # If layout is NULL, then use 'cols' to determine layout
     if (is.null(layout)) {
          # Make the panel
          # ncol: Number of columns of plots
          # nrow: Number of rows needed, calculated from # of cols
          layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                           ncol = cols, nrow = ceiling(numPlots/cols))
     }
     
     if (numPlots==1) {
          print(plots[[1]])
          
     } else {
          # Set up the page
          grid.newpage()
          pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
          
          # Make each plot, in the correct location
          for (i in 1:numPlots) {
               # Get the i,j matrix positions of the regions that contain this subplot
               matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
               
               print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                               layout.pos.col = matchidx$col))
          }
     }
}