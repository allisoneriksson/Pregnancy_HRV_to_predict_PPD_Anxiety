

#####packages to install 

install.packages("readxl")      ###read excel data 
install.packages("CGPfunctions") ###### graphs 
install.packages("lsr")         #########cramers 
install.packages("dplyr")     ####subsetting


#########packages needed to run analysis, need to be activated
library(CGPfunctions)   
library(lsr)           
library(dplyr)          
library(readxl)
library(foreign)
##########################################################

##Chidata<- read.spss("/Users/aller405/Desktop/BASIC_updated_22nov_na.rm.sav")  

###Chi data dep ### 
Chidata_dep<- df_11_12epds
View(Chidata_dep)        #### look at our data 
attach(Chidata_dep)      #### R can search our data and find column titles 

###Chi data anxiety ### 
Chidata_anx<- df_ppbeck
View(Chidata_anx)        #### look at our data 
attach(Chidata_anx)      #### R can search our data and find column titles 
#####
is.na(df_11_12epds$ppv6_EPDS_10_11R)
outcome_dep<-(df_11_12epds$ppv6_EPDS_10_11R)

is.na(ppv6beck)
outcome_beck <- ppv6beck
is.na(df_11_12epds$ppv6_EPDS_10_11R)
outcome_dep<-(df_11_12epds$ppv6_EPDS_10_11R)
#########################################################

POB <- as.numeric(Chidata_dep$v17_fodelseort_R)
Chidata_dep$POB <- POB
Chidata_POB <- Chidata_dep[!is.na(POB),]
attach((Chidata_POB))

###DEP/POB 
chi2_dep_POB <-table(POB,Chidata_dep$ppv6_EPDS_10_11R)  ###create crosstab 
ftable(chi2_dep_POB)               ###show table
summary(chi2_dep_POB)              ###run chi squared 
cramersV(chi2_dep_POB)             ###produce cramers V -requires the lsr package out of your library
prop.table(chi2_dep_POB, 1)       ###as proportions 

chi2_dep_POB <-table(POB,ppv6_EPDS_10_11R)  ###create crosstab 
ftable(chi2_dep_POB)               ###show table
summary(chi2_dep_POB)              ###run chi squared 
cramersV(chi2_dep_POB)             ###produce cramers V 

###ANX/POB 
ppv6beck <- as.numeric(Chidata_anx$ppv6_beck_score_R_01)
Chidata_anx$ppv6beck <- ppv6beck
POB_anx <- as.numeric(Chidata_anx$v17_fodelseort_R)
Chidata_anx$POB_anx <- POB_anx
Chidata_POB_anx <- Chidata_anx[!is.na(POB_anx),]
attach((Chidata_POB_anx))

chi2_anx_POB <-table(POB_anx,ppv6beck)  ###create crosstab 
ftable(chi2_anx_POB)               ###show table
summary(chi2_anx_POB)              ###run chi squared 
cramersV(chi2_anx_POB)             ###produce cramers V 
prop.table(chi2_anx_POB, 2)       ###as proportions 



### EDU and DEP### 

YNedu <- as.numeric(as.numeric(Chidata_dep$v17_utbildning_R_01))
Chidata_dep$YNedu <- YNedu
EDU_epds_df <- Chidata_dep[!is.na(Chidata_dep$YNedu),]
EDU_epds <- EDU_epds_df$YNedu


chi2_dep_EDU <-table(EDU_epds,EDU_epds_df$ppv6_EPDS_10_11R) 
ftable(chi2_dep_EDU)               ###show table
summary(chi2_dep_EDU)              ###run chi squared 
cramersV(chi2_dep_EDU)             ###produce cramers V 
prop.table(chi2_dep_EDU, 2)       ###as proportions 



###EDU and ANX###

YNedu_anx <- as.numeric(as.numeric(Chidata_anx$v17_utbildning_R_01))
Chidata_anx$YNedu_anx <- YNedu_anx
EDU_beck_df <- Chidata_anx[!is.na(Chidata_anx$YNedu_anx),]
EDU_beck <- EDU_beck_df$YNedu_anx


chi2_anx_EDU <-table(EDU_beck,EDU_beck_df$ppv6_beck_score_R_01)  
ftable(chi2_anx_EDU)               ###show table
summary(chi2_anx_EDU)              ###run chi squared 
cramersV(chi2_anx_EDU)             ###produce cramers V 
prop.table(chi2_anx_EDU, 2)       ###as proportions 







YNedu_anx <- as.numeric(as.numeric(Chidata_anx$v17_utbildning_R_01))
Chidata_anx$YNedu_anx <- YNedu_anx
Chidata_anx_EDU <- Chidata_anx[!is.na(Chidata_anx$YNedu_anx),]
attach(Chidata_anx_EDU)


chi2_anx_EDU  <-table(YNedu_anx,ppv6_beck_score_R_01)  
ftable(chi2_anx_EDU )               ###show table
summary(chi2_anx_EDU )              ###run chi squared 
cramersV(chi2_anx_EDU )             ###produce cramers V
prop.table(chi2_anx_EDU , 2)       ###as proportions 

#### Sleep and dep###

EPDS <- df_epds$ppv6_EPDS_10_11R

chi2_dep_sleep  <-table(SLEEP_epds,EPDS) 
ftable(chi2_dep_sleep)               ###show table
summary(chi2_dep_sleep)              ###run chi squared 
cramersV(chi2_dep_sleep )             ###produce cramers V 
prop.table(chi2_dep_sleep, 2)       ###as proportions 


#### Sleep and dep using complete dataset as comparison ###

EPDS <- df_short_complete$ppv6EPDS
length(df_short_complete$SLEEP)
chi2_dep_sleep  <-table(df_short_complete$SLEEP,df_short_complete$ppEPDS)  
ftable(chi2_dep_sleep)               ###show table
summary(chi2_dep_sleep)              ###run chi squared 
cramersV(chi2_dep_sleep )             ###produce cramers V 
prop.table(chi2_dep_sleep, 2)       ###as proportions 

#### Sleep and anx###
BECK <- as.numeric(df_beck$ppv6_beck_score_R_01)

chi2_anx_sleep  <-table(SLEEP_beck,BECK)  
ftable(chi2_anx_sleep)               ###show table
summary(chi2_anx_sleep)              ###run chi squared 
cramersV(chi2_anx_sleep)             ###produce cramers V 
prop.table(chi2_anx_sleep, 2)       ###as proportions 


####AGE + ANX####
AGE_beck <- df_beck$B_alder_groups

chi2_anx_age  <-table(AGE_beck,BECK) 
ftable(chi2_anx_age)               ###show table
summary(chi2_anx_age)              ###run chi squared 
cramersV(chi2_anx_age)             ###produce cramers V 
prop_2 <- prop.table(chi2_anx_age, 2)       ###as proportions 
View(round(prop_2,2))


####AGE + DEP####
AGE_epds <- df_epds$B_alder_groups

chi2_dep_age  <-table(AGE_epds,EPDS)  
ftable(chi2_dep_age)               ###show table
summary(chi2_dep_age)              ###run chi squared 
cramersV(chi2_dep_age)             ###produce cramers V
prop.table(chi2_dep_age, 2)       ###as proportions 

###Parity + DEP###
install.packages("generalCorr")
library("generalCorr")
PAR_epds <- as.numeric(df_epds$NK_parity_D)



chi2_dep_PAR <- table(napair(PAR_epds,EPDS))  
View(chi2_dep_PAR)
ftable(chi2_dep_PAR)               ###show table
summary(chi2_dep_PAR)              ###run chi squared 
cramersV(chi2_dep_PAR)             ###produce cramers V -
prop.table(chi2_dep_PAR, 2)       ###as proportions 

###Parity + ANX###
PAR_beck <- as.numeric(df_beck$NK_parity_D)

chi2_anx_PAR <- table(napair(PAR_beck,BECK))  
View(chi2_anx_PAR)
ftable(chi2_anx_PAR)               ###show table
summary(chi2_anx_PAR)              ###run chi squared 
cramersV(chi2_anx_PAR)             ###produce cramers V 
prop.table(chi2_anx_PAR, 2)       ###as proportions 


####Illness + Dep####
View(df_epds$Illness_combined)
Ill_epds <- (df_epds$Illness_combined)

chi2_dep_Ill <- table(napair(Ill_epds,EPDS))  ###create crosstab 
ftable(chi2_dep_Ill)               ###show table
summary(chi2_dep_Ill)              ###run chi squared 
cramersV(chi2_dep_Ill)             ###produce cramers V 
prop.table(chi2_dep_Ill, 2)       ###as proportions 

####Illness + Anx####

Ill_beck <- (df_beck$Illness_combined)

chi2_anx_Ill <- table(napair(Ill_beck,BECK))  ###create crosstab 
ftable(chi2_anx_Ill)               ###show table
summary(chi2_anx_Ill)              ###run chi squared 
cramersV(chi2_anx_Ill)             ###produce cramers V 
prop.table(chi2_anx_Ill, 2)       ###as proportions 


#### DEPH + ePDS####

DEP_epds <- as.numeric(df_epds$NK_depression_history)

chi2_dep_DEP <- table(napair(DEP_epds,EPDS))  ###create crosstab 
ftable(chi2_dep_DEP)               ###show table
summary(chi2_dep_DEP)              ###run chi squared 
cramersV(chi2_dep_DEP)             ###produce cramers V 
prop.table(chi2_dep_DEP, 2)       ###as proportions 

####DEP + BECK####

DEP_beck <- as.numeric(df_beck$Illness_combined)

chi2_anx_DEP <- table(napair(DEP_beck,BECK))  ###create crosstab 
ftable(chi2_anx_DEP)               ###show table
summary(chi2_anx_DEP)              ###run chi squared 
cramersV(chi2_anx_DEP)             ###produce cramers V 
prop.table(chi2_anx_DEP, 2)       ###as proportions 


#### FOD + ePDS####
View(df_epds$NK_delivery_fear_2 )
FOD_epds <- (df_epds$NK_delivery_fear_2 )
FOD_epds <- as.numeric(df_epds$NK_delivery_fear_2 )

chi2_dep_FOD <- table(FOD_epds,EPDS)  ###create crosstab 
ftable(chi2_dep_FOD)               ###show table
summary(chi2_dep_FOD)              ###run chi squared 
cramersV(chi2_dep_FOD)             ###produce cramers V 
prop.table(chi2_dep_FOD, 2)       ###as proportions 

####FOD + BECK####

FOD_beck <-  as.numeric(df_beck$NK_delivery_fear_2 )

chi2_anx_FOD <- table(FOD_beck,BECK)  ###create crosstab 
ftable(chi2_anx_FOD)               ###show table
summary(chi2_anx_FOD)              ###run chi squared 
cramersV(chi2_anx_FOD)             ###produce cramers V 
prop.table(chi2_anx_FOD, 2)       ###as proportions 

#### SSRI + ePDS####
SSRI_epds <-  as.numeric(df_epds$NK_SSRI_pregnancy_loose)

chi2_dep_SSRI <- table(SSRI_epds,EPDS)  ###create crosstab 
ftable(chi2_dep_SSRI)               ###show table
summary(chi2_dep_SSRI)              ###run chi squared 
cramersV(chi2_dep_SSRI)             ###produce cramers V
prop.table(chi2_dep_SSRI, 2)       ###as proportions 

####SSRI + BECK####
View(df_beck$NK_SSRI_pregnancy_loose)
SSRI_beck <-  as.numeric(df_beck$NK_SSRI_pregnancy_loose)

chi2_anx_SSRI <- table(SSRI_beck,BECK)  ###create crosstab 
ftable(chi2_anx_SSRI)               ###show table
summary(chi2_anx_SSRI)              ###run chi squared 
cramersV(chi2_anx_SSRI)             ###produce cramers V 
prop.table(chi2_anx_SSRI, 2)       ###as proportions 

#### RS14 + EPDS####
View(df_epds$v32_RS_14_R)
# Recode data
# Convert the column to a factor
df_epds$RS14_D_R <- factor(df_epds$RS14_D_R)
df_epds$RS14_D_R[df_epds$v32_RS_14_SCORE_R =="Very low"] <- "0"
df_epds$RS14_D_R[df_epds$v32_RS_14_SCORE_R =="Low"] <- "0"
df_epds$RS14_D_R[df_epds$v32_RS_14_SCORE_R =="On the low end"] <- "0"
df_epds$RS14_D_R[df_epds$v32_RS_14_SCORE_R =="On the low end"] <- "0"
df_epds$RS14_D_R[df_epds$v32_RS_14_SCORE_R =="Moderate"] <- "1"
df_epds$RS14_D_R[df_epds$v32_RS_14_SCORE_R =="Moderately high"] <- "1"
df_epds$RS14_D_R[df_epds$v32_RS_14_SCORE_R =="High"] <- "1"


View(df_epds$RS14_D_R)
RS14_epds <- (df_epds$RS14_D_R)
chi2_dep_RS14 <- table(RS14_epds,EPDS)  ###create crosstab 
ftable(chi2_dep_RS14)               ###show table
summary(chi2_dep_RS14)              ###run chi squared 
cramersV(chi2_dep_RS14)             ###produce cramers V 
prop.table(chi2_dep_RS14, 2)       ###as proportions 

####RS14 + BECK####

View(df_beck$v32_RS_14_SCORE_R)
View(df_beck$RS14_D_R)
# Recode data
# Convert the column to a factor
df_beck$RS14_D_R <- factor(na.omit(df_beck$v32_RS_14_SCORE_R))
df_beck$RS14_D_R[df_beck$v32_RS_14_SCORE_R =="Very low"] <- "0"
df_beck$RS14_D_R[df_beck$v32_RS_14_SCORE_R =="Low"] <- "0"
df_beck$RS14_D_R[df_beck$v32_RS_14_SCORE_R =="On the low end"] <- "0"
df_beck$RS14_D_R[df_beck$v32_RS_14_SCORE_R =="Moderate"] <- "1"
df_beck$RS14_D_R[df_beck$v32_RS_14_SCORE_R =="Moderately high"] <- "1"
df_beck$RS14_D_R[df_beck$v32_RS_14_SCORE_R =="High"] <- "1"


View(df_beck$RS14_D_R)
RS14_beck<- (df_beck$RS14_D_R)
chi2_anx_RS14 <- table(RS14_beck,BECK)  ###create crosstab 
ftable(chi2_anx_RS14)               ###show table
summary(chi2_anx_RS14)              ###run chi squared 
cramersV(chi2_anx_RS14)             ###produce cramers V 
prop.table(chi2_anx_RS14, 2)       ###as proportions 


#### BECK + SSRI 
View(df_beck$NK_SSRI_pregnancy_loose)
SSRI_beck <-  as.numeric(df_beck$NK_SSRI_pregnancy_loose)

chi2_anx_SSRI <- table(SSRI_beck,BECK)  ###create crosstab 
ftable(chi2_anx_SSRI)               ###show table
summary(chi2_anx_SSRI)              ###run chi squared 
cramersV(chi2_anx_SSRI)             ###produce cramers V 
prop.table(chi2_anx_SSRI, 2)       ###as proportions 

#### DEP32 + ePDS####
View(df_epds$v32_EPDS_D_R)

DEP32_epds <- as.numeric(df_epds$v32_EPDS_D_R )

chi2_dep_DEP32 <- table(DEP32_epds,EPDS)  ###create crosstab 
ftable(chi2_dep_DEP32)               ###show table
summary(chi2_dep_DEP32)              ###run chi squared 
cramersV(chi2_dep_DEP32)             ###produce cramers V 
prop.table(chi2_dep_DEP32, 2)       ###as proportions 


#### DEP32 10/11 cutoff + EPDS####

  
df_epds$DEP32_EPDS10_11 <-ifelse(df_epds$EPDS32sum >10, 1, 0)

DEP32_epds_10_11 <- as.numeric(df_epds$DEP32_EPDS10_11)

chi2_dep_DEP32_10_11 <- table(DEP32_epds_10_11,EPDS)  ###create crosstab 
ftable(chi2_dep_DEP32_10_11)               ###show table
summary(chi2_dep_DEP32_10_11)              ###run chi squared 
cramersV(chi2_dep_DEP32_10_11)             ###produce cramers V 
prop.table(chi2_dep_DEP32_10_11, 2)       ###as proportions 



###DEP32 + BECK####

DEP32_beck <- as.numeric(df_beck$v32_EPDS_D_R)

chi2_dep_DEP32 <- table(napair(DEP32_beck,BECK))  ###create crosstab 
ftable(chi2_dep_DEP32)               ###show table
summary(chi2_dep_DEP32)              ###run chi squared 
cramersV(chi2_dep_DEP32)             ###produce cramers V 
prop.table(chi2_dep_DEP32, 2)       ###as proportions 


chi2_dep_POB <-table(POB,ppv6_EPDS_10_11R)  ###create crosstab 
ftable(chi2_dep_POB)               ###show table
summary(chi2_dep_POB)              ###run chi squared 
cramersV(chi2_dep_POB)             ###produce cramers V 



###DEP32 + BECK####
df_beck$v32_EPDS_R

ifelse(df_beck$v32_EPDS_R >10, 1, 0)

df_beck$DEP32_beck_EPDS10_11 <-ifelse(df_beck$v32_EPDS_R >10, 1, 0)

DEP32_beck_10_11 <- as.numeric(df_beck$DEP32_beck_EPDS10_11)

chi2_anx_DEP32_10_11 <- table(DEP32_beck_10_11,BECK)  ###create crosstab 
ftable(chi2_anx_DEP32_10_11)               ###show table
summary(chi2_anx_DEP32_10_11)              ###run chi squared 
cramersV(chi2_anx_DEP32_10_11)             ###produce cramers V 
prop.table(chi2_anx_DEP32_10_11, 2)       ###as proportions 

mean(df_epds$EPDS32sum)
sd(df_epds$EPDS32sum)
mean(df_beck$v32_EPDS_R)
sd(df_beck$v32_EPDS_R)

###BECK32 + BECK####

BECK32_beck <- as.numeric(df_beck$v32_beck_score_R_01)

chi2_anx_ANX32 <- table(napair(BECK32_beck,BECK))  ###create crosstab 
ftable(chi2_anx_ANX32)               ###show table
summary(chi2_anx_ANX32)              ###run chi squared 
cramersV(chi2_anx_ANX32)             ###produce cramers V 
prop.table(chi2_anx_ANX32, 2)       ###as proportions 


chi2_dep_POB <-table(POB,ppv6_EPDS_10_11R)  ###create crosstab 
ftable(chi2_dep_POB)               ###show table
summary(chi2_dep_POB)              ###run chi squared 
cramersV(chi2_dep_POB)             ###produce cramers V 



###RS14 + DEP####

RS14_epds <- as.numeric(df_epds$RS14_D_R)

chi2_dep_DEP32 <- table(napair(BECK32_beck,BECK))  ###create crosstab 
ftable(chi2_dep_DEP32)               ###show table
summary(chi2_dep_DEP32)              ###run chi squared 
cramersV(chi2_dep_DEP32)             ###produce cramers V 
prop.table(chi2_dep_DEP32, 2)       ###as proportions 


chi2_dep_POB <-table(POB,ppv6_EPDS_10_11R)  ###create crosstab 
ftable(chi2_dep_RS14)               ###show table
summary(chi2_dep_RS14)              ###run chi squared 
cramersV(chi2_dep_RS14)             ###produce cramers V 

###RS14 + BECK####

BECK32_beck <- as.numeric(df_beck$v32_beck_score_R_01)

chi2_dep_DEP32 <- table(napair(BECK32_beck,BECK))  ###create crosstab 
ftable(chi2_dep_DEP32)               ###show table
summary(chi2_dep_DEP32)              ###run chi squared 
cramersV(chi2_dep_DEP32)             ###produce cramers V 
prop.table(chi2_dep_DEP32, 2)       ###as proportions 


chi2_dep_POB <-table(POB,ppv6_EPDS_10_11R)  ###create crosstab 
ftable(chi2_dep_POB)               ###show table
summary(chi2_dep_POB)              ###run chi squared 
cramersV(chi2_dep_POB)             ###produce cramers V 


### T1 BMI
BMI_epds_t.test <- t.test(v32_BMI ~ ppv6_EPDS_10_11R, data = df_epds)
BMI_beck_t.test <- t.test(v32_BMI ~ ppv6_beck_score_R_01, data = df_beck)
df_epds$ppv6_beck_score_R_01

##T1 EPDS32 mean
df_healthy <- df2[!df2$v32_EPDS_D_R=='12-30',]

df_EPDS32_DEP <- df_epds[!df_epds$v32_EPDS_R== '11-30',]
as.numeric(df_epds$EPDS32sum)
df_epds$v32_EPDS_R
EPDS_EPDS32_10_11 <- (!EPDS32sum == '> 10')
mean(df_epds$EPDS32sum)
sd(df_epds$EPDS32sum)
mean(df_beck$v32_EPDS_R)
sd(df_beck$v32_EPDS_R)
EPDS32_epds_t.test <- t.test(df_epds$EPDS32sum ~ ppv6_EPDS_10_11R, data = df_epds)
EPDS32_beck_t.test <- t.test(df_beck$v32_EPDS_R ~ ppv6_beck_score_R_01, data = df_beck)
sd(EPDS32_beck_t.test)
mean(df_epds$EPDS32sum == '11:30')
