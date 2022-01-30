#Import the package that may be used in the code
library(lattice)
library(survival)
library(Formula)
library(ggplot2)
library(Hmisc)
library(readr)
library(writexl)

#Import the dataset that about diabets patients
diabetic_data <- read.csv("C:/Users/liyuping/Desktop/Homework/competition/dataset_diabetes/diabetic_data.csv",na.string ="?")

#Checking the information in the dataset
View(diabetic_data)

#create a dataframe out of the data
df_diab_data <- data.frame(diabetic_data)

# Create the function mode to fill the missing value
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}#closing the function

################# RACE #################
# mode of race: Caucasian. The missing values were replaced by the mode
#checking the value in race 
table(df_diab_data$race)
#checking the size of missing value
sum(is.na(df_diab_data$race))
#get the mode for the race
getmode(df_diab_data$race)
#fill in the missing value
df_diab_data$race[is.na(df_diab_data$race)] <- getmode(df_diab_data$race)
#check the value after filling the missing value for the race column
table(df_diab_data$race)

################# GENDER #################
# frequency table of gender: Female The missing values were replaced by the mode
table(df_diab_data$gender)
# to find total missing values ("?") for gender
sum(is.na(df_diab_data$gender))
# filling missing values with the mode
df_diab_data$gender[df_diab_data$gender == "Unknown/Invalid"] <- getmode(df_diab_data$gender)
# double checking the table
table(df_diab_data$gender)


################# WEIGHT #################
# mode for weight: ? 
# the rate of ? is 96% we don't have enough data to analyze.
table(diabetic_data$weight)
sum(is.na(df_diab_data$weight))
getmode(df_diab_data$weight)

################# PAYER_CODE #################
# mode for payer_code: ?. We replacing with 0 as unknown
# which is the second highest value. The difference between MC and ? was 2%
table(df_diab_data$payer_code)
df_diab_data$payer_code[ is.na(df_diab_data$payer_code) ] <- 0
table(df_diab_data$payer_code)
sum(is.na(df_diab_data$payer_code))

################# MEDICAL SPECIALTY #################
# mode for medical_specialty:?   will change the NA values for unknown. 
table(df_diab_data$medical_specialty)
sum(is.na(df_diab_data$medical_specialty))
df_diab_data$medical_specialty[is.na(df_diab_data$medical_specialty)]<- "Unknown"
getmode(df_diab_data$medical_specialty)

################# DIAGNOSIS 2 #################
# 358 missing diagnosis. We discovered that there are several diagnosis codes. 
# All the missing values will fill in a because it is unknown. 
# Research shows that diabetic related code are connected to 250 - 250.93
table(df_diab_data$diag_2)
sum(is.na(df_diab_data$diag_2))
df_diab_data$diag_2[is.na(df_diab_data$diag_2)]<- 0

################# DIAGNOSIS 3 #################
# 1423 missing diagnosis. We discovered that there are several diagnosis codes.
# All the missing values will fill in a because it is unknown. 
# Research shows that diabetic related code are connected to 250 - 250.93
table(df_diab_data$diag_3)
getmode(df_diab_data$diag_3)
df_diab_data$diag_3[is.na(df_diab_data$diag_3)]<- 0 
sum(is.na(df_diab_data$diag_3))


## Changing the character variables in to number or make bins for number variables ###

################# RACE #################      
# Categorize Caucasian as bin 1
df_diab_data$race_bin <- gsub("Caucasian","1",df_diab_data$race)
# Categorize African American as bin 2
df_diab_data$race_bin <- gsub("AfricanAmerican","2",df_diab_data$race_bin)
# Categorize The rest value as bin 3: The other
df_diab_data$race_bin <- gsub("Asian","3",df_diab_data$race_bin)
df_diab_data$race_bin <- gsub("Hispanic","3",df_diab_data$race_bin)
df_diab_data$race_bin <- gsub("Other","3",df_diab_data$race_bin)
# Change the value in new column as numeric variables
df_diab_data$race_bin <- as.numeric(df_diab_data$race_bin)
# Check the detail information for new column
table(df_diab_data$race_bin)
# Draw the histogram plot for the new column
hist(df_diab_data$race_bin)

################# Admission_Type #################
# check the original count values of the variable
table(df_diab_data$admission_type_id)    
# Categorize 1,2,7 as bin 1: required treatment in less than 12 hours
df_diab_data$admission_type_bin <- gsub("1","1",df_diab_data$admission_type_id)
df_diab_data$admission_type_bin <- gsub("2","1",df_diab_data$admission_type_bin)
df_diab_data$admission_type_bin <- gsub("7","1",df_diab_data$admission_type_bin)
# Categorize 3 as bin 2: don't need treatment in less than 12 hours
df_diab_data$admission_type_bin <- gsub("3","2",df_diab_data$admission_type_bin)
# Categorize 4 as bin 3: related to new born
df_diab_data$admission_type_bin <- gsub("4","3",df_diab_data$admission_type_bin)
# Categorize 4 as bin 3: lack of information for their treatment
df_diab_data$admission_type_bin <- gsub("5","4",df_diab_data$admission_type_bin)
df_diab_data$admission_type_bin <- gsub("6","4",df_diab_data$admission_type_bin)
df_diab_data$admission_type_bin <- gsub("8","4",df_diab_data$admission_type_bin)
# Change the value in new column as numeric variables
df_diab_data$admission_type_bin <- as.numeric(df_diab_data$admission_type_bin)
# Check the detail information for new column
table(df_diab_data$admission_type_bin)
# Draw the histogram plot for the new column
hist(df_diab_data$admission_type_bin)

################# Discharge_Disposition_Id #################
# Check the information
table(df_diab_data$discharge_disposition_id)
# Create the new column for new bins
df_diab_data$dis_dis_bin <-df_diab_data$discharge_disposition_id
# New bin 1 - Recover and only need daily care out of hospital             
for(i in 1:nrow(df_diab_data)){
  if(df_diab_data[i,8]== 1|df_diab_data[i,8]==3|df_diab_data[i,8]==4|
     df_diab_data[i,8]==6|df_diab_data[i,8]==8|df_diab_data[i,8]==24|
     df_diab_data[i,8]==27|df_diab_data[i,8]==30){
    df_diab_data[i,53]<- 1
# New bin 2 - People who still need to stay in the hospital    
  }else if(df_diab_data[i,8]== 2|df_diab_data[i,8]==5|df_diab_data[i,8]==9|
           df_diab_data[i,8]==10|df_diab_data[i,8]==12|df_diab_data[i,8]==15|
           df_diab_data[i,8]==16|df_diab_data[i,8]==17|df_diab_data[i,8]==22|
           df_diab_data[i,8]==23|df_diab_data[i,8]==28|df_diab_data[i,8]==29){
    df_diab_data[i,53] <-2
# New bin 3 - People who expired or near expired
  }else if(df_diab_data[i,8]==11|df_diab_data[i,8]==13|df_diab_data[i,8]==14|
           df_diab_data[i,8]==19|df_diab_data[i,8]==20|df_diab_data[i,8]==21){
    df_diab_data[i,53] <-3
# New bin 3 - Unknown patient or the patient did not follow the medical advice
  }else{
    df_diab_data[i,53] <-4
  }
} 
#change the new value in new column as numeric variables
df_diab_data$dis_dis_bin <- as.numeric(df_diab_data$dis_dis_bin)
#Check the value in new column
table(df_diab_data$dis_dis_bin)
#visualization the frequency of the bins 
hist(df_diab_data$dis_dis_bin)

################# Payer_Code #################
#Check the information for original payer_code
table(df_diab_data$payer_code)
#Create new col_bin 1 for "Unknown" Payer Code                  
df_diab_data$payer_code_bin <- gsub("0","1",df_diab_data$payer_code) 
#Create new col_bin 2 for "Other Insurance" Payer Code     
df_diab_data$payer_code_bin <- gsub("BC","2",df_diab_data$payer_code_bin) 
df_diab_data$payer_code_bin <- gsub("CH","2",df_diab_data$payer_code_bin)
df_diab_data$payer_code_bin <- gsub("CM","2",df_diab_data$payer_code_bin)
df_diab_data$payer_code_bin <- gsub("CP","2",df_diab_data$payer_code_bin)
df_diab_data$payer_code_bin <- gsub("DM","2",df_diab_data$payer_code_bin)
df_diab_data$payer_code_bin <- gsub("FR","2",df_diab_data$payer_code_bin)
df_diab_data$payer_code_bin <- gsub("HM","2",df_diab_data$payer_code_bin)
df_diab_data$payer_code_bin <- gsub("MD","2",df_diab_data$payer_code_bin) 
df_diab_data$payer_code_bin <- gsub("MP","2",df_diab_data$payer_code_bin)
df_diab_data$payer_code_bin <- gsub("OG","2",df_diab_data$payer_code_bin)
df_diab_data$payer_code_bin <- gsub("OT","2",df_diab_data$payer_code_bin)
df_diab_data$payer_code_bin <- gsub("PO","2",df_diab_data$payer_code_bin)
df_diab_data$payer_code_bin <- gsub("SI","2",df_diab_data$payer_code_bin)
df_diab_data$payer_code_bin <- gsub("UN","2",df_diab_data$payer_code_bin)
df_diab_data$payer_code_bin <- gsub("WC","2",df_diab_data$payer_code_bin)
#Create new col_bin 3 for "Medicare & Medicaid" Payer Code      
df_diab_data$payer_code_bin <- gsub("MC","3",df_diab_data$payer_code_bin) 
#Create new col_bin 4 for "Self-Pay" Payer Code                 
df_diab_data$payer_code_bin <- gsub("SP","4",df_diab_data$payer_code_bin) #Self-Pay
#change the new value in new column as numeric variables
df_diab_data$payer_code_bin <- as.numeric(df_diab_data$payer_code_bin)
#Check the value in new column
table(df_diab_data$payer_code_bin)
#visualization the frequency of the new bins
hist(df_diab_data$payer_code_bin)

################# Age #################
#Check the information for original payer_code
table(df_diab_data$age)
#remove the "[" for age column to make sure the rest gsub code can work
df_diab_data$age <- gsub(pattern="\\[", replacement="", x= df_diab_data$age)
#creating bins by 0 to 30 to be considered young 
df_diab_data$age_bin <- gsub("0-10)","1",df_diab_data$age)
#remove the "[" for age_bin column to make sure the rest gsub code can work
df_diab_data$age_bin <- gsub(pattern="\\[", replacement="", x= df_diab_data$age_bin)
df_diab_data$age_bin <- gsub("10-20)","1",df_diab_data$age_bin)
df_diab_data$age_bin <- gsub("20-30)","1",df_diab_data$age_bin)
#creating bins by ages 30 to 60 to be considered middle aged
df_diab_data$age_bin <- gsub("30-40)","2",df_diab_data$age_bin)
df_diab_data$age_bin <- gsub("40-50)","2",df_diab_data$age_bin)
df_diab_data$age_bin <- gsub("50-60)","2",df_diab_data$age_bin)
#creating bins by combining the ages 60 to 100 to be considered old aged 
df_diab_data$age_bin <- gsub("60-70)","3",df_diab_data$age_bin)
df_diab_data$age_bin <- gsub("70-80)","3",df_diab_data$age_bin)
df_diab_data$age_bin <- gsub("80-90)","3",df_diab_data$age_bin)
df_diab_data$age_bin <- gsub("90-100)","3",df_diab_data$age_bin)

#change the new value in new column as numeric variables
df_diab_data$age_bin <- as.numeric(df_diab_data$age_bin)
#Check the value in new column
table(df_diab_data$age_bin)
#visualization the frequency of the age bins
hist(df_diab_data$age_bin)

################# Admission_Source #################
#Check the information - Admission_Source
table(df_diab_data$admission_source_id)
#Create a new column to create bins for Admission_Source
df_diab_data$admission_referral_bin <-df_diab_data$admission_source_id
#New bin 1 - Unknown             
for(i in 1:nrow(df_diab_data)){
  if(df_diab_data[i,9]== 9|df_diab_data[i,9]==15|df_diab_data[i,9]==16|
     df_diab_data[i,9]==19|df_diab_data[i,9]==20){
    df_diab_data[i,56]<- 1
#New bin 2 - new_born
  }else if(df_diab_data[i,9]== 11|df_diab_data[i,9]==12|df_diab_data[i,9]==13|
           df_diab_data[i,9]==14|df_diab_data[i,9]==22|df_diab_data[i,9]==23){
    df_diab_data[i,56] <-2
#New bin 3 - low risk medical institution
  }else if(df_diab_data[i,9]== 1|df_diab_data[i,9]==2|df_diab_data[i,9]==3|
           df_diab_data[i,9]==17|df_diab_data[i,9]==18){
    df_diab_data[i,56] <-3
#New bin 4 - high risk medical institution
  }else if(df_diab_data[i,9]== 4|df_diab_data[i,9]==7|df_diab_data[i,9]==10|
           df_diab_data[i,9]==21|df_diab_data[i,9]==24|df_diab_data[i,9]==25){
    df_diab_data[i,56] <-4
#New bin 5 - the other
  }else{
    df_diab_data[i,56] <-5
  } # closing the if statement
} #closing the for loop
#change the new value in new column as numeric variables
df_diab_data$admission_referral_bin <- as.numeric(df_diab_data$admission_referral_bin)
#Check the value in new column
table(df_diab_data$admission_referral_bin)
#visualization the frequency of new column
hist(df_diab_data$admission_referral_bin)

################# Medical_Specialty #################
#Check the information - medical_specialty
table(df_diab_data$medical_specialty)
#Create a new column to create bins for medical_specialty
df_diab_data$medical_specialty_bin <-df_diab_data$medical_specialty
#New bin 1 - Popular medical specialty             
for(i in 1:nrow(df_diab_data)){
  if(df_diab_data[i,12]== "Emergency/Trauma"|df_diab_data[i,12]== "Family/General Practice"|
     df_diab_data[i,12]== "Cardiology"|df_diab_data[i,12]== "InternalMedicine"|
     df_diab_data[i,12]== "Surgery-Cardiovascular"|df_diab_data[i,12]== "Surgery-Cardiovascular/Thoracic"|
     df_diab_data[i,12]== "Surgery-Colon&Rectal"|df_diab_data[i,12]== "Surgery-General"|
     df_diab_data[i,12]== "Surgery-Maxillofacial"|df_diab_data[i,12]== "Surgery-Neuro"|
     df_diab_data[i,12]== "Surgery-Pediatric"|df_diab_data[i,12]== "Surgery-Plastic"|
     df_diab_data[i,12]== "Surgery-PlasticwithinHeadandNeck"|df_diab_data[i,12]== "Surgery-Thoracic"|
     df_diab_data[i,12]== "Surgery-Vascular"|df_diab_data[i,12]== "SurgicalSpecialty"|
     df_diab_data[i,12]== "Surgeon"|df_diab_data[i,12]== "Pediatrics"|
     df_diab_data[i,12]== "Pediatrics-AllergyandImmunology"|df_diab_data[i,12]== "Pediatrics-CriticalCare"|
     df_diab_data[i,12]== "Pediatrics-EmergencyMedicine"|df_diab_data[i,12]== "Pediatrics-Endocrinology"|
     df_diab_data[i,12]== "Pediatrics-Hematology-Oncology"|df_diab_data[i,12]== "Pediatrics-InfectiousDiseases"|
     df_diab_data[i,12]== "Pediatrics-Neurology"|df_diab_data[i,12]== "Pediatrics-Pulmonology"|
     df_diab_data[i,12]== "Anesthesiology-Pediatric"|df_diab_data[i,12]== "Cardiology-Pediatric"|
     df_diab_data[i,12]== "Orthopedics-Reconstructive"|df_diab_data[i,12]== "Orthopedics"|
     df_diab_data[i,12]== "Radiologist"|df_diab_data[i,12]== "Radiology"){
    df_diab_data[i,57]<- 1
#New bin 3 - Unknown  
  }else if(df_diab_data[i,12]=="Unknown"){
    df_diab_data[i,57] <-3
#New bin 2 - Unpopular medical specialty
  }else{
    df_diab_data[i,57] <-2
  }
} 
#change the new value in new column as numeric variables
df_diab_data$medical_specialty_bin <- as.numeric(df_diab_data$medical_specialty_bin)
#Check the value in new column
table(df_diab_data$medical_specialty_bin)
#visualization the frequency of medical specialty
hist(df_diab_data$medical_specialty_bin)

##########  Changing medicines to numeric information  ##########
## 0 is for No taking the medicine                    
## 1 is for Down, is reducing the dose of the medicine
## 2 is for Steady, is taking the same dose of medicine
## 3 is for Up, is increasing the dose of the medicine

########## Metformin ##########
table(df_diab_data$metformin)
df_diab_data$metformin_bin <- gsub("No","0",df_diab_data$metformin)
df_diab_data$metformin_bin <- gsub("Down","1",df_diab_data$metformin_bin)
df_diab_data$metformin_bin <- gsub("Steady","2",df_diab_data$metformin_bin)
df_diab_data$metformin_bin <- gsub("Up","3",df_diab_data$metformin_bin)
table(df_diab_data$metformin_bin)
df_diab_data$metformin_bin <- as.numeric(df_diab_data$metformin_bin)
hist(df_diab_data$metformin_bin)

########## repaglinide ##########
table(df_diab_data$repaglinide)
df_diab_data$repaglinide_bin <- gsub("No","0",df_diab_data$repaglinide)
df_diab_data$repaglinide_bin <- gsub("Down","1",df_diab_data$repaglinide_bin)
df_diab_data$repaglinide_bin <- gsub("Steady","2",df_diab_data$repaglinide_bin)
df_diab_data$repaglinide_bin <- gsub("Up","3",df_diab_data$repaglinide_bin)
table(df_diab_data$repaglinide_bin)
df_diab_data$repaglinide_bin <- as.numeric(df_diab_data$repaglinide_bin)
hist(df_diab_data$repaglinide_bin)

########## nateglinide ##########
table(df_diab_data$nateglinide)
df_diab_data$nateglinide_bin <- gsub("No","0",df_diab_data$nateglinide)
df_diab_data$nateglinide_bin <- gsub("Down","1",df_diab_data$nateglinide_bin)
df_diab_data$nateglinide_bin <- gsub("Steady","2",df_diab_data$nateglinide_bin)
df_diab_data$nateglinide_bin <- gsub("Up","3",df_diab_data$nateglinide_bin)
table(df_diab_data$nateglinide_bin)
df_diab_data$nateglinide_bin <- as.numeric(df_diab_data$nateglinide_bin)
hist(df_diab_data$nateglinide_bin)

########## chlorpropamide ##########
table(df_diab_data$chlorpropamide)
df_diab_data$chlorpropamide_bin <- gsub("No","0",df_diab_data$chlorpropamide)
df_diab_data$chlorpropamide_bin <- gsub("Down","1",df_diab_data$chlorpropamide_bin)
df_diab_data$chlorpropamide_bin <- gsub("Steady","2",df_diab_data$chlorpropamide_bin)
df_diab_data$chlorpropamide_bin <- gsub("Up","3",df_diab_data$chlorpropamide_bin)
table(df_diab_data$chlorpropamide_bin)
df_diab_data$chlorpropamide_bin <- as.numeric(df_diab_data$chlorpropamide_bin)
hist(df_diab_data$chlorpropamide_bin)

########## glimepirida ##########
table(df_diab_data$glimepiride)
df_diab_data$glimepiride_bin <- gsub("No","0",df_diab_data$glimepiride)
df_diab_data$glimepiride_bin <- gsub("Down","1",df_diab_data$glimepiride_bin)
df_diab_data$glimepiride_bin <- gsub("Steady","2",df_diab_data$glimepiride_bin)
df_diab_data$glimepiride_bin <- gsub("Up","3",df_diab_data$glimepiride_bin)
table(df_diab_data$glimepiride_bin)
df_diab_data$glimepiride_bin <- as.numeric(df_diab_data$glimepiride_bin)
hist(df_diab_data$glimepiride_bin)

########## acetohexamide ##########
table(df_diab_data$acetohexamide)
df_diab_data$acetohexamide_bin <- gsub("No","0",df_diab_data$acetohexamide)
df_diab_data$acetohexamide_bin <- gsub("Down","1",df_diab_data$acetohexamide_bin)
df_diab_data$acetohexamide_bin <- gsub("Steady","2",df_diab_data$acetohexamide_bin)
df_diab_data$acetohexamide_bin <- gsub("Up","3",df_diab_data$acetohexamide_bin)
table(df_diab_data$acetohexamide_bin)
df_diab_data$acetohexamide_bin <- as.numeric(df_diab_data$acetohexamide_bin)
hist(df_diab_data$acetohexamide_bin)

########## glipizide ##########
table(df_diab_data$glipizide)
df_diab_data$glipizide_bin <- gsub("No","0",df_diab_data$glipizide)
df_diab_data$glipizide_bin <- gsub("Down","1",df_diab_data$glipizide_bin)
df_diab_data$glipizide_bin <- gsub("Steady","2",df_diab_data$glipizide_bin)
df_diab_data$glipizide_bin <- gsub("Up","3",df_diab_data$glipizide_bin)
table(df_diab_data$glipizide_bin)
df_diab_data$glipizide_bin <- as.numeric(df_diab_data$glipizide_bin)
hist(df_diab_data$glipizide_bin)

########## glyburide ##########
table(df_diab_data$glyburide)
df_diab_data$glyburide_bin <- gsub("No","0",df_diab_data$glyburide)
df_diab_data$glyburide_bin <- gsub("Down","1",df_diab_data$glyburide_bin)
df_diab_data$glyburide_bin <- gsub("Steady","2",df_diab_data$glyburide_bin)
df_diab_data$glyburide_bin <- gsub("Up","3",df_diab_data$glyburide_bin)
table(df_diab_data$glyburide_bin)
df_diab_data$glyburide_bin <- as.numeric(df_diab_data$glyburide_bin)
hist(df_diab_data$glyburide_bin)

########## tolbutamide ##########
table(df_diab_data$tolbutamide)
df_diab_data$tolbutamide_bin <- gsub("No","0",df_diab_data$tolbutamide)
df_diab_data$tolbutamide_bin <- gsub("Down","1",df_diab_data$tolbutamide_bin)
df_diab_data$tolbutamide_bin <- gsub("Steady","2",df_diab_data$tolbutamide_bin)
df_diab_data$tolbutamide_bin <- gsub("Up","3",df_diab_data$tolbutamide_bin)
table(df_diab_data$tolbutamide_bin)
df_diab_data$tolbutamide_bin <- as.numeric(df_diab_data$tolbutamide_bin)
hist(df_diab_data$tolbutamide_bin)

########## pioglitazone ##########
table(df_diab_data$pioglitazone)
df_diab_data$pioglitazone_bin <- gsub("No","0",df_diab_data$pioglitazone)
df_diab_data$pioglitazone_bin <- gsub("Down","1",df_diab_data$pioglitazone_bin)
df_diab_data$pioglitazone_bin <- gsub("Steady","2",df_diab_data$pioglitazone_bin)
df_diab_data$pioglitazone_bin <- gsub("Up","3",df_diab_data$pioglitazone_bin)
table(df_diab_data$pioglitazone_bin)
df_diab_data$pioglitazone_bin <- as.numeric(df_diab_data$pioglitazone_bin)
hist(df_diab_data$pioglitazone_bin)

########## rosiglitazone ##########
table(df_diab_data$rosiglitazone)
df_diab_data$rosiglitazone_bin <- gsub("No","0",df_diab_data$rosiglitazone)
df_diab_data$rosiglitazone_bin <- gsub("Down","1",df_diab_data$rosiglitazone_bin)
df_diab_data$rosiglitazone_bin <- gsub("Steady","2",df_diab_data$rosiglitazone_bin)
df_diab_data$rosiglitazone_bin <- gsub("Up","3",df_diab_data$rosiglitazone_bin)
table(df_diab_data$rosiglitazone_bin)
df_diab_data$rosiglitazone_bin <- as.numeric(df_diab_data$rosiglitazone_bin)
hist(df_diab_data$rosiglitazone_bin)

########## acarbose ##########
table(df_diab_data$acarbose)
df_diab_data$acarbose_bin <- gsub("No","0",df_diab_data$acarbose)
df_diab_data$acarbose_bin <- gsub("Down","1",df_diab_data$acarbose_bin)
df_diab_data$acarbose_bin <- gsub("Steady","2",df_diab_data$acarbose_bin)
df_diab_data$acarbose_bin <- gsub("Up","3",df_diab_data$acarbose_bin)
table(df_diab_data$acarbose_bin)
df_diab_data$acarbose_bin <- as.numeric(df_diab_data$acarbose_bin)
hist(df_diab_data$acarbose_bin)

########## miglitol ##########
table(df_diab_data$miglitol)
df_diab_data$miglitol_bin <- gsub("No","0",df_diab_data$miglitol)
df_diab_data$miglitol_bin <- gsub("Down","1",df_diab_data$miglitol_bin)
df_diab_data$miglitol_bin <- gsub("Steady","2",df_diab_data$miglitol_bin)
df_diab_data$miglitol_bin <- gsub("Up","3",df_diab_data$miglitol_bin)
table(df_diab_data$miglitol_bin)
df_diab_data$miglitol_bin <- as.numeric(df_diab_data$miglitol_bin)
hist(df_diab_data$miglitol_bin)

########## troglitazone ##########
table(df_diab_data$troglitazone)
df_diab_data$troglitazone_bin <- gsub("No","0",df_diab_data$troglitazone)
df_diab_data$troglitazone_bin <- gsub("Down","1",df_diab_data$troglitazone_bin)
df_diab_data$troglitazone_bin <- gsub("Steady","2",df_diab_data$troglitazone_bin )
df_diab_data$troglitazone_bin <- gsub("Up","3",df_diab_data$troglitazone_bin)
table(df_diab_data$troglitazone_bin)
df_diab_data$troglitazone_bin <- as.numeric(df_diab_data$troglitazone_bin)
hist(df_diab_data$troglitazone_bin)

########## tolazamide ##########
table(df_diab_data$tolazamide)
df_diab_data$tolazamide_bin <- gsub("No","0",df_diab_data$tolazamide)
df_diab_data$tolazamide_bin <- gsub("Down","1",df_diab_data$tolazamide_bin)
df_diab_data$tolazamide_bin <- gsub("Steady","2",df_diab_data$tolazamide_bin )
df_diab_data$tolazamide_bin <- gsub("Up","3",df_diab_data$tolazamide_bin)
table(df_diab_data$tolazamide_bin)
df_diab_data$tolazamide_bin <- as.numeric(df_diab_data$tolazamide_bin)
hist(df_diab_data$tolazamide_bin)

########## examide ##########
table(df_diab_data$examide)
df_diab_data$examide_bin <- gsub("No","0",df_diab_data$examide)
df_diab_data$examide_bin <- gsub("Down","1",df_diab_data$examide_bin)
df_diab_data$examide_bin <- gsub("Steady","2",df_diab_data$examide_bin )
df_diab_data$examide_bin <- gsub("Up","3",df_diab_data$examide_bin)
table(df_diab_data$examide_bin)
df_diab_data$examide_bin <- as.numeric(df_diab_data$examide_bin)
hist(df_diab_data$examide_bin)

########## citogliptone ##########
table(df_diab_data$citoglipton)
df_diab_data$citoglipton_bin <- gsub("No","0",df_diab_data$citoglipton)
df_diab_data$citoglipton_bin <- gsub("Down","1",df_diab_data$citoglipton_bin)
df_diab_data$citoglipton_bin <- gsub("Steady","2",df_diab_data$citoglipton_bin )
df_diab_data$citoglipton_bin <- gsub("Up","3",df_diab_data$citoglipton_bin)
table(df_diab_data$citoglipton_bin)
df_diab_data$citoglipton_bin <- as.numeric(df_diab_data$citoglipton_bin)
hist(df_diab_data$citoglipton_bin)

########## insuline ##########
table(df_diab_data$insulin)
df_diab_data$insulin_bin <- gsub("No","0",df_diab_data$insulin)
df_diab_data$insulin_bin <- gsub("Down","1",df_diab_data$insulin_bin)
df_diab_data$insulin_bin <- gsub("Steady","2",df_diab_data$insulin_bin )
df_diab_data$insulin_bin <- gsub("Up","3",df_diab_data$insulin_bin)
table(df_diab_data$insulin_bin)
df_diab_data$insulin_bin <- as.numeric(df_diab_data$insulin_bin)
hist(df_diab_data$insulin_bin)

########## glyburide-metformin ##########
table(df_diab_data$glyburide.metformin)
df_diab_data$glyburide.metformin_bin <- gsub("No","0",df_diab_data$glyburide.metformin)
df_diab_data$glyburide.metformin_bin <- gsub("Down","1",df_diab_data$glyburide.metformin_bin)
df_diab_data$glyburide.metformin_bin <- gsub("Steady","2",df_diab_data$glyburide.metformin_bin )
df_diab_data$glyburide.metformin_bin <- gsub("Up","3",df_diab_data$glyburide.metformin_bin)
table(df_diab_data$glyburide.metformin_bin)
df_diab_data$glyburide.metformin_bin <- as.numeric(df_diab_data$glyburide.metformin_bin)
hist(df_diab_data$glyburide.metformin_bin)

########## blipizide-metformin ##########
table(df_diab_data$glipizide.metformin)
df_diab_data$glipizide.metformin_bin <- gsub("No","0",df_diab_data$glipizide.metformin)
df_diab_data$glipizide.metformin_bin <- gsub("Down","1",df_diab_data$glipizide.metformin_bin)
df_diab_data$glipizide.metformin_bin <- gsub("Steady","2",df_diab_data$glipizide.metformin_bin )
df_diab_data$glipizide.metformin_bin <- gsub("Up","3",df_diab_data$glipizide.metformin_bin)
table(df_diab_data$glipizide.metformin_bin)
df_diab_data$glipizide.metformin_bin <- as.numeric(df_diab_data$glipizide.metformin_bin)
hist(df_diab_data$glipizide.metformin_bin)

########## glimepiride-pioglitazone ##########
table(df_diab_data$glimepiride.pioglitazone)
df_diab_data$glimepiride.pioglitazone_bin <- gsub("No","0",df_diab_data$glimepiride.pioglitazone)
df_diab_data$glimepiride.pioglitazone_bin <- gsub("Down","1",df_diab_data$glimepiride.pioglitazone_bin)
df_diab_data$glimepiride.pioglitazone_bin <- gsub("Steady","2",df_diab_data$glimepiride.pioglitazone_bin )
df_diab_data$glimepiride.pioglitazone_bin <- gsub("Up","3",df_diab_data$glimepiride.pioglitazone_bin)
table(df_diab_data$glimepiride.pioglitazone_bin)
df_diab_data$glimepiride.pioglitazone_bin <- as.numeric(df_diab_data$glimepiride.pioglitazone_bin)
hist(df_diab_data$glimepiride.pioglitazone_bin)

########## metformin-rosiglitazone ##########
table(df_diab_data$metformin.rosiglitazone)
df_diab_data$metformin.rosiglitazone_bin <- gsub("No","0",df_diab_data$metformin.rosiglitazone)
df_diab_data$metformin.rosiglitazone_bin <- gsub("Down","1",df_diab_data$metformin.rosiglitazone_bin)
df_diab_data$metformin.rosiglitazone_bin <- gsub("Steady","2",df_diab_data$metformin.rosiglitazone_bin )
df_diab_data$metformin.rosiglitazone_bin <- gsub("Up","3",df_diab_data$metformin.rosiglitazone_bin)
table(df_diab_data$metformin.rosiglitazone_bin)
df_diab_data$metformin.rosiglitazone_bin <- as.numeric(df_diab_data$metformin.rosiglitazone_bin)
hist(df_diab_data$metformin.rosiglitazone_bin)

########## metformin-pioglitazone ##########
table(df_diab_data$metformin.pioglitazone)
df_diab_data$metformin.pioglitazone_bin <- gsub("No","0",df_diab_data$metformin.pioglitazone)
df_diab_data$metformin.pioglitazone_bin <- gsub("Down","1",df_diab_data$metformin.pioglitazone_bin)
df_diab_data$metformin.pioglitazone_bin <- gsub("Steady","2",df_diab_data$metformin.pioglitazone_bin )
df_diab_data$metformin.pioglitazone_bin <- gsub("Up","3",df_diab_data$metformin.pioglitazone_bin)
table(df_diab_data$metformin.pioglitazone_bin)
df_diab_data$metformin.pioglitazone_bin <- as.numeric(df_diab_data$metformin.pioglitazone_bin)
hist(df_diab_data$metformin.pioglitazone_bin)

########## Y variable - diabetesMed ##########
###Check the information - diabetesMed
table(df_diab_data$diabetesMed)
#Change Yes for 1, No for 0                  
df_diab_data$diabetesMed_dummy <- gsub("Yes","1",df_diab_data$diabetesMed) 
df_diab_data$diabetesMed_dummy <- gsub("No","0",df_diab_data$diabetesMed_dummy)
#Change the result into numeric
df_diab_data$diabetesMed_dummy <- as.numeric(df_diab_data$diabetesMed_dummy)
#Check the value for the dummy
table(df_diab_data$diabetesMed_dummy)

#Select all the new numeric variables for logistic regression
df_logistic <- df_diab_data[, 51:81]
#check the logistic regression with all X variables
logistic_all <- glm(diabetesMed_dummy ~ .,family = "binomial",data = df_logistic)
print(logistic_all)
#check the p-value
summary(logistic_all)

#Select the new numeric variables we need for logistic regression
df_logistic_1 <- df_diab_data[,51:57]
#Add Y variables in dataframe
df_logistic_1$diabetesMed_dummy <- df_diab_data[,81]
#check the logistic regression with the demographic or medical status X variables
logistic_1 <- glm(diabetesMed_dummy ~ .,family = "binomial",data = df_logistic_1)
print(logistic_1)
#check the p-value for selected variables and the coeffienct.
summary(logistic_1)

#check the result between race_bin and diabetesMed
xtabs(~ diabetesMed_dummy + race_bin,data = df_diab_data)
xtabs(~ diabetesMed_dummy + dis_dis_bin,data = df_diab_data)
xtabs(~ diabetesMed_dummy + payer_code_bin,data = df_diab_data)
xtabs(~ diabetesMed_dummy + age_bin,data = df_diab_data)
xtabs(~ diabetesMed_dummy + admission_referral_bin,data = df_diab_data)
xtabs(~ diabetesMed_dummy + medical_specialty_bin,data = df_diab_data)

#selecting the column that related to the recommendation
#race, payer_code, age
col_index <- c(1,4,5,31)
#creatting the data frame only have the X variables we choose and the Y variable
df_try <- df_logistic[,col_index]
#create regression model
logistic_final <- glm(diabetesMed_dummy ~ .,family = "binomial",data = df_try)
#summary the insights from the regression
summary(logistic_final)

