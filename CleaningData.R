
# READING IN ALL DATA FOR ANALYSIS AND CLEANING

setDTthreads(0L)

bd <- fread(paste0(here("GitHub", "IsolatedBP_UKBiobank", "Data"),"/ukb40537.csv"), header=TRUE, nrows = 0)
  
# get column names for imaging
bdtest <- bd[,grep("[0-9]{1,10}\\-[2]\\.[0-9]|[a-z]{1,3}", colnames(bd), value = TRUE ), ]

# we are interested in Category 110 Brain MRI imaging from T1 structural brain MRI

##########################################################################
# BRAIN IMAGING VARIABLES #
###########################################################################

colwewant <- c("eid",   ## id
               "^53\\-",
               "^25000\\-"   ,      #Volumetric scaling from T1 head image to standard space
               "^25010\\-"   ,      #Volume of brain, grey+white matter
               "^25008\\-"   ,      #Volume of white matter
               "^25006\\-"   ,      #Volume of grey matter
               "^25004\\-"   ,      #Volume of ventricular cerebrospinal fluid
               "^25023\\-"   ,      #Volume of accumbens (left)
               "^25024\\-"   ,      #Volume of accumbens (right)
               "^25021\\-"   ,      #Volume of amygdala (left)
               "^25022\\-"   ,      #Volume of amygdala (right)
               "^25013\\-"   ,      #Volume of caudate (left)
               "^25014\\-"   ,      #Volume of caudate (right)
               "^25019\\-"   ,      #Volume of hippocampus (left)
               "^25020\\-"   ,      #Volume of hippocampus (right)
               "^25017\\-"   ,      #Volume of pallidum (left)
               "^25018\\-"   ,      #Volume of pallidum (right)
               "^25015\\-"   ,      #Volume of putamen (left)
               "^25016\\-"   ,      #Volume of putamen (right)
               "^25011\\-"   ,      #Volume of thalamus (left)
               "^25012\\-"       #Volume of thalamus (right)

               
)



bdtest3 <- unique(grep(paste(colwewant, collapse ="|"),
                       bdtest, value = TRUE, fixed = FALSE ))



tic()
# read in dataset with columns needed
brain_imaging_rawdata <- fread(paste0(here("GitHub", "IsolatedBP_UKBiobank", "Data"),"/ukb40537.csv"), 
                               header=TRUE, 
                               sep=",", 
                               verbose = T,
                               select = bdtest3)

toc()   


# subset to only include those with brain imgaging data
brain_imaging_rawdata1 <- subset(brain_imaging_rawdata, as.character(brain_imaging_rawdata$`53-2.0`) != "" & !is.na(brain_imaging_rawdata$`25000-2.0`) )

##############################################
# labelling the brain imaging data variables #
##############################################

names(brain_imaging_rawdata1)[names(brain_imaging_rawdata1) == "25000-2.0"] <- "f.25000_Volumetric_scaling_T1_head_space"
names(brain_imaging_rawdata1)[names(brain_imaging_rawdata1) == "25010-2.0"] <- "f.25010_Volbrain_grey+white matter"
names(brain_imaging_rawdata1)[names(brain_imaging_rawdata1) == "25006-2.0"] <- "f.25006_Volgrey matter"
names(brain_imaging_rawdata1)[names(brain_imaging_rawdata1) == "25004-2.0"] <- "f.25004_vol_ventricular_CSF"
names(brain_imaging_rawdata1)[names(brain_imaging_rawdata1) == "25023-2.0"] <- "f.25023_vol_accumbens_L"
names(brain_imaging_rawdata1)[names(brain_imaging_rawdata1) == "25024-2.0"] <- "f.25024_vol_accumbens_R"
names(brain_imaging_rawdata1)[names(brain_imaging_rawdata1) == "25021-2.0"] <- "f.25021_vol_amygdala_L"
names(brain_imaging_rawdata1)[names(brain_imaging_rawdata1) == "25022-2.0"] <- "f.25022_vol_amygdala_R"
names(brain_imaging_rawdata1)[names(brain_imaging_rawdata1) == "25019-2.0"] <- "f.25019_vol_hippocampus_L"
names(brain_imaging_rawdata1)[names(brain_imaging_rawdata1) == "25020-2.0"] <- "f.25020_vol_hippocampus_R"
names(brain_imaging_rawdata1)[names(brain_imaging_rawdata1) == "25017-2.0"] <- "f.25017_vol_pallidum_L"
names(brain_imaging_rawdata1)[names(brain_imaging_rawdata1) == "25018-2.0"] <- "f.25018_vol_pallidum_R"
names(brain_imaging_rawdata1)[names(brain_imaging_rawdata1) == "25015-2.0"] <- "f.25015_vol_putamen_L"
names(brain_imaging_rawdata1)[names(brain_imaging_rawdata1) == "25016-2.0"] <- "f.25016_vol_putamen_R"
names(brain_imaging_rawdata1)[names(brain_imaging_rawdata1) == "25013-2.0"] <- "f.25013_vol_caudate_L"
names(brain_imaging_rawdata1)[names(brain_imaging_rawdata1) == "25014-2.0"] <- "f.25014_vol_caudate_R"
names(brain_imaging_rawdata1)[names(brain_imaging_rawdata1) == "25011-2.0"] <- "f.25011_vol_thalamus_L"
names(brain_imaging_rawdata1)[names(brain_imaging_rawdata1) == "25012-2.0"] <- "f.25012_vol_thalamus_R"

##########
# date of visit #
##########
names(brain_imaging_rawdata1)[names(brain_imaging_rawdata1) == "53-2.0"] <- "Date_of_visit"


##############################################################################
# scanner position imaging variables #
#############################################################################

setDTthreads(0L)

bd <- fread(paste0(here("GitHub", "IsolatedBP_UKBiobank", "Data"),"/ukb41533.csv"), header=TRUE, nrows = 0)


# get column names for imaging
bdtest <- bd[,grep("[0-9]{1,10}\\-[2]\\.[0-9]|[a-z]{1,3}", colnames(bd), value = TRUE ), ]

colwewant <- c("eid",   ## id
               "^54\\-", # assessment centre
               "^25756\\-"   ,      #Scanner lateral (X) brain position space
               "^25757\\-"   ,      #Scanner transverse (Y) brain position
               "^25758\\-"   ,      #Scanner longitudinal (Z) brain position
               "^25759\\-"        #Scanner table position
)


bdtest3 <- unique(grep(paste(colwewant, collapse ="|"),
                       bdtest, value = TRUE, fixed = FALSE ))


tic()

scanner_position_brain_imaging <- fread(paste0(here("GitHub", "IsolatedBP_UKBiobank", "Data"),"/ukb41533.csv"), 
                                        header=TRUE, 
                                        sep=",", 
                                        verbose = T,
                                        select = bdtest3)

toc()          


#rename the variables
names(scanner_position_brain_imaging)[names(scanner_position_brain_imaging) == "25756-2.0"] <- "25756-2_ScannerXposition"

names(scanner_position_brain_imaging)[names(scanner_position_brain_imaging) == "25757-2.0"] <- "25757-2.ScannerYposition"

names(scanner_position_brain_imaging)[names(scanner_position_brain_imaging) == "25758-2.0"] <- "25758-2_ScannerZposition"

names(scanner_position_brain_imaging)[names(scanner_position_brain_imaging) == "25759-2.0"] <- "25759-2_Scannerposition"

scanner_position_brain_imaging <- subset(scanner_position_brain_imaging, scanner_position_brain_imaging$eid %in% brain_imaging_rawdata1$eid)


###################################
### white matter hypertensities ###
###################################

setDTthreads(0L)

bd <- fread(paste0(here("GitHub", "IsolatedBP_UKBiobank", "Data"),"/ukb41321.csv"), header=TRUE, nrows = 0)


# get column names for imaging
bdtest <- bd[,grep("[0-9]{1,10}\\-[2]\\.[0-9]|[a-z]{1,3}", colnames(bd), value = TRUE ), ]

colwewant <- c("eid",   ## id
               "^25781\\-"  # white matter hyperintensity,
              )


bdtest3 <- unique(grep(paste(colwewant, collapse ="|"),
                       bdtest, value = TRUE, fixed = FALSE ))


tic()

brain_imaging_rawdata_WMH <- fread(paste0(here("GitHub", "IsolatedBP_UKBiobank", "Data"),"/ukb41321.csv"), 
                                   header=TRUE, 
                                   sep=",", 
                                   verbose = T,
                                   select = bdtest3)

toc()          


brain_imaging_rawdata_WMH1 <- subset(brain_imaging_rawdata_WMH, brain_imaging_rawdata_WMH$eid %in% brain_imaging_rawdata1$eid)

names(brain_imaging_rawdata_WMH1)[names(brain_imaging_rawdata_WMH1) == "25781-2.0"] <- "f.25781_WMH"


# merge brain imaging data together
Brain_imaging_subset <- merge(brain_imaging_rawdata1, brain_imaging_rawdata_WMH1,  by = "eid")
Brain_imaging_subset <- merge(Brain_imaging_subset, scanner_position_brain_imaging,  by = "eid")


save(Brain_imaging_subset, file = paste(saving_directory, "/brain_imaging_compiled_13122020.Rdata", sep=""))

###############################
# demographics and cognition
###############################

setDTthreads(0L)

bd <- fread(paste0(here("GitHub", "IsolatedBP_UKBiobank", "Data"),"/ukb40537.csv"), header=TRUE, nrows = 0)

bdtest <- bd[,grep("[0-9]{1,10}\\-[0|2]\\.[0-9]|[a-z]{1,3}", colnames(bd), value = TRUE ), ]

colwewant1 <- c("eid",   ## id
                "^31\\-0",          ## sex
                "^21003\\-2",       ## age at assessment 
                "^399\\-2",         ## memory test pairs matching second round of 6 cards
                "^400\\-2",         ## time to complete memory test we need 400.02 for 6 card test
                "^404\\-2",         ## duration to first press of snap button in each round
                "^398\\-2",         ## pairs matching Number of correct matches in round
                "^20016\\-2",       ## fluid intelligence score number of correct answers 0-13
                "^20023\\-2",       ## Mean reaction time
                "^189\\-" ,         # Townsend deprivation index at recruitment
                "^6138\\-2",        ## Qualifications
                "^20116\\-2",       ## smoking status
                "^21000\\-2",       ## ethnic background imaging
                "^21000\\-0",       ## ethnic background baseline
                "^21001\\-2"   ,   ## BMI
                "^54\\-2"    ,     ## UK BB assessment centre
                "^2966\\-2"  ,# age high blood pressure diagnosed
                "^2976\\-2"  ,# Age diabetes diagnosed
                "^2443\\-2"  ,# Diabetes diagnosed by doctor
                "^4079\\-2"  ,# diastolic blood pressure automated reading
                "^4080\\-2", # systolic blood pressure automated reading
                "^6177\\-2", # medication for cholesterol, blood pressure or diabetes
                "^6153\\-2", # medication for cholesterol, blood pressure or diabetes or exogenous hormones
                "^2966\\-2"  # age high blood pressure diagnosed
)



bdtest4 <- unique(grep(paste(colwewant1, collapse ="|"),
                       bdtest, value = TRUE, fixed = FALSE
))



tic()
# read in dataset 
demographics_cog_rawdata <- fread(paste0(here("GitHub", "IsolatedBP_UKBiobank", "Data"),"/ukb40537.csv"), 
                                  header=TRUE, 
                                  sep=",", 
                                  verbose = T,
                                  select = bdtest4)

toc()



#subset to those that have imaging data
demographics_cog_rawdata1 <- subset(demographics_cog_rawdata, demographics_cog_rawdata$eid %in% Brain_imaging_subset$eid)


#####################################
# extra cognitive variables
######################################

# trail making, matrix and digits

setDTthreads(0L)

bd <- fread("C:/Users/dnewby/Desktop/UKBB/ukb43218.csv", header=TRUE, nrows = 0)

# get column names for imaging
#
#  #get column names for imaging
bdtest2a <- bd[,grep("[0-9]{1,10}\\-[0|1|2]\\.[0-9]|[a-z]{1,3}", colnames(bd), value = TRUE ), ]
# #
# # # to select the columns we need
colwewant1 <- c("eid",   ## id
                "^6348\\-2",         ## Duration to complete numeric path (trail #1)
                "^6350\\-2",         ## Duration to complete numeric path (trail #2)
                "^6373\\-2" ,      ## Number of puzzles correctly solved (Matrix pattern completion)
                "^23324\\-2"         ## Number of symbol digit matches made correctly
                
)


bdtest4a <- unique(grep(paste(colwewant1, collapse ="|"),
                        bdtest2a, value = TRUE, fixed = FALSE
))


tic()
# # read in dataset takes nearly 1 hour (might be less now removed columns we dont need)
extra_cog_rawdata <- fread("C:/Users/dnewby/Desktop/UKBB/ukb43218.csv",
                           header=TRUE,
                           sep=",",
                           verbose = T,
                           select = bdtest4a)

toc()
# 
# 
# 



newcogtests_rawdata <- subset(extra_cog_rawdata, extra_cog_rawdata$eid %in% Brain_imaging_subset$eid)

names(newcogtests_rawdata)[names(newcogtests_rawdata) == "6348-2.0" ] <- "f6348_Duration_trailA"

names(newcogtests_rawdata)[names(newcogtests_rawdata) == "6350-2.0" ] <- "f6350_Duration_trailB" 

names(newcogtests_rawdata)[names(newcogtests_rawdata) == "6373-2.0" ] <- "f6373_matrix_pattern_puzzles_solved" 

names(newcogtests_rawdata)[names(newcogtests_rawdata) == "23324-2.0" ] <- "f23324_correct_symbol_digit_matches" 



#####################################
# tower rearranging
######################################

setDTthreads(0L)


bd <- fread("C:/Users/dnewby/Desktop/UKBB/ukb43579.csv", header=TRUE, nrows = 0)

# get column names for imaging
#
#  #get column names for imaging
bdtest2ab <- bd[,grep("[0-9]{1,10}\\-[0|1|2]\\.[0-9]|[a-z]{1,3}", colnames(bd), value = TRUE ), ]
# #
# # # to select the columns we need
colwewant8 <- c("eid",   ## id        ## Duration to complete numeric path (trail #1)
                "21004\\-2"      ## Number of puzzles correctly solved (tower)
                
                
)


bdtest4ab <- unique(grep(paste(colwewant8, collapse ="|"),
                         bdtest2ab, value = TRUE, fixed = FALSE
))


tic()
# # read in dataset takes nearly 1 hour (might be less now removed columns we dont need)
extra_cog_tower_rawdata <- fread("C:/Users/dnewby/Desktop/UKBB/ukb43579.csv",
                                 header=TRUE,
                                 sep=",",
                                 verbose = T,
                                 select = bdtest4ab)

toc()
# 
# 
# 



newcogtests_tower_rawdata <- subset(extra_cog_tower_rawdata , extra_cog_tower_rawdata$eid %in% Brain_imaging_subset$eid)

names(newcogtests_tower_rawdata)[names(newcogtests_tower_rawdata) == "21004-2.0" ] <- "21004_correct_tower_arranging"
# 




#
#############################################################################################
# in this section we are extracting the number of people taking bp meds, diabetics etc
# subset the data you need 6177############################################################

demographics_cog_rawdata2 <- demographics_cog_rawdata1[,c("6177-2.0" ,
                                                          "6177-2.1" ,
                                                          "6177-2.2")]


diseases_UKBB <- unique(c(as.matrix(demographics_cog_rawdata2[,paste("6177-2.",c(0:2),sep="")])))
# 
diseases_UKBB <- diseases_UKBB[!is.na(diseases_UKBB)]
stopifnot(length(diseases_UKBB) == 6)
# 
# # this takes ages!!
# # Using function over disease columns create a boolean matrix for each cancer disease for each subject in UKBB
bCase_iSiD_diseases = findCases( diseases_UKBB , demographics_cog_rawdata2[,paste("6177-2.",c(0:2),sep="")])


colnames(bCase_iSiD_diseases) <- diseases_UKBB
dim(bCase_iSiD_diseases) # should be xx diseases
rownames(bCase_iSiD_diseases) <- demographics_cog_rawdata1$eid


# turn into a integer matrix
bCase_iSiD_diseases <- 1 * bCase_iSiD_diseases

# subset to only include the ones we need (BP, high chol, insulin etc)
bCase_iSiD_diseases <- bCase_iSiD_diseases[,c(2,3,6)]


# rename columns 
colnames(bCase_iSiD_diseases) <- c("High_chol_M",
                                   "BP_medications_M",
                                   "Insulin_M"
)




#################################################################################################
#############################################################################################
# in this section we are extracting the number of people taking bp meds, diabetics etc
# subset the data you need 6177############################################################

demographics_cog_rawdata2a <- demographics_cog_rawdata1[,c("6153-2.0" ,
                                                           "6153-2.1" ,
                                                           "6153-2.2")]


diseases_UKBBa <- unique(c(as.matrix(demographics_cog_rawdata2a[,paste("6153-2.",c(0:2),sep="")])))
# 
diseases_UKBBa <- diseases_UKBBa[!is.na(diseases_UKBBa)]
stopifnot(length(diseases_UKBBa) == 8)
# 
# # this takes ages!!
# # Using function over disease columns create a boolean matrix for each cancer disease for each subject in UKBB
bCase_iSiD_diseasesa = findCases( diseases_UKBBa , demographics_cog_rawdata2a[,paste("6153-2.",c(0:2),sep="")])


colnames(bCase_iSiD_diseasesa) <- diseases_UKBBa
dim(bCase_iSiD_diseasesa) # should be xx diseases
rownames(bCase_iSiD_diseasesa) <- demographics_cog_rawdata1$eid


# turn into a integer matrix
bCase_iSiD_diseasesa <- 1 * bCase_iSiD_diseasesa

# subset to only include the ones we need (BP, high chol, insulin etc)
bCase_iSiD_diseasesa <- bCase_iSiD_diseasesa[,c(4,2,7)]


# rename columns 
colnames(bCase_iSiD_diseasesa) <- c("High_chol_F",
                                    "BP_medications_F",
                                    "Insulin_F"
)



# join the two together

bCase_iSiD_diseases_both <- cbind(bCase_iSiD_diseases,
                                  bCase_iSiD_diseasesa)

bCase_iSiD_diseases_both <- as.data.frame(bCase_iSiD_diseases_both)


bCase_iSiD_diseases_both$BP_medications <- bCase_iSiD_diseases_both$BP_medications_M + bCase_iSiD_diseases_both$BP_medications_F

bCase_iSiD_diseases_both$Cholesterol_medications <- bCase_iSiD_diseases_both$High_chol_M + bCase_iSiD_diseases_both$High_chol_F

bCase_iSiD_diseases_both$Insulin_medications <- bCase_iSiD_diseases_both$Insulin_M + bCase_iSiD_diseases_both$Insulin_F

bCase_iSiD_diseases_both$eid <- rownames(bCase_iSiD_diseases_both)

#susbet this

bCase_iSiD_diseases_both <- bCase_iSiD_diseases_both[,7:10]

bCase_iSiD_diseases_both$eid <- as.integer(bCase_iSiD_diseases_both$eid)



#############################################################################
#merge back into cognitive tests and demographics 
Cognition_demographics_subset <- merge(newcogtests_tower_rawdata,  newcogtests_rawdata,  by = "eid")

Cognition_demographics_subset <- merge(Cognition_demographics_subset, demographics_cog_rawdata1,  by = "eid")

Cognition_demographics_subset <- merge(Cognition_demographics_subset,  bCase_iSiD_diseases_both,  by = "eid")


save(Cognition_demographics_subset, file = paste(saving_directory, "Cognition_demographics_subset_compiled_13122020.Rdata", sep=""))








