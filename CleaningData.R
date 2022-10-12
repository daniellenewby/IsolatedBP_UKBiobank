
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

bd <- fread("C:/Users/dnewby/Desktop/UKBB/ukb41533.csv", header=TRUE, nrows = 0)


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

scanner_position_brain_imaging <- fread("C:/Users/dnewby/Desktop/UKBB/ukb41533.csv", 
                                        header=TRUE, 
                                        sep=",", 
                                        verbose = T,
                                        #fill = FALSE ,
                                        #quote = "",
                                        #nrows = 1000 ,
                                        #nrows = 502507,
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

###################################################################
# AND hippocampal free surfer whole, body, head and tail
##################################################################

setDTthreads(0L)

bd <- fread("C:/Users/dnewby/Desktop/UKBB/ukb41321.csv", header=TRUE, nrows = 0)


# get column names for imaging
bdtest <- bd[,grep("[0-9]{1,10}\\-[2]\\.[0-9]|[a-z]{1,3}", colnames(bd), value = TRUE ), ]

colwewant <- c("eid",   ## id
               "^25781\\-"  # white matter hyperintensity,
               
            )


bdtest3 <- unique(grep(paste(colwewant, collapse ="|"),
                       bdtest, value = TRUE, fixed = FALSE ))


tic()

brain_imaging_rawdata_WMH <- fread("C:/Users/dnewby/Desktop/UKBB/ukb41321.csv", 
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


save(Brain_imaging_subset, file = paste(saving_directory, "brain_imaging_compiled_13122020.Rdata", sep=""))








