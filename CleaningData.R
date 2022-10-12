
# READING IN ALL DATA FOR ANALYSIS AND CLEANING

setDTthreads(0L)

bd <- fread(paste0(here("GitHub", "IsolatedBP_UKBiobank", "Data"),"/ukb40537.csv"))
  
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
               "^25009\\-" ,#Volume of brain, grey+white matter (normalised for head size)
               "^25008\\-"   ,      #Volume of white matter
               "^25007\\-"   ,      #Volume of white matter (normalised for head size)
               "^25006\\-"   ,      #Volume of grey matter
               "^25005\\-"   ,      #Volume of grey matter (normalised for head size)
               "^25002\\-"   ,      #Volume of peripheral cortical grey matter
               "^25001\\-"   ,      #Volume of peripheral cortical grey matter (normalised for head size)
               "^25004\\-"   ,      #Volume of ventricular cerebrospinal fluid
               "^25003\\-"   ,      #Volume of ventricular cerebrospinal fluid (normalised for head size)
               "^25886\\-"   ,      #Volume of grey matter in Hippocampus (left)
               "^25887\\-"   ,      #Volume of grey matter in Hippocampus (right)
               # "25781\\-"   ,      #white matter hyperintensitiy (dont have access to)
               "^25139\\-"   ,    #Mean MD in cingulum cingulate gyrus on FA skeleton L
               "^25138\\-"  ,   #Mean MD in cingulum cingulate gyrus on FA skeleton (right)
               "^25091\\-"   ,         #	Mean FA in cingulum cingulate gyrus on FA skeleton (left)
               "^25090\\-"    ,            # Mean FA in cingulum cingulate gyrus on FA skeleton (right)
               "^25093\\-"    ,        # Mean FA in cingulum hippocampus on FA skeleton (left)
               "^25092\\-"    ,           #Mean FA in cingulum hippocampus on FA skeleton (right)
               "^25141\\-"    ,   #	Mean MD in cingulum hippocampus on FA skeleton (left)
               "^25140\\-"    , #	Mean MD in cingulum hippocampus on FA skeleton (right)
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
               "^25012\\-"     ,  #Volume of thalamus (right)
               "^25738\\-"	,#Discrepancy between SWI brain image and T1 brain image
               "^25038\\-"	,#Median T2star in accumbens (left)
               "^25039\\-"	,#Median T2star in accumbens (right)
               "^25036\\-"	,#Median T2star in amygdala (left)
               "^25037\\-"	,#Median T2star in amygdala (right)
               "^25028\\-"	,#Median T2star in caudate (left)
               "^25029\\-"	,#Median T2star in caudate (right)
               "^25034\\-"	,#Median T2star in hippocampus (left)
               "^25035\\-"	,#Median T2star in hippocampus (right)
               "^25032\\-"	,#Median T2star in pallidum (left)
               "^25033\\-"	,#Median T2star in pallidum (right)
               "^25030\\-"	,#Median T2star in putamen (left)
               "^25031\\-"	,#Median T2star in putamen (right)
               "^25026\\-"	,#Median T2star in thalamus (left)
               "^25027\\-"	#Median T2star in thalamus (right)
               
)



bdtest3 <- unique(grep(paste(colwewant, collapse ="|"),
                       bdtest, value = TRUE, fixed = FALSE ))



tic()
# read in dataset takes nearly 1 hour (might be less now removed columns we dont need)
brain_imaging_rawdata <- fread("C:/Users/dnewby/Desktop/UKBB/ukb40537.csv", 
                               header=TRUE, 
                               sep=",", 
                               verbose = T,
                               #fill = FALSE ,
                               #quote = "",
                               #nrows = 1000 ,
                               #nrows = 502507,
                               select = bdtest3)

toc()   


# subset to only include those with brain imgaging data

brain_imaging_rawdata1 <- subset(brain_imaging_rawdata, as.character(brain_imaging_rawdata$`53-2.0`) != "" & !is.na(brain_imaging_rawdata$`25000-2.0`) )

##############################################
# labelling the brain imaging data variables #
##############################################

names(brain_imaging_rawdata1)[names(brain_imaging_rawdata1) == "25000-2.0"] <- "f.25000_Volumetric_scaling_T1_head_space"
names(brain_imaging_rawdata1)[names(brain_imaging_rawdata1) == "25010-2.0"] <- "f.25010_Volbrain_grey+white matter"
names(brain_imaging_rawdata1)[names(brain_imaging_rawdata1) == "25009-2.0"] <- "f.25009_Volbrain_grey+white matter_normalised"
names(brain_imaging_rawdata1)[names(brain_imaging_rawdata1) == "25008-2.0"] <- "f.25008_Volwhite matter"
names(brain_imaging_rawdata1)[names(brain_imaging_rawdata1) == "25007-2.0"] <- "f.25007_Volbrain_white_matter_normalised"
names(brain_imaging_rawdata1)[names(brain_imaging_rawdata1) == "25006-2.0"] <- "f.25006_Volgrey matter"
names(brain_imaging_rawdata1)[names(brain_imaging_rawdata1) == "25005-2.0"] <- "f.25005_Volbrain_grey_matter_normalised"
names(brain_imaging_rawdata1)[names(brain_imaging_rawdata1) == "25002-2.0"] <- "f.25002_vol_peripheral_cortical_grey"
names(brain_imaging_rawdata1)[names(brain_imaging_rawdata1) == "25001-2.0"] <- "f.25001_vol_peripheral_cortical_grey_normalised"
names(brain_imaging_rawdata1)[names(brain_imaging_rawdata1) == "25004-2.0"] <- "f.25004_vol_ventricular_CSF"
names(brain_imaging_rawdata1)[names(brain_imaging_rawdata1) == "25003-2.0"] <- "f.25003_vol_ventricular_CSF_normalised"
names(brain_imaging_rawdata1)[names(brain_imaging_rawdata1) == "25886-2.0"] <- "f.25886_FAST_GM_Hippocampus_L"
names(brain_imaging_rawdata1)[names(brain_imaging_rawdata1) == "25887-2.0"] <- "f.25887_FAST_GM_Hippocampus_R"
names(brain_imaging_rawdata1)[names(brain_imaging_rawdata1) == "25139-2.0"] <- "f.25139_MD_Skeleton_cingulum_cingulate_gyrus_L"
names(brain_imaging_rawdata1)[names(brain_imaging_rawdata1) == "25138-2.0"] <- "f.25138_MD_Skeleton_cingulum_cingulate_gyrus_R"   
names(brain_imaging_rawdata1)[names(brain_imaging_rawdata1) == "25091-2.0"] <- "f.25091_FA_Skeleton_cingulum_cingulate_gyrus_L"
names(brain_imaging_rawdata1)[names(brain_imaging_rawdata1) == "25090-2.0"] <- "f.25090_FA_Skeleton_cingulum_cingulate_gyrus_R"
names(brain_imaging_rawdata1)[names(brain_imaging_rawdata1) == "25093-2.0"] <- "f.25093_FA_Skeleton_hippocampus_L"
names(brain_imaging_rawdata1)[names(brain_imaging_rawdata1) == "25092-2.0"] <- "f.25092_FA_Skeleton_hippocampus_R"
names(brain_imaging_rawdata1)[names(brain_imaging_rawdata1) == "25141-2.0"] <- "f.25141_MD_Skeleton_hippocampus_L"
names(brain_imaging_rawdata1)[names(brain_imaging_rawdata1) == "25140-2.0"] <- "f.25140_MD_Skeleton_hippocampus_R"
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
names(brain_imaging_rawdata1)[names(brain_imaging_rawdata1) == "25738-2.0"] <- "f.25738_Discrepancy_SWI+T1_brain image"
names(brain_imaging_rawdata1)[names(brain_imaging_rawdata1) == "25038-2.0"] <- "f.25038_MedianT2star_accumbens_L"
names(brain_imaging_rawdata1)[names(brain_imaging_rawdata1) == "25039-2.0"] <- "f.25039_MedianT2star_accumbens_R"
names(brain_imaging_rawdata1)[names(brain_imaging_rawdata1) == "25036-2.0"] <- "f.25036_MedianT2star_amygdala_L"
names(brain_imaging_rawdata1)[names(brain_imaging_rawdata1) == "25037-2.0"] <- "f.25037_MedianT2star_amygdala_R"
names(brain_imaging_rawdata1)[names(brain_imaging_rawdata1) == "25034-2.0"] <- "f.25034_MedianT2star_hippocampus_L"
names(brain_imaging_rawdata1)[names(brain_imaging_rawdata1) == "25035-2.0"] <- "f.25035_MedianT2star_hippocampus_R"
names(brain_imaging_rawdata1)[names(brain_imaging_rawdata1) == "25032-2.0"] <- "f.25032_MedianT2star_pallidum_L"
names(brain_imaging_rawdata1)[names(brain_imaging_rawdata1) == "25033-2.0"] <- "f.25033_MedianT2star_pallidum_R"
names(brain_imaging_rawdata1)[names(brain_imaging_rawdata1) == "25030-2.0"] <- "f.25030_MedianT2star_putamen_L"
names(brain_imaging_rawdata1)[names(brain_imaging_rawdata1) == "25031-2.0"] <- "f.25031_MedianT2star_putamen_R"
names(brain_imaging_rawdata1)[names(brain_imaging_rawdata1) == "25028-2.0"] <- "f.25028_MedianT2star_caudate_L"
names(brain_imaging_rawdata1)[names(brain_imaging_rawdata1) == "25029-2.0"] <- "f.25029_MedianT2star_caudate_R"
names(brain_imaging_rawdata1)[names(brain_imaging_rawdata1) == "25026-2.0"] <- "f.25026_MedianT2star_thalamus_L"
names(brain_imaging_rawdata1)[names(brain_imaging_rawdata1) == "25027-2.0"] <- "f.25027_MedianT2star_thalamus_R"

##########
# date of visit #
##########
names(brain_imaging_rawdata1)[names(brain_imaging_rawdata1) == "53-2.0"] <- "Date_of_visit"


##############################################################################
# extra brain imaging variables #
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
               "^25781\\-" , # white matter hyperintensity,
               
               "^26639\\-" ,	#Volume of Whole-hippocampal-body (left hemisphere)
               "^26661\\-" ,	#Volume of Whole-hippocampal-body (right hemisphere)
               "^26640\\-" ,	#Volume of Whole-hippocampal-head (left hemisphere)
               "^26662\\-" ,	#Volume of Whole-hippocampal-head (right hemisphere)
               "^26641\\-" ,	#Volume of Whole-hippocampus (left hemisphere)
               "^26663\\-" ,	#Volume of Whole-hippocampus (right hemisphere)
               "^26620\\-" ,	#Volume of Hippocampal-tail (left hemisphere)
               "^26642\\-" 	#Volume of Hippocampal-tail (right hemisphere)
               
               
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
names(brain_imaging_rawdata_WMH1)[names(brain_imaging_rawdata_WMH1) == "26639-2.0"] <- "f.26639_body_hippo_L"
names(brain_imaging_rawdata_WMH1)[names(brain_imaging_rawdata_WMH1) == "26661-2.0"] <- "f.26661_body_hippo_R"
names(brain_imaging_rawdata_WMH1)[names(brain_imaging_rawdata_WMH1) == "26640-2.0"] <- "f.26640_head_hippo_L"
names(brain_imaging_rawdata_WMH1)[names(brain_imaging_rawdata_WMH1) == "26662-2.0"] <- "f.26662_head_hippo_R"
names(brain_imaging_rawdata_WMH1)[names(brain_imaging_rawdata_WMH1) == "26641-2.0"] <- "f.26641_whole_hippo_L"
names(brain_imaging_rawdata_WMH1)[names(brain_imaging_rawdata_WMH1) == "26663-2.0"] <- "f.26663_whole_hippo_R"
names(brain_imaging_rawdata_WMH1)[names(brain_imaging_rawdata_WMH1) == "26620-2.0"] <- "f.26620_tail_hippo_L"
names(brain_imaging_rawdata_WMH1)[names(brain_imaging_rawdata_WMH1) == "26642-2.0"] <- "f.26642_tail_hippo_R"





# merge brain imaging data together
Brain_imaging_subset <- merge(brain_imaging_rawdata1, brain_imaging_rawdata_WMH1,  by = "eid")
Brain_imaging_subset <- merge(Brain_imaging_subset, scanner_position_brain_imaging,  by = "eid")


save(Brain_imaging_subset, file = paste(saving_directory, "brain_imaging_compiled_13122020.Rdata", sep=""))
