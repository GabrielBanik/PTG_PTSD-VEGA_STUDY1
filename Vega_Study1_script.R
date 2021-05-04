########################################################################
##############VEGA TRNAVA STUDY1 (data collection1)#####################
########################################################################

#load data
library(readr)
data <- read_csv("raw_data.csv")

#load packages 
library(tidyverse)
library(mice)
library(psych)
library(MBESS)
library(miceadds)
library(kableExtra)
library(MKmisc)
library(yarrr)
library(BayesFactor)

#data cleaning and wrangling

data <- data %>% 
        mutate(Sex = as.factor(Sex)) %>% #make categorical variables
        mutate(current_state = as.factor(current_state)) %>% 
        mutate(Education = as.factor(Education)) %>% 
        mutate(marital_status = as.factor(marital_status)) %>% 
        mutate(job = as.factor(job)) %>% 
        mutate(living = as.factor(living)) %>% 
        mutate(cancer_family_anamnesis = as.factor(cancer_family_anamnesis)) %>% 
        mutate(relaps_cancer = as.factor(relaps_cancer)) %>% 
        mutate(type_of_cancer = as.factor(type_of_cancer)) %>% 
        mutate(metastasis = as.factor(metastasis)) %>% 
        mutate(chemotherapy = as.factor(chemotherapy)) %>% 
        mutate(surgery = as.factor(surgery)) %>% 
        mutate(radiotherapy = as.factor(radiotherapy)) %>% 
        mutate(hormonal = as.factor(hormonal)) %>% 
        mutate(alternative = as.factor(alternative)) %>% 
        mutate(palliative = as.factor(palliative)) %>% 
        mutate(time_since_dg_categories = as.factor(time_since_dg_categories))

#inspect categories
table(data$Sex)
table(data$current_state)
table(data$Education)
table(data$marital_status)
table(data$job)
table(data$living)
table(data$cancer_family_anamnesis)
table(data$relaps_cancer)
table(data$type_of_cancer)
table(data$metastasis)
table(data$chemotherapy)
table(data$surgery)
table(data$radiotherapy)
table(data$hormonal)
table(data$alternative)
table(data$palliative)
table(data$time_since_dg_categories)

#rename values in categorical variables
data <- data %>% 
        mutate_at("Sex", funs(recode(., `žena`="female", `1`="female", `muž`="male", `0`="male"))) %>% 
        mutate_at("current_state", funs(recode(., `pacient v remisii (bez príznakov)`="remission", `2`="remission", `pacient v liečbe`="treatment", `1`="treatment"))) %>% 
        mutate_at("Education", funs(recode(., `stredoškolské`="high school", `VOŠ`="university", `vysokoškolské`="university", `základné`="elementary"))) %>% 
        mutate_at("marital_status", funs(recode(.,`1`="single", `slobodný/á`="single", `2`="married",`ženatý/vydatá`="married", `3`="divorced", `rozvedený/á`="divorced", `4`="widowed", `vdovec/vdova`="widowed"))) %>%
        mutate_at("job", funs(recode(., `invalidný dôchodok`="retired(invalidity)", `materská dovolenka`="maternity leave", `nezamestnaný/á`="unemployed",`starobný dôchodok`="retired", `zamestnaný/á`="employed"))) %>% 
        mutate_at("living", funs(recode(., `krajské/hlavné mesto`="big city", `mesto`="city", `vidiek`="village"))) %>% 
        mutate_at("type_of_cancer", funs(recode(., `Nádory dýchacích a vnútrohrudníkových orgánov`="lung cancer", `10`="lung cancer", 
                                                `Nádory endokrinných žliaz`="tumors of endocrine glands", `4`="tumors of endocrine glands",
                                                `Nádory GIT (žalúdok, tenké črevo, hrubé črevo, pankreas, pečeň, konečník)`= "gastrointestinal cancer (stomach, intestine, pancreas, liver, rectum)",
                                                `2`="gastrointestinal cancer (stomach, intestine, pancreas, liver, rectum)",
                                                `Nádory hlavy a iných častí CNS`="brain, head, and neck cancer", `9`="brain, head, and neck cancer", 
                                                `Nádory lymfatického a krvotvorného tkaniva`="hematological cancer (leukemia, lymphoma)",
                                                `3`="hematological cancer (leukemia, lymphoma)",
                                                `Nádory močovej sústavy`="urinary cancer",`5`="urinary cancer", `Nádory neurčitého alebo neznámeho správania`="tumors of indeterminate or unknown behavior",
                                                `8`="tumors of indeterminate or unknown behavior",
                                                `Nádory pery, ústnej dutiny, hltana`="tumors of the lips, oral cavity, or pharynx", `11`="tumors of the lips, oral cavity, or pharynx", 
                                                `Nádory prsníka`="breast cancer", `1`="breast cancer", 
                                                `Nádory ženských pohlavných orgánov`="ovaria cancer", `6`="ovaria cancer", `Viacnásobný primárny nádor`="multiple primary tumor", `12`="multiple primary tumor",
                                                `Nádory mužských pohlavných orgánov`="testicular cancer", `7`="testicular cancer"))) %>% 
        mutate_at("metastasis", funs(recode(., `rozšírený`="yes", `lokálny`="no"))) %>% 
        mutate_at("time_since_dg_categories", funs(recode(.,`6 mesiacov až 1 rok`="6 months to 1 year", `1`="6 months to 1 year", `2`="from 1 year to 2 years",
                                                          `viac než rok až 2 roky`="from 1 year to 2 years", `viac než 2 roky až 5 rokov`="2 to 5 years", `3`="2 to 5 years",
                                                          `viac než 5 rokov`="more than 5 years", `4`="more than 5 years"))) %>% 
        mutate_at(c("cancer_family_anamnesis", "relaps_cancer", "chemotherapy", "surgery", "radiotherapy", "hormonal", "alternative", "palliative"), funs(recode(., `áno`="yes", `nie`="no", `1`="yes", `2`="no", `neviem`="do not know")))


#delete mixed variable - type of cancer treatment (more than one category in one row) - it is redundant because type of treatment is in to 6 separate columns
data[,22] <- NULL

#control for unuseal values in numeric variables
table(data$child_number)
table(data$time_since_diagnosis) #40969 is probably typo because it is not possible to be ill 40969 months
print(data[5,11])
data[5,11] <- NA #errores number will be handle as missing
table(data$hospitalisation_number)
table(data$pain)
table(data$discomfort)
table(data$social_isolation)
table(data$anxiety_fear)
table(data$sadness_depression)
table(data$lost_of_meaning)
lengths(lapply(data[,28:55], unique)) #mini-mac
table(data$Minimac28) #0 is probably typo
print(data[98,55])
data[98,55] <- NA #errores number will be handle as missing
lengths(lapply(data[,56:61], unique)) #resilience
lengths(lapply(data[,62:71], unique))
table(data$Religiozita_1)
data <- data %>% 
  mutate_at("Religiozita_1", funs(recode(.,`veľmi často`=1, `často`=2, `príležitostne`=3, `zriedkakedy`=4, `nikdy`=5, `1`=1, `2`=2, `3`=3, `4`=4, `5`=5)))
table(data$Religiozita_2)
data <- data %>% 
  mutate_at("Religiozita_2", funs(recode(.,`úplne`=1, `prevažne`=2, `stredne`=3, `málo`=4, `vôbec`=5, `1`=1, `2`=2, `3`=3, `4`=4, `5`=5)))
table(data$Religiozita_3)
data <- data %>% 
  mutate_at("Religiozita_3", funs(recode(.,`viac ako raz do týždňa`=1, `raz do týždňa`=2, `jeden- až trikrát do mesiaca`=3, `zopárkrát do roka`=4, `menej často`=5, `nikdy`=6, `1`=1, `2`=2, `3`=3, `5`=5)))
table(data$Religiozita_4)
data <- data %>% 
  mutate_at("Religiozita_4", funs(recode(.,`niekoľkokrát do dňa`=1, `jedenkrát denne`=2, `viac ako raz za týždeň`=3, `raz za týždeň`=4, `jeden- až trikrát do mesiaca`=5, `zriedka`=6, `nikdy`=7, `1`=1, `2`=2, `3`=3, `4`=4, `5`=5)))
table(data$Religiozita_5)
data <- data %>% 
  mutate_at("Religiozita_5", funs(recode(.,`veľmi často`=1, `často`=2, `príležitostne`=3, `zriedkakedy`=4, `nikdy`=5, `1`=1, `2`=2, `3`=3, `4`=4, `5`=5)))
lengths(lapply(data[,77:92], unique))
lengths(lapply(data[,93:112], unique)) 
lengths(lapply(data[,114:123], unique)) #PTGI
sum(is.na(data$PTGI_1))

#####missing values
sum(is.na(data))/prod(dim(data))
colMeans(is.na(data))

#imputation of of one item from MINI-MAC which was omit in first data collection by technical mistake, according to pattern of MINI-MAC, PTGI, RESILIENCE from second data collection

#dataset2 - pattern for imputation

dataset2 <- read_csv("dataset2.csv")

#imputation of of one item from MINI-MAC which was omit in first data collection by technical mistake, according to pattern of MINI-MAC, PTGI, RESILIENCE from second data collection

data_MM_imp <- data[,c(28:55,56:61,114:123)]
data_MM_imp$Minimac29 <- NA

data_MM_imp <- data_MM_imp[,c("Minimac1", "Minimac2", "Minimac3", "Minimac4", "Minimac5", "Minimac6", 
                                    "Minimac7", "Minimac8", "Minimac9", "Minimac10", "Minimac11", "Minimac12", 
                                    "Minimac13", "Minimac14", "Minimac15", "Minimac16", 
                                    "Minimac17", "Minimac18", "Minimac19", "Minimac20", "Minimac21", 
                                    "Minimac22", "Minimac23", "Minimac24", "Minimac25", 
                                    "Minimac26", "Minimac27", "Minimac28", "Minimac29", "Reziliencia1", 
                                    "Reziliencia2", "Reziliencia3", "Reziliencia4", "Reziliencia5", "Reziliencia6",
                                    "PTGI_1", "PTGI_2", "PTGI_3", "PTGI_4", "PTGI_5", "PTGI_6", "PTGI_7", "PTGI_8",
                                    "PTGI_9", "PTGI_10")]

####imputation (MICE)
dataset_for_MM_imp <- rbind(data_MM_imp, dataset2)

#missing values in dataset extracted for Minimac29 imputation
sum(is.na(dataset_for_MM_imp))/prod(dim(dataset_for_MM_imp))
colMeans(is.na(dataset_for_MM_imp))

set.seed(333)
dataset_imputed <- mice(dataset_for_MM_imp, m=15,maxit=10, meth ='pmm') #imputation of missing data [Minimac29]

#creating Minimac29 mean from all imputed datasets and adding Minimac29 to dataset for study1
dataset_imputed_complete_all <- mice::complete(dataset_imputed, "long")
imput1 <- dataset_imputed_complete_all[1:126,31]
imput2 <- dataset_imputed_complete_all[291:416,31]
imput3 <- dataset_imputed_complete_all[581:706,31]
imput4 <- dataset_imputed_complete_all[871:996,31]
imput5 <- dataset_imputed_complete_all[1161:1286,31]
imput6 <- dataset_imputed_complete_all[1451:1576,31]
imput7 <- dataset_imputed_complete_all[1741:1866,31]
imput8 <- dataset_imputed_complete_all[2031:2156,31]
imput9 <- dataset_imputed_complete_all[2321:2446,31]
imput10 <- dataset_imputed_complete_all[2611:2736,31]
imput11 <- dataset_imputed_complete_all[2901:3026,31]
imput12 <- dataset_imputed_complete_all[3191:3316,31]
imput13 <- dataset_imputed_complete_all[3481:3606,31]
imput14 <- dataset_imputed_complete_all[3771:3896,31]
imput15 <- dataset_imputed_complete_all[4061:4186,31]

data$Minimac29 <- round((imput1 + imput2 + imput3 + imput4 + imput5 + imput6 + imput7 + imput8 + imput9 + imput10 + imput11 + imput12 + imput13 + imput14 + imput15)/15, digits = 0)

#missing data in dataset for study1
sum(is.na(data))/prod(dim(data))
colMeans(is.na(data)) #missing values are in: pain, discomfort, social_isolation, anxiety_fear, sadness_depression, lost_of_meaning, Minimac28, meaning1:meaning10, PCL9:PCL20, PTGI1

###recoding items
#recoding the MINI-MAC items (because values was admistrated in reverse order)

data <- data %>% 
  mutate_at(c("Minimac1", "Minimac2", "Minimac3", "Minimac4", "Minimac5", "Minimac6", 
              "Minimac7", "Minimac8", "Minimac9", "Minimac10", "Minimac11", "Minimac12", 
              "Minimac13", "Minimac14", "Minimac15", "Minimac16", 
              "Minimac17", "Minimac18", "Minimac19", "Minimac20", "Minimac21", 
              "Minimac22", "Minimac23", "Minimac24", "Minimac25", 
              "Minimac26", "Minimac27", "Minimac28", "Minimac29"), funs(recode(., `1`=4, `2`=3, `3`=2, `4`=1)))

#recoding Resilience items

data <- data %>% 
  mutate_at (c("Reziliencia2", "Reziliencia4", "Reziliencia6"), funs(recode(., `1`=5, `2`=4, `3`=3, `4`=2, `5`=1)))

#recoding Integration of stressful events item nb. 2
data <- data %>% 
  mutate_at ("ISLES2", funs(recode(., `1`=5, `2`=4, `3`=3, `4`=2, `5`=1)))

#recoding religiosity items (change to higher level of item means higher level of religiosity domain)
data <- data %>% 
  mutate_at("Religiozita_1", funs(recode(., `1`=5, `2`=4, `3`=3, `4`=2, `5`=1)))
table(data$Religiozita_2)
data <- data %>% 
  mutate_at("Religiozita_2", funs(recode(., `1`=5, `2`=4, `3`=3, `4`=2, `5`=1)))
table(data$Religiozita_3)
data <- data %>% 
  mutate_at("Religiozita_3", funs(recode(.,`1`=6, `2`=5, `3`=4, `4`=3, `5`=2, `6`=1)))
table(data$Religiozita_4)
data <- data %>% 
  mutate_at("Religiozita_4", funs(recode(., `1`=7, `2`=6, `3`=5, `4`=4, `5`=3, `6`=2, `7`=1)))
table(data$Religiozita_5)
data <- data %>% 
  mutate_at("Religiozita_5", funs(recode(., `1`=5, `2`=4, `3`=3, `4`=2, `5`=1)))

#religiosity rename factors

data <- data %>% 
  rename(Religiosity_Intellect = Religiozita_1, Religiosity_Ideology = Religiozita_2, 
         Religiosity_Public_practice = Religiozita_3, Religiosity_Private_practice = Religiozita_4,
         Religiosity_Experience = Religiozita_5) 

#object MICE
init <-   mice(data, maxit=0) 
meth <-   init$method
predM <-   init$predictorMatrix

predM[,c("ID", "Age", "current_state", "Education", "child_number", "job", "living", "cancer_family_anamnesis",
         "time_since_diagnosis", "relaps_cancer", "hospitalisation_number", "type_of_cancer", "metastasis", "chemotherapy", "surgery",
         "radiotherapy", "hormonal", "alternative", "palliative", "Religiosity_Intellect", "Religiosity_Ideology", 
         "Religiosity_Public_practice", "Religiosity_Private_practice", 
         "Religiosity_Experience", "time_since_dg_categories")] <-  0 #exclusion from the prediction
predM[c("ID", "Age", "current_state", "Education", "child_number", "job", "living", "cancer_family_anamnesis",
         "time_since_diagnosis", "relaps_cancer", "hospitalisation_number", "type_of_cancer", "metastasis", "chemotherapy", "surgery",
         "radiotherapy", "hormonal", "alternative", "palliative", "Religiosity_Intellect", "Religiosity_Ideology", 
        "Religiosity_Public_practice", "Religiosity_Private_practice", 
         "Religiosity_Experience", "time_since_dg_categories"),] <- 0 #exclusion from the prediction

meth[c("Age", "Education", "child_number", "job", "living", "cancer_family_anamnesis", 
       "time_since_diagnosis", "relaps_cancer", "hospitalisation_number", "metastasis", "time_since_dg_categories")] <-  "" #exclusion from the imputation


set.seed(123)
data_imp <- mice(data, method=meth, predictorMatrix=predM, m=25, maxit = 10) #imputation of missing data - imputed object for the analysis

#convert mice object to list
dat <- miceadds::mids2datlist(data_imp)

#####################################################################################################
###### next part is applied to basic analysis to list (25 imputed datasets) created from MICE #######
#####################################################################################################

###### if there was no missing values, the basic analysis (descriptives, reliability, etc.) is carried out on original dataset (data)
###### total score is also compute in list because correlation and regression will be carried out on list

### total score and subscales 
#total PTSD
dat <- lapply(dat, function(x){cbind(x, PTSD = rowSums(x[,c("PCL_1", "PCL_2", "PCL_3", "PCL_4",
                                                     "PCL_5", "PCL_6", "PCL_7", "PCL_8",
                                                     "PCL_9", "PCL_10", "PCL_11", "PCL_12",
                                                     "PCL_13", "PCL_14", "PCL_15", "PCL_16",
                                                     "PCL_17", "PCL_18", "PCL_19", "PCL_20")], na.rm = TRUE))})
#criterionB
data$criterionB <- with(data, PCL_1 + PCL_2 + PCL_3 + PCL_4 + PCL_5)

dat <- lapply(dat, function(x){cbind(x, criterionB = rowSums(x[,c("PCL_1", "PCL_2", "PCL_3", "PCL_4", "PCL_5")], na.rm = TRUE))})

#criterionC
data$criterionC <- data$PCL_6 + data$PCL_7

dat <- lapply(dat, function(x){cbind(x, criterionC = rowSums(x[,c("PCL_7", "PCL_8")], na.rm = TRUE))})

#criterionD
dat <- lapply(dat, function(x){cbind(x, criterionD = rowSums(x[,c("PCL_8","PCL_9", "PCL_10", "PCL_11", "PCL_12", "PCL_13", "PCL_14")], na.rm = TRUE))})

#criterionE
dat <- lapply(dat, function(x){cbind(x, criterionE = rowSums(x[,c("PCL_15","PCL_16", "PCL_17", "PCL_18", "PCL_19", "PCL_20")], na.rm = TRUE))})


#posttraumatic growth
dat <- lapply(dat, function(x){cbind(x, PTG = rowSums(x[,c("PTGI_1", "PTGI_2", "PTGI_3", "PTGI_4", "PTGI_5", "PTGI_6", "PTGI_7", 
                                                           "PTGI_8", "PTGI_9", "PTGI_10")], na.rm = TRUE))})

#Resilience
data$REZIL <- with(data, Reziliencia1 + Reziliencia2 + Reziliencia3 + Reziliencia4 + Reziliencia5 + Reziliencia6)

dat <- lapply(dat, function(x){cbind(x, REZIL = rowSums(x[,c("Reziliencia1", "Reziliencia2", "Reziliencia3", "Reziliencia4",
                                                             "Reziliencia5", "Reziliencia6")], na.rm = TRUE))})

#Integration of stressful events
data$ISLESFiW <- with(data, ISLES1 + ISLES3 + ISLES5 + ISLES7 + ISLES9 + ISLES11 + ISLES12 + ISLES13 + ISLES14 + ISLES15 + ISLES16)

dat <- lapply(dat, function(x){cbind(x, ISLESFiW = rowSums(x[,c("ISLES1", "ISLES3", "ISLES5", "ISLES7",
                                                             "ISLES9", "ISLES11", "ISLES12", "ISLES13", "ISLES14", "ISLES15", 
                                                             "ISLES16")], na.rm = TRUE))})

data$ISLESComp <- with(data, ISLES2 + ISLES4 + ISLES6 + ISLES8 + ISLES10)

dat <- lapply(dat, function(x){cbind(x, ISLESComp = rowSums(x[,c("ISLES2", "ISLES4", "ISLES6", "ISLES8", "ISLES10")], na.rm = TRUE))})

#Mental adjustment to cancer
data$MMHHless <- with(data, Minimac1 + Minimac2 + Minimac4 + Minimac5 + Minimac7 + Minimac8 + Minimac14 + Minimac19 + Minimac25)

dat <- lapply(dat, function(x){cbind(x, MMHHless = rowSums(x[,c("Minimac1", "Minimac2", "Minimac4", "Minimac5",
                                                                "Minimac7", "Minimac8", "Minimac14", "Minimac19", "Minimac25")], na.rm = TRUE))})

data$MMAnx <- with(data, Minimac9 + Minimac10 + Minimac16 + Minimac19 + Minimac20 + Minimac21 + Minimac22 + Minimac29)

dat <- lapply(dat, function(x){cbind(x, MMAnx = rowSums(x[,c("Minimac9", "Minimac10", "Minimac16", "Minimac19",
                                                                "Minimac20", "Minimac21", "Minimac22", "Minimac29")], na.rm = TRUE))})

data$MMFight <- with(data, Minimac3 + Minimac6 + Minimac12 + Minimac13 + Minimac17) 

dat <- lapply(dat, function(x){cbind(x, MMFight = rowSums(x[,c("Minimac3", "Minimac6", "Minimac12", "Minimac13", "Minimac17")], na.rm = TRUE))})

dat <- lapply(dat, function(x){cbind(x, MMFatal = rowSums(x[,c("Minimac11", "Minimac24", "Minimac27", "Minimac28")], na.rm = TRUE))})

data$MMCogAvoid <- data$Minimac15 + data$Minimac18 + data$Minimac23 + data$Minimac26

dat <- lapply(dat, function(x){cbind(x, MMCogAvoid = rowSums(x[,c("Minimac15", "Minimac18", "Minimac23", "Minimac26")], na.rm = TRUE))})

#self-transcendence
dat <- lapply(dat, function(x){cbind(x, Self_transcend = rowSums(x[, c("meaning1", "meaning2", "meaning3", "meaning4", "meaning5",   
                                                                       "meaning6", "meaning7", "meaning8", "meaning9", "meaning10")], na.rm = TRUE))})

#religiosity
dat <- lapply(dat, function(x){cbind(x, Religiosity = rowSums(x[, c("Religiosity_Intellect", 
                                                                    "Religiosity_Ideology", 
                                                                    "Religiosity_Public_practice", 
                                                                    "Religiosity_Private_practice", 
                                                                    "Religiosity_Experience")], na.rm = TRUE))})

#new variable (categories and recode education into ordered)       
dat <- dat %>% 
  map(~ .x %>% mutate_at("Education", funs(recode(., `high school`=1, `university`=2, `elementary`=0))))
  
dat <- dat %>% 
  map(~ .x %>% mutate(Education = as.numeric(Education)))

dat <- dat %>% 
  map(~ .x %>% mutate(relationship = case_when(marital_status == "single" ~ "no relationship",
                                  marital_status == "divorced" ~ "no relationship",
                                  marital_status == "widowed" ~ "no relationship",
                                  marital_status == "married" ~ "in relationship")))
                                                                                                                                            
#######transform list back to MICE object
data_imp <- miceadds::datlist2mids(dat)
#complete all imputed dataset after lapply
imp_complete <- mice::complete(data_imp, "long")

#####reliability

#PTSD
lapply(dat, function(x){ci.reliability(x[,c("PCL_1", "PCL_2", "PCL_3", "PCL_4",
                                               "PCL_5", "PCL_6", "PCL_7", "PCL_8",
                                               "PCL_9", "PCL_10", "PCL_11", "PCL_12",
                                               "PCL_13", "PCL_14", "PCL_15", "PCL_16",
                                               "PCL_17", "PCL_18", "PCL_19", "PCL_20")], type = "omega")})
##criterionB
criterionB <- data[,c("PCL_1", "PCL_2", "PCL_3", "PCL_4", "PCL_5")]
ci.reliability(criterionB, type = "omega")
##criterionC
criterionC <- data[,c("PCL_6", "PCL_7")]
cor(criterionC, method = "spearman")
#criterionD
lapply(dat, function(x){ci.reliability(x[,c("PCL_8","PCL_9", "PCL_10", "PCL_11", "PCL_12", "PCL_13", "PCL_14")], type = "omega")})
#criterionE
lapply(dat, function(x){ci.reliability(x[,c("PCL_15","PCL_16", "PCL_17", "PCL_18", "PCL_19", "PCL_20")], type = "omega")})

#resilience
RESIL <- data[,83:88]
ci.reliability(RESIL, type = "omega")

#integration of stressful events
##footing in the world
ISLESFiW <- subset(data, select = c("ISLES1", "ISLES3", "ISLES5", "ISLES7", "ISLES9", "ISLES11", "ISLES12", "ISLES13", 
                           "ISLES14", "ISLES15", "ISLES16"))
ci.reliability(ISLESFiW, type = "omega")
##comprehensibility
ISLESComp <- subset(data, select = c("ISLES2", "ISLES4", "ISLES6", "ISLES8", "ISLES10"))
ci.reliability(ISLESComp, type = "omega")

#posttraumatic growth
lapply(dat, function(x){ci.reliability(x[,c("PTGI_1","PTGI_2", "PTGI_3", "PTGI_4", "PTGI_5", "PTGI_6", "PTGI_7", "PTGI_8", "PTGI_9",
                                            "PTGI_10")], type = "omega")})

#mini-mac
##hopelessness - helplessness
MMHHless <- subset(data, select = c("Minimac1", "Minimac2", "Minimac4", "Minimac5", "Minimac7", "Minimac8", 
                                    "Minimac14", "Minimac19", "Minimac25"))
ci.reliability(MMHHless, type = "omega")
##anxious preocupation
MMAnx <- subset(data, select = c("Minimac9", "Minimac10", "Minimac16", "Minimac19", "Minimac20", "Minimac21", 
                                 "Minimac22", "Minimac29"))
ci.reliability(MMAnx, type = "omega")
##figthing spirit
MMFight <- subset(data, select = c("Minimac3", "Minimac6", "Minimac12", "Minimac13", "Minimac17"))
ci.reliability(MMFight, type = "omega")
##fatalism
lapply(dat, function(x){ci.reliability(x[,c("Minimac11", "Minimac24", "Minimac27", "Minimac28")], type = "omega")})
##cognitive avoidance
MMCogAvoid <- subset(data, select = c("Minimac15", "Minimac18", "Minimac23", "Minimac26"))
ci.reliability(MMCogAvoid, type = "omega")

#self-transcendence
lapply(dat, function(x){ci.reliability(x[,c("meaning1", "meaning2", "meaning3", "meaning4", "meaning5", 
                                            "meaning6", "meaning7", "meaning8", "meaning9", "meaning10")], type = "omega")})

#####Cronbach's alpha
lapply(dat, function(x){alpha(x[,c("PCL_1", "PCL_2", "PCL_3", "PCL_4",                 #PTSD
                                            "PCL_5", "PCL_6", "PCL_7", "PCL_8",
                                            "PCL_9", "PCL_10", "PCL_11", "PCL_12",
                                            "PCL_13", "PCL_14", "PCL_15", "PCL_16",
                                            "PCL_17", "PCL_18", "PCL_19", "PCL_20")])})
alpha(criterionB)
alpha(criterionC)
lapply(dat, function(x){alpha(x[,c("PCL_8","PCL_9", "PCL_10", "PCL_11", "PCL_12", "PCL_13", "PCL_14")])}) #criterionD
lapply(dat, function(x){alpha(x[,c("PCL_15","PCL_16", "PCL_17", "PCL_18", "PCL_19", "PCL_20")])}) #criterionE
lapply(dat, function(x){alpha(x[,c("PTGI_1","PTGI_2", "PTGI_3", "PTGI_4", "PTGI_5", "PTGI_6", "PTGI_7", "PTGI_8", "PTGI_9", #PTG
                                            "PTGI_10")])})
alpha(ISLESFiW)
alpha(ISLESComp)
alpha(RESIL)
alpha(MMHHless)
alpha(MMAnx)
alpha(MMFight)
lapply(dat, function(x){alpha(x[,c("Minimac11", "Minimac24", "Minimac27", "Minimac28")])}) #fatalism
alpha(MMCogAvoid)
lapply(dat, function(x){alpha(x[,c("meaning1", "meaning2", "meaning3", "meaning4", "meaning5",
                                            "meaning6", "meaning7", "meaning8", "meaning9", "meaning10")])}) #self-transcendence
####### descriptives #######

lapply(dat, function(x){mean(x[,c("PTSD")])})
lapply(dat, function(x){sd(x[,c("PTSD")])})

#PTSD
PTSD_descriptive <- with(data_imp, expr=c("PTSD(mean)"=mean(PTSD), "PTSD(SD)"=stats::sd(PTSD), "PTSD(S.E)"=sd(PTSD)/sqrt(length(PTSD))))
# pool estimates
withPool_MI(PTSD_descriptive)

#criterionB
describe(data$criterionB)
#criterionC
describe(data$criterionC)

#criterionD
criterionD_descriptive <- with(imp, expr=c("criterionD(mean)"=mean(criterionD), "criterionD(SD)"=stats::sd(criterionD), 
                                           "criterionD(S.E)"=sd(criterionD)/sqrt(length(criterionD))))
# pool estimates
withPool_MI(criterionD_descriptive)

#criterionE
criterionE_descriptive <- with(imp, expr=c("criterionE(mean)"=mean(criterionE), "criterionE(SD)"=stats::sd(criterionE), 
                                           "criterionE(S.E)"=sd(criterionE)/sqrt(length(criterionE))))
# pool estimates
withPool_MI(criterionE_descriptive)

#resilience
describe(data$REZIL)

#integration of stressful events
##footing in the world
describe(data$ISLESFiW)
##comprehensibility
describe(data$ISLESComp)

#posttraumatic growth
PTG_descriptive <- with(imp, expr=c("PTG(mean)"=mean(PTG), "PTG(SD)"=stats::sd(PTG), "PTG(S.E)"=sd(PTG)/sqrt(length(PTG))))
# pool estimates
withPool_MI(PTG_descriptive)

#mini-mac
##hopelessness - helplessness
describe(data$MMHHless)
##anxious preocupation
describe(data$MMAnx)
##figthing spirit
describe(data$MMFight)
##fatalism
MMFatal_descriptive <- with(imp, expr=c("MMFatal(mean)"=mean(MMFatal), "MMFatal(SD)"=stats::sd(MMFatal), 
                                        "MMFatal(S.E)"=sd(MMFatal)/sqrt(length(MMFatal))))
# pool estimates
withPool_MI(MMFatal_descriptive)

##cognitive avoidance
describe(data$MMCogAvoid)

#self-transcendence
Self_transcend_descriptive <- with(imp, expr=c("Self_transcend(mean)"=mean(Self_transcend), "Self_transcend(SD)"=stats::sd(Self_transcend), 
                                               "Self_transcend(S.E)"=sd(Self_transcend)/sqrt(length(Self_transcend))))
# pool estimates
withPool_MI(Self_transcend_descriptive)

#self-transcendence
Religiosity_descriptive <- with(imp, expr=c("Religiosity(mean)"=mean(Religiosity), "Religiosity(SD)"=stats::sd(Religiosity), 
                                               "Religiosity(S.E)"=sd(Religiosity)/sqrt(length(Religiosity))))
# pool estimates
withPool_MI(Religiosity_descriptive)

#pain
describe(data$pain)
#discomfort
describe(data$discomfort)
#social_isolation
describe(data$social_isolation)
#anxiety_fear
describe(data$anxiety_fear)
#sadness_depression
describe(data$sadness_depression)
#lost_of_meaining
describe(data$lost_of_meaning)

#religosity(intellect)
describe(data$Religiosity_Intellect)
#religiosity(ideology)
describe(data$Religiosity_Ideology)
#religiosity(public practice)
describe(data$Religiosity_Public_practice)
#religiosity(private practice)
describe(data$Religiosity_Private_practice)
#religiosity(experience)
describe(data$Religiosity_Experience)

##### description of sample #####
as.data.frame(table(data$Sex)) %>% rename(Count=1,Freq=2) %>% mutate(Perc=100*Freq/sum(Freq))
as.data.frame(table(data$current_state)) %>% rename(Count=1,Freq=2) %>% mutate(Perc=100*Freq/sum(Freq))
as.data.frame(table(data$Education)) %>% rename(Count=1,Freq=2) %>% mutate(Perc=100*Freq/sum(Freq))
as.data.frame(table(data$marital_status)) %>% rename(Count=1,Freq=2) %>% mutate(Perc=100*Freq/sum(Freq))
as.data.frame(table(data$job)) %>% rename(Count=1,Freq=2) %>% mutate(Perc=100*Freq/sum(Freq))
as.data.frame(table(data$living)) %>% rename(Count=1,Freq=2) %>% mutate(Perc=100*Freq/sum(Freq))
as.data.frame(table(data$cancer_family_anamnesis)) %>% rename(Count=1,Freq=2) %>% mutate(Perc=100*Freq/sum(Freq))
as.data.frame(table(data$relaps_cancer)) %>% rename(Count=1,Freq=2) %>% mutate(Perc=100*Freq/sum(Freq))
as.data.frame(table(data$type_of_cancer)) %>% rename(Count=1,Freq=2) %>% mutate(Perc=100*Freq/sum(Freq))
as.data.frame(table(data$time_since_dg_categories)) %>% rename(Count=1,Freq=2) %>% mutate(Perc=100*Freq/sum(Freq))
as.data.frame(table(data$chemotherapy)) %>% rename(Count=1,Freq=2) %>% mutate(Perc=100*Freq/sum(Freq))
as.data.frame(table(data$surgery)) %>% rename(Count=1,Freq=2) %>% mutate(Perc=100*Freq/sum(Freq))
as.data.frame(table(data$radiotherapy)) %>% rename(Count=1,Freq=2) %>% mutate(Perc=100*Freq/sum(Freq))
as.data.frame(table(data$hormonal)) %>% rename(Count=1,Freq=2) %>% mutate(Perc=100*Freq/sum(Freq))
as.data.frame(table(data$alternative)) %>% rename(Count=1,Freq=2) %>% mutate(Perc=100*Freq/sum(Freq))
as.data.frame(table(data$palliative)) %>% rename(Count=1,Freq=2) %>% mutate(Perc=100*Freq/sum(Freq))
as.data.frame(table(data$metastasis)) %>% rename(Count=1,Freq=2) %>% mutate(Perc=100*Freq/sum(Freq))

#age, time from diagnosis (in months), number of hospitalisation, numebr of children
describe(data$Age)
describe(data$time_since_diagnosis)
describe(data$hospitalisation_number)
describe(data$child_number)

####PTG a PTSD relationship (curvilinear regression)

#average values for PTG and PTSD from imputed dataset 
#PTG
imput1 <- imp_complete[1:126, "PTG"]
imput2 <- imp_complete[127:252, "PTG"]
imput3 <- imp_complete[253:378, "PTG"]
imput4 <- imp_complete[379:504, "PTG"]
imput5 <- imp_complete[505:630, "PTG"]
imput6 <- imp_complete[631:756, "PTG"]
imput7 <- imp_complete[757:882, "PTG"]
imput8 <- imp_complete[883:1008, "PTG"]
imput9 <- imp_complete[1009:1134, "PTG"]
imput10 <- imp_complete[1135:1260, "PTG"]
imput11 <- imp_complete[1261:1386, "PTG"]
imput12 <- imp_complete[1387:1512, "PTG"]
imput13 <- imp_complete[1513:1638, "PTG"]
imput14 <- imp_complete[1639:1764, "PTG"]
imput15 <- imp_complete[1765:1890, "PTG"]
imput16 <- imp_complete[1891:2016, "PTG"]
imput17 <- imp_complete[2017:2142, "PTG"]
imput18 <- imp_complete[2143:2268, "PTG"]
imput19 <- imp_complete[2269:2394, "PTG"]
imput20 <- imp_complete[2395:2520, "PTG"]
imput21 <- imp_complete[2521:2646, "PTG"]
imput22 <- imp_complete[2647:2772, "PTG"]
imput23 <- imp_complete[2773:2898, "PTG"]
imput24 <- imp_complete[2899:3024, "PTG"]
imput25 <- imp_complete[3025:3150, "PTG"]

PTG_avrg <- round((imput1 + imput2 + imput3 + imput4 + imput5 + imput6 + imput7 + imput8 + imput9 + imput10 + imput11 + imput12 + imput13 + 
                     imput14 + imput15 + imput16 + imput17 + imput18 + imput19 + imput20 + imput21 + imput22 + imput23 +
                     imput24 + imput25)/25, digits = 0)

#PTSD
imput1 <- imp_complete[1:126, "PTSD"]
imput2 <- imp_complete[127:252, "PTSD"]
imput3 <- imp_complete[253:378, "PTSD"]
imput4 <- imp_complete[379:504, "PTSD"]
imput5 <- imp_complete[505:630, "PTSD"]
imput6 <- imp_complete[631:756, "PTSD"]
imput7 <- imp_complete[757:882, "PTSD"]
imput8 <- imp_complete[883:1008, "PTSD"]
imput9 <- imp_complete[1009:1134, "PTSD"]
imput10 <- imp_complete[1135:1260, "PTSD"]
imput11 <- imp_complete[1261:1386, "PTSD"]
imput12 <- imp_complete[1387:1512, "PTSD"]
imput13 <- imp_complete[1513:1638, "PTSD"]
imput14 <- imp_complete[1639:1764, "PTSD"]
imput15 <- imp_complete[1765:1890, "PTSD"]
imput16 <- imp_complete[1891:2016, "PTSD"]
imput17 <- imp_complete[2017:2142, "PTSD"]
imput18 <- imp_complete[2143:2268, "PTSD"]
imput19 <- imp_complete[2269:2394, "PTSD"]
imput20 <- imp_complete[2395:2520, "PTSD"]
imput21 <- imp_complete[2521:2646, "PTSD"]
imput22 <- imp_complete[2647:2772, "PTSD"]
imput23 <- imp_complete[2773:2898, "PTSD"]
imput24 <- imp_complete[2899:3024, "PTSD"]
imput25 <- imp_complete[3025:3150, "PTSD"]

PTSD_avrg <- round((imput1 + imput2 + imput3 + imput4 + imput5 + imput6 + imput7 + imput8 + imput9 + imput10 + imput11 + imput12 + imput13 + 
                     imput14 + imput15 + imput16 + imput17 + imput18 + imput19 + imput20 + imput21 + imput22 + imput23 +
                     imput24 + imput25)/25, digits = 0)

PTG_PTSD <- tibble(PTG_avrg, PTSD_avrg)
is.numeric(PTG_PTSD$PTG_avrg)
#create quadratic variable - PTSD
PTG_PTSD$PTSD_avrg1 <- PTG_PTSD$PTSD_avrg*PTG_PTSD$PTSD_avrg
PTG_PTSD$PTSD_avrg2 <- PTG_PTSD$PTSD_avrg*PTG_PTSD$PTSD_avrg*PTG_PTSD$PTSD_avrg
PTG_PTSD$PTSD_avrg3 <- PTG_PTSD$PTSD_avrg*PTG_PTSD$PTSD_avrg*PTG_PTSD$PTSD_avrg*PTG_PTSD$PTSD_avrg

#comparing models for curvilinear regression
model1 <- lm(PTG_avrg~PTSD_avrg, PTG_PTSD)
AIC(model1)
model2 <- lm(PTG_avrg~PTSD_avrg + PTSD_avrg1, PTG_PTSD)
AIC(model2)
anova(model1, model2)
model3 <- lm(PTG_avrg~PTSD_avrg + PTSD_avrg1 + PTSD_avrg2, PTG_PTSD)
AIC(model3)
anova(model2, model3)

#best fitting model = model2
#plot regression  
ptg_ptsd_curvilinear <- ggplot(model2, aes(x=PTSD_avrg, y=PTG_avrg)) + 
  geom_point() +
  stat_smooth(se=T, method='lm', formula=y~poly(x,2)) +
  xlab("PTSD") +
  ylab ("Posttraumatic growth") +
  ggtitle("PTSD as a predictor of Posttraumatic growth") +
  theme_light()

ggsave('ptg_ptsd_curvilinear.png', ptg_ptsd_curvilinear)

##comaprisons--------------------------------
library(MKmisc)
library(effectsize)
library(psychometric)
#PTG
mi.t.test(dat, x = "PTG", y = "Sex", var.equal = FALSE) #m1 = 14.808; sd1 = 13.550; n1 = 26; m2 = 29.827; sd2 = 13.129; n = 100
d1 <- (29.827-14.808)/sqrt(((13.550)^2+(13.129)^2/2)) # d = 0.914
rd1 <- d_to_r(d1) # r = 0.416
ci1 <- CIr(r = rd1, n = 126, level = .95)
ci1lower <- 0.2597838
ci1upper <- 0.5506576

mi.t.test(dat, x = "PTG", y = "relationship", var.equal = FALSE) #m1 = 27.047; sd1 = 14.309; m2 = 26.208; sd2 = 14.962
d2 <- (26.208-27.047)/sqrt(((14.309)^2+(14.962)^2/2)) # d = -0.047
rd2 <- d_to_r(d2) # r = -0.024
ci2 <- CIr(r = rd2, n = 126, level = .95)
ci2lower <- -0.1976591
ci2upper <- 0.1519664

mi.t.test(dat, x = "PTG", y = "current_state", var.equal = FALSE) #m1 = 27.544; sd1 = 12.051; m2 = 26.259; sd2 = 15.802
d3 <- (26.259-27.544)/sqrt(((12.051)^2+(15.802)^2/2)) # d = -0.078
rd3 <- d_to_r(d3) # r = -0.039
ci3 <- CIr(r = rd3, n = 126, level = .95)
ci3lower <- -0.2125206
ci3upper <- 0.1367757

mi.t.test(dat, x = "PTG", y = "relaps_cancer", var.equal = FALSE) #m1 = 32.764; sd1 = 10.797; m2 = 23.053; sd2 = 15.399
d4 <- (23.053-32.764)/sqrt(((10.797)^2+(15.399)^2/2)) # d = -0.633
rd4 <- d_to_r(d4) # r = -0.302
ci4 <- CIr(r = rd4, n = 126, level = .95)
ci4lower <- -0.4528676
ci4upper <- -0.1340423

mi.t.test(dat, x = "PTG", y = "metastasis", var.equal = FALSE) #m1 = 22.334; sd1 = 14.757; m2 = 30.464; sd2 = 14.441
d5 <- (30.464-22.334)/sqrt(((14.757)^2+(14.441)^2/2)) # d = 0.453
rd5 <- d_to_r(d5) # r = 0.221
ci5 <- CIr(r = rd5, n = 126, level = .95)
ci5lower <- 0.04786507
ci5upper <- 0.38110320

rcat1 <- c(rd1, rd2, rd3, rd4, rd5) #correlations vector for PTG and factor variables
rcatlower1 <- c(ci1lower, ci2lower, ci3lower, ci4lower, ci5lower)
rcatupper1 <- c(ci1upper, ci2upper, ci3upper, ci4upper, ci5upper)

#PTSD
mi.t.test(dat, x = "PTSD", y = "Sex", var.equal = FALSE) #m1 = 15.799; sd1 = 18.526; m2 = 26.227; sd2 = 16.651
d6 <- (26.227-15.799)/sqrt(((18.526)^2+(16.651)^2/2)) # d = 0.475
rd6 <- d_to_r(d6) # r = 0.231
ci6 <- CIr(r = rd6, n = 126, level = .95)
ci6lower <- 0.05856067
ci6upper <- 0.39023375  
  
mi.t.test(dat, x = "PTSD", y = "relationship", var.equal = FALSE) #m1 = 23.596; sd1 = 17.559; m2 = 24.850; sd2 = 17.228
d7 <- (24.850-23.596)/sqrt(((17.559)^2+(17.228)^2/2)) # d = 0.059
rd7 <- d_to_r(d7) # r = 0.029
ci7 <- CIr(r = rd7, n = 126, level = .95)
ci7lower <- -0.1463313
ci7upper <- 0.2031909

mi.t.test(dat, x = "PTSD", y = "current_state", var.equal = FALSE) #m1 = 28.040; sd1 = 17.543; m2 = 21.794; sd2 = 17.186
d8 <- (21.794-28.040)/sqrt(((17.543)^2+(17.186)^2/2)) # d = -0.293
rd8 <- d_to_r(d8) # r = -0.145
ci8 <- CIr(r = rd8, n = 126, level = .95)
ci8lower <- -0.31180635
ci8upper <- 0.03089312

mi.t.test(dat, x = "PTSD", y = "relaps_cancer", var.equal = FALSE) #m1 = 26.059; sd1 = 13.521; m2 = 18.514; sd2 = 18.590
d9 <- (18.514-26.059)/sqrt(((13.521)^2+(18.590)^2/2)) # d = -0.400
rd9 <- d_to_r(d9) # r = -0.196
ci9 <- CIr(r = rd9, n = 126, level = .95)
ci9lower <- -0.35876210
ci9upper <- -0.02201285

mi.t.test(dat, x = "PTSD", y = "metastasis", var.equal = FALSE) #m1 = 15.356; sd1 = 15.232; m2 = 29.750; sd2 = 19.396
d10 <- (29.750-15.356)/sqrt(((15.232)^2+(19.396)^2/2)) # d = 0.702
rd10 <- d_to_r(d10) # r = 0.331
ci10 <- CIr(r = rd10, n = 126, level = .95)
ci10lower <- 0.1660122
ci10upper <- 0.4784800

rcat2 <- c(rd6, rd7, rd8, rd9, rd10)
rcatlower2 <- c(ci6lower, ci7lower, ci8lower, ci9lower, ci10lower)
rcatupper2 <- c(ci6upper, ci7upper, ci8upper, ci9upper, ci10upper)

##correlations-------------------------------
dataComplete <- complete(data_imp, "long")
dataComplete <- dataComplete[,-c(1:2)]
match(c("PTG", "PTSD", "time_since_diagnosis", "hospitalisation_number", 
        "pain", "discomfort", "social_isolation", "anxiety_fear", "sadness_depression", "lost_of_meaning",
        "REZIL", "ISLESFiW", "ISLESComp", "MMHHless", "MMAnx", "MMFight", "MMFatal", "MMCogAvoid", 
        "Self_transcend", "Religiosity"), names(dataComplete))
miceadds::micombine.cor(data_imp, 
                        variables = c(130, 125, 11, 13, 15:20, 131:140), 
                        conf.level=0.95, method="pearson", nested=FALSE, partial=NULL)

miceadds::micombine.cor(data_imp, 
                        variables = c(130, 125, 5), 
                        conf.level=0.95, method="spearman", nested=FALSE, partial=NULL)

correlations <- miceadds::micombine.cor(data_imp, 
                                        variables = c(130, 125, 11, 13, 15:20, 131:140), 
                                        conf.level=0.95, method="pearson", nested=FALSE, partial=NULL)

s <- miceadds::micombine.cor(data_imp, 
                        variables = c(130, 125, 5), 
                        conf.level=0.95, method="spearman", nested=FALSE, partial=NULL)

#correlations plot
#ptg~~ptsd plot
cor.test(PTG_PTSD$PTG_avrg, PTG_PTSD$PTSD_avrg) #produced from average PTG and PTSD
library(grid)
r_anotation <-  grobTree(textGrob(paste("Pearson Correlation: 0.302 [.13,.46]"), 
                                  x = 0.57, y = 0.89, hjust = 0, 
                                  gp = gpar(col = "red", fontsize = 14, fontface = "bold")))

ptgptsd <- ggplot(data = PTG_PTSD, aes(x=PTSD_avrg, y=PTG_avrg, y)) +
  geom_point() +
  geom_smooth() +
  theme_bw() +
  theme_minimal() +
  scale_color_hue(h = c(180, 300)) +
  annotation_custom(r_anotation) +
  ylab("Posttraumatic growth") +
  xlab("PTSD") +
  ggtitle("Posttraumatic growth and PTSD")

ggsave('ptg_ptsd_scatterplot.png', ptgptsd)
 
#PTG and PTSD correlates
variables <- c("gender", "education", "partnership", 
               "remission", "cancer relaps", "metastasis", "diagnosis time", "hospitalisation number", 
               "pain", "discomfort", "social isolation", "anxiety", "sadness", "lost of meaning",
               "resilience", "footing in the world", "comprehensibility", 
               "help-hopelessness", "anxious preocupation", "fighting spirit", "fatalism", "cognitive avoidance", 
               "self-transcendence", "religiosity")

scor <- s$r
s1 <- scor[2]
s2 <- scor[3]

slower <- s$lower95
slower1 <- slower[2]
slower2 <- slower[3]

supper <- s$upper95 
supper1 <- supper[2]
supper2 <- supper[3]

#sociodemographic correlations vector
socdem1 <- c(rd1,s1,rd2) 
socdemlower1 <- c(ci1lower,slower1,ci2lower) 
socdemupper1 <- c(ci1upper,supper1,ci2upper) 

socdem2 <- c(rd6,s2,rd7) 
socdemlower2 <- c(ci6lower,slower2,ci7lower) 
socdemupper2 <- c(ci6upper,supper2,ci7upper) 

#correlations vector for cancer-related
cancer1 <- c(rd3, rd4, rd5)
cancerlower1 <- c(ci3lower,ci4lower,ci5lower) 
cancerupper1 <- c(ci3upper,ci4upper,ci5upper) 

cancer2 <- c(rd8, rd9, rd10)
cancerlower2 <- c(ci8lower,ci9lower,ci10lower) 
cancerupper2 <- c(ci8upper,ci9upper,ci10upper) 

#correlations vector for numeric
r <- correlations$r
numeric1 <- r[2:19]
numeric2 <- r[20:37]

lower <- correlations$lower95
numericlower1 <- lower[2:19]
numericlower2 <- lower[20:37]

upper <- correlations$upper95
numericupper1 <- upper[2:19]
numericupper2 <- upper[20:37]

groups <- c("personal","personal","personal", 
            "cancer-related", "cancer-related", "cancer-related", "cancer-related", "cancer-related",
            "emotional", "emotional", "emotional", "emotional", "emotional", "emotional",
            "psychological", "psychological", "psychological",
            "psychological", "psychological", "psychological", "psychological", "psychological",
            "psychological", "psychological")

r1 <- c(socdem1, cancer1, numeric1)
r2 <- c(socdem2, cancer2, numeric2)
lower1 <- c(socdemlower1, cancerlower1, numericlower1)
lower2 <- c(socdemlower2, cancerlower2, numericlower2)
upper1 <- c(socdemupper1, cancerupper1, numericupper1)
upper2 <- c(socdemupper2, cancerupper2, numericupper2)

ptg <- tibble(variables, r1, lower1, upper1, groups)
ptsd <- tibble(variables, r2, lower2, upper2, groups)

ptg_plot <- ggplot(data=ptg, aes(x=variables, y=r1, ymin=lower1, ymax=upper1, color = groups)) +
  ylim(-0.5,0.6) +
  geom_pointrange() + 
  geom_hline(yintercept=0, lty=2) +  # add a dotted line at x=1 after flip
  coord_flip() +  # flip coordinates (puts labels on y axis)
  facet_grid(groups~., scales= "free", space="free") +
  theme_bw() + # use a white background
  theme(strip.background =element_rect(fill="white")) +
  theme_minimal() +
  xlab("correlates") + ylab("Pearson's r") +
  theme(axis.title.x = element_text(hjust= 0.45)) +
  labs(title = "Posttraumatic growth",
       subtitle = "") +
  theme(plot.title = element_text(hjust = 0.45)) +
  theme(legend.position = "none") +
  theme(strip.text = element_blank()) +
  scale_color_hue(h = c(180, 300))
  

ptsd_plot <- ggplot(data=ptsd, aes(x=variables, y=r2, ymin=lower2, ymax=upper2, color = groups)) +
  ylim(-0.75,0.75) +
  geom_pointrange() + 
  geom_hline(yintercept=0, lty=2) +  # add a dotted line at x=1 after flip
  coord_flip() +  # flip coordinates (puts labels on y axis)
  facet_grid(groups~., scales= "free", space="free") +
  theme_bw() + # use a white background
  theme(strip.background =element_rect(fill="white")) +
  theme_minimal() +
  xlab("") + ylab("Pearson's r") +
  labs(title = "PTSD",
       subtitle = "Pearson Correlation between PTG and PTSD: 0.302 [.13,.46]") +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(size = 9, face = "bold.italic", color = "red")) +
  theme(axis.text.y = element_blank()) +
  guides(col=guide_legend("Correlates categories")) +
  scale_color_hue(h = c(180, 300)) 
  

p <- ptg_plot + ptsd_plot
ggsave('ptg_ptsd_plot.png', p)





