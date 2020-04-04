########################################################################
##############VEGA TRNAVA STUDY1 (data collection1)#####################
########################################################################

#import data
library(readr)
data <- read_csv("raw_data.csv")

#load packages 
library(tidyverse)
library(mice)
library(psych)
library(MBESS)

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
dataset_imputed_complete <- complete(dataset_imputed)

#creating Minimac29 mean from all imputed datasets and adding Minimac29 to dataset for study1
dataset_imputed_complete_all <- complete(dataset_imputed, "long")
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

#religiosity rename factors

data %>% 
  rename("Religiosity (Intellect)" = Religiozita_1, "Religiosity (Ideology)" = Religiozita_2, 
         "Religiosity (Public practice)" = Religiozita_3, "Religiosity (Private practice)" = Religiozita_4,
         "Religiosity (Experience)" = Religiozita_5) %>% 
  colnames()

#object MICE
init <-   mice(data, maxit=0) 
meth <-   init$method
predM <-   init$predictorMatrix

predM[,c("ID", "Age", "current_state", "Education", "child_number", "job", "living", "cancer_family_anamnesis",
         "time_since_diagnosis", "relaps_cancer", "hospitalisation_number", "type_of_cancer", "metastasis", "chemotherapy", "surgery",
         "radiotherapy", "hormonal", "alternative", "palliative", "Religiosity (Intellect)", "Religiosity (Ideology)", 
         "Religiosity (Public practice)", "Religiosity (Private practice)", 
         "Religiosity (Experience)", "time_since_dg_categories")] <- 0 #exclusion from the prediction
predM[c("ID", "Age", "current_state", "Education", "child_number", "job", "living", "cancer_family_anamnesis",
         "time_since_diagnosis", "relaps_cancer", "hospitalisation_number", "type_of_cancer", "metastasis", "chemotherapy", "surgery",
         "radiotherapy", "hormonal", "alternative", "palliative", "Religiosity (Intellect)", "Religiosity (Ideology)", 
        "Religiosity (Public practice)", "Religiosity (Private practice)", 
         "Religiosity (Experience)", "time_since_dg_categories"),] <- 0 #exclusion from the prediction

meth[c("Age", "Education", "child_number", "job", "living", "cancer_family_anamnesis", 
       "time_since_diagnosis", "relaps_cancer", "hospitalisation_number", "metastasis", "time_since_dg_categories")] <-  "" #exclusion from the imputation


set.seed(123)
data_imp <- mice(data, method=meth, predictorMatrix=predM, m=25, maxit = 10) #imputation of missing data - imputed object for the analysis

#all imputed datasets

dat <- complete(data_imp, "long")

#convert mice object to list
library(miceadds)
dat <- miceadds::mids2datlist(data_imp)

#####################################################################################################
###### next part is applied to basic analysis to list (25 imputed datasets) created from MICE #######
#####################################################################################################

###### if there was no missing values, the basic analysis is carried out on original dataset (data)

#total PTSD
dat <- lapply(dat, function(x){cbind(x, PTSD = rowSums(x[,c("PCL_1", "PCL_2", "PCL_3", "PCL_4",
                                                     "PCL_5", "PCL_6", "PCL_7", "PCL_8",
                                                     "PCL_9", "PCL_10", "PCL_11", "PCL_12",
                                                     "PCL_13", "PCL_14", "PCL_15", "PCL_16",
                                                     "PCL_17", "PCL_18", "PCL_19", "PCL_20")], na.rm = TRUE))})
#criterionB
data$criterionB <- with(data, PCL_1 + PCL_2 + PCL_3 + PCL_4 + PCL_5)

#criterionC
data$criterionC <- data$PCL_6 + data$PCL_7

#criterionD
dat <- lapply(dat, function(x){cbind(x, criterionD = rowSums(x[,c("PCL_8","PCL_9", "PCL_10", "PCL_11", "PCL_12", "PCL_13", "PCL_14")], na.rm = TRUE))})

#criterionE
dat <- lapply(dat, function(x){cbind(x, criterionE = rowSums(x[,c("PCL_15","PCL_16", "PCL_17", "PCL_18", "PCL_19", "PCL_20")], na.rm = TRUE))})


#posttraumatic growth
dat <- lapply(dat, function(x){cbind(x, PTG = rowSums(x[,c("PTGI_1", "PTGI_2", "PTGI_3", "PTGI_4", "PTGI_5", "PTGI_6", "PTGI_7", 
                                                           "PTGI_8", "PTGI_9", "PTGI_10")], na.rm = TRUE))})

#Resilience
data$REZIL <- with(data, Reziliencia1 + Reziliencia2 + Reziliencia3 + Reziliencia4 + Reziliencia5 + Reziliencia6)

#Integration of stressful events
data$ISLESFiW <- with(data, ISLES1 + ISLES3 + ISLES5 + ISLES7 + ISLES9 + ISLES11 + ISLES12 + ISLES13 + ISLES14 + ISLES15 + ISLES16)

data$ISLESComp <- with(data, ISLES2 + ISLES4 + ISLES6 + ISLES8 + ISLES10)

#Mental adjustment to cancer
data$MMHHless <- with(data, Minimac1 + Minimac2 + Minimac4 + Minimac5 + Minimac7 + Minimac8 + Minimac14 + Minimac19 + Minimac25)

data$MMAnx <- with(data, Minimac9 + Minimac10 + Minimac16 + Minimac19 + Minimac20 + Minimac21 + Minimac22 + Minimac29)

data$MMFight <- with(data, Minimac3 + Minimac6 + Minimac12 + Minimac13 + Minimac17) 

dat <- lapply(dat, function(x){cbind(x, MMFi = rowSums(x[,c("Minimac11", "Minimac24", "Minimac27", "Minimac28")], na.rm = TRUE))})

data$MMCogAvoid <- data$Minimac15 + data$Minimac18 + data$Minimac23 + data$Minimac26

#transform list back to MICE object
imp <- miceadds::datlist2mids(dat)
#complete all imputed dataset after lapply
imp_complete <- complete(imp, "long")

#####reliability

#PTSD
lapply(dat, function(x){ci.reliability(x[,c("PCL_1", "PCL_2", "PCL_3", "PCL_4",
                                               "PCL_5", "PCL_6", "PCL_7", "PCL_8",
                                               "PCL_9", "PCL_10", "PCL_11", "PCL_12",
                                               "PCL_13", "PCL_14", "PCL_15", "PCL_16",
                                               "PCL_17", "PCL_18", "PCL_19", "PCL_20")], type = "omega")})
##criterionB
criterionB <- PTSD[,1:5]
ci.reliability(criterionB, type = "omega")
##criterionC
criterionC <- PTSD[,6:7]
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

