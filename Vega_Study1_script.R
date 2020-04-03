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

#extracting Minimac29 and adding Minimac29 to dataset for study1
dataset_imputed_complete <- dataset_imputed_complete[-c(127:290),]
data$Minimac29 <- dataset_imputed_complete$Minimac29

#missing data in dataset for study1
sum(is.na(data))/prod(dim(data))
colMeans(is.na(data))

#recoding the MINI-MAC items (because values was admistrated in reverse order)

data <- data %>% 
  mutate_at(c("Minimac1", "Minimac2", "Minimac3", "Minimac4", "Minimac5", "Minimac6", 
              "Minimac7", "Minimac8", "Minimac9", "Minimac10", "Minimac11", "Minimac12", 
              "Minimac13", "Minimac14", "Minimac15", "Minimac16", 
              "Minimac17", "Minimac18", "Minimac19", "Minimac20", "Minimac21", 
              "Minimac22", "Minimac23", "Minimac24", "Minimac25", 
              "Minimac26", "Minimac27", "Minimac28", "Minimac29"), funs(recode(., `1`=4, `2`=3, `3`=2, `4`=1)))

#object MICE
init <-   mice(data, maxit=0) 
meth <-   init$method
predM <-   init$predictorMatrix

predM[,c("ID", "Age", "current_state", "Education", "child_number", "job", "living", "cancer_family_anamnesis",
         "time_since_diagnosis", "relaps_cancer", "hospitalisation_number", "type_of_cancer", "metastasis", "chemotherapy", "surgery",
         "radiotherapy", "hormonal", "alternative", "palliative", "Religiozita_1", "Religiozita_2", "Religiozita_3", "Religiozita_4", 
         "Religiozita_5", "time_since_dg_categories")] <- 0 #exclusion from the prediction
predM[c("ID", "Age", "current_state", "Education", "child_number", "job", "living", "cancer_family_anamnesis",
         "time_since_diagnosis", "relaps_cancer", "hospitalisation_number", "type_of_cancer", "metastasis", "chemotherapy", "surgery",
         "radiotherapy", "hormonal", "alternative", "palliative", "Religiozita_1", "Religiozita_2", "Religiozita_3", "Religiozita_4", 
         "Religiozita_5", "time_since_dg_categories"),] <- 0 #exclusion from the prediction

meth[c("Age", "Education", "child_number", "job", "living", "cancer_family_anamnesis", 
       "time_since_diagnosis", "relaps_cancer", "hospitalisation_number", "metastasis", "time_since_dg_categories")] <-  "" #exclusion from the imputation


set.seed(123)
data_imp <- mice(data, method=meth, predictorMatrix=predM, m=25, maxit = 10) #imputation of missing data - imputed object for the analysis

#all imputed datasets

x <- complete(data_imp, "long")

#convert mice object to list
library(miceadds)
x <- miceadds::mids2datlist(data_imp)

########################################################
###### next part is needed to be applied to MICE #######
########################################################

#sum of scales and subscales
#PTSD
data$PTSD <- data$PCL_1 + data$PCL_2 + data$PCL_3 + data$PCL_4 + data$PCL_5 + data$PCL_6 + 
             data$PCL_7 + data$PCL_8 + data$PCL_9 + data$PCL_10 + data$PCL_11 + data$PCL_12 +
             data$PCL_13 + data$PCL_14 + data$PCL_15 + data$PCL_16 + data$PCL_17 + data$PCL_18 + data$PCL_19 + data$PCL_20  

data$criterionB <- data$PCL_1 + data$PCL_2 + data$PCL_3 + data$PCL_4 + data$PCL_5

data$criterionC <- data$PCL_6 + data$PCL_7

data$criterionD <- data$PCL_8 + data$PCL_9 + data$PCL_10 + data$PCL_11 + data$PCL_12 + data$PCL_13 + data$PCL_14

data$criterionE <- data$PCL_15 + data$PCL_16 + data$PCL_17 + data$PCL_18 + data$PCL_19 + data$PCL_20

#sum in all imputed datasets


#posttraumatic growth
data$PTG <- data$PTGI_1 + data$PTGI_2 + data$PTGI_3 + data$PTGI_4 + data$PTGI_5 + data$PTGI_6 + data$PTGI_7 + 
            data$PTGI_8 + data$PTGI_9 + data$PTGI_10

#Resilience
data <- data %>% 
  mutate_at (c("Reziliencia2", "Reziliencia4", "Reziliencia6"), funs(recode(., `1`=5, `2`=4, `3`=3, `4`=2, `5`=1)))
data$REZIL <- data$Reziliencia1 + data$Reziliencia2 + data$Reziliencia3 + data$Reziliencia4 + data$Reziliencia5 + data$Reziliencia6

#Integration of stressful events
data <- data %>% 
  mutate_at ("ISLES2", funs(recode(., `1`=5, `2`=4, `3`=3, `4`=2, `5`=1)))
data$ISLESFiW <- data$ISLES1 + data$ISLES3 + data$ISLES5 + data$ISLES7 + data$ISLES9 + data$ISLES11 + data$ISLES12 +
                 data$ISLES13 + data$ISLES14 + data$ISLES15 + data$ISLES16

data$ISLESComp <- data$ISLES2 + data$ISLES4 + data$ISLES6 + data$ISLES8 + data$ISLES10

#Mental adjustment to cancer
data$MMHHless <- data$Minimac1 + data$Minimac2 + data$Minimac4 + data$Minimac5 + data$Minimac7 + data$Minimac8 + 
                 data$Minimac14 + data$Minimac19 + data$Minimac25

data$MMAnx <- data$Minimac9 + data$Minimac10 + data$Minimac16 + data$Minimac19 + data$Minimac20 + data$Minimac21 + 
              data$Minimac22 + data$Minimac29

data$MMFight <- data$Minimac3 + data$Minimac6 + data$Minimac12 + data$Minimac13 + data$Minimac17 

data$MMFatal <- data$Minimac11 + data$Minimac24 + data$Minimac27 + data$Minimac28 

data$MMCogAvoid <- data$Minimac15 + data$Minimac18 + data$Minimac23 + data$Minimac26

#religiosity rename factors
data %>% 
  rename("Religiosity (Intellect)" = Religiozita_1, "Religiosity (Ideology)" = Religiozita_2, 
                "Religiosity (Public practice)" = Religiozita_3, "Religiosity (Private practice)" = Religiozita_4,
                "Religiosity (Experience)" = Religiozita_5) %>% 
  colnames()

#####reliability

library(MBESS)

#PTSD
PTSD <- data[,30:49]
ci.reliability(PTSD, type = "omega")
##criterionB
criterionB <- PTSD[,1:5]
ci.reliability(criterionB, type = "omega")
##criterionC
criterionC <- PTSD[,6:7]
cor(criterionC, method = "spearman")
##criterionD
criterionD <- PTSD[,8:14]
ci.reliability(criterionD, type = "omega")
##criterionE 
criterionE <- PTSD[, 15:20]
ci.reliability(criterionE, type = "omega")

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
PTG <- data[,89:98]
ci.reliability(PTG, type = "omega")

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
MMFatal <- subset(data, select = c("Minimac11", "Minimac24", "Minimac27", "Minimac28"))
ci.reliability(MMFatal, type = "omega")
##cognitive avoidance
MMCogAvoid <- subset(data, select = c("Minimac15", "Minimac18", "Minimac23", "Minimac26"))
ci.reliability(MMCogAvoid, type = "omega")

#####Cronbach's alpha
alpha(PTSD)
alpha(criterionB)
alpha(criterionC)
alpha(criterionD)
alpha(criterionE)
alpha(PTG)
alpha(ISLESFiW)
alpha(ISLESComp)
alpha(RESIL)
alpha(MMHHless)
alpha(MMAnx)
alpha(MMFight)
alpha(MMFatal)
alpha(MMCogAvoid)

