library(nephro, warn.conflicts = F)
library(dplyr, warn.conflicts = F)

## Chargement des données
path_data <- '../data/data.csv'
data <- read.csv2(path_data, encoding = 'utf8' , header=TRUE, stringsAsFactors=FALSE, fileEncoding="latin1")
data <- remove_empty_cols(data)

## On récupère les données qui ont été saisies manuellement après récupération
# Outcome
dataset_outcome_renal <- read.csv2("../data/outcome_add.csv", dec = ".")
dataset_outcome_renal$creat_m12_add <- as.numeric(dataset_outcome_renal$creat_m12_add)
dataset_outcome_renal$creat_m3_add <- as.numeric(dataset_outcome_renal$creat_m3_add)
dataset_outcome_renal$creat_m1_add <- as.numeric(dataset_outcome_renal$creat_m1_add)
dataset_outcome_renal$dfg_m12_add <- as.numeric(dataset_outcome_renal$dfg_m12_add)
dataset_outcome_renal$dfg_m3_add <- as.numeric(dataset_outcome_renal$dfg_m3_add)
dataset_outcome_renal$dfg_m1_add <- as.numeric(dataset_outcome_renal$dfg_m1_add)
data <- left_join(data, dataset_outcome_renal, by = "npatient", keep = F)

# Baseline
dataset_baseline_renal <- read.csv2("../data/baseline_add.csv", dec = ".")
dataset_baseline_renal$creat.de.base_add <- as.numeric(dataset_baseline_renal$creat.de.base_add)
dataset_baseline_renal$DFG.base_add <- as.numeric(dataset_baseline_renal$DFG.base_add)
data <- left_join(data, dataset_baseline_renal, by = "npatient", keep = F)

# Curation
data$Glasgow <- data$Glasgow < 15
data <- convert(data)
data$JX.décès.par...à.entrée <- as.numeric(data$JX.décès.par...à.entrée)
data$Délai..J0.Hospit.EER. <- as.numeric(data$Délai..J0.Hospit.EER.)
data$Stade.KDIGO.AKI <- data$Stade.KDIGO.AKI == 3

# Remplacement des valeurs manquantes 
# Outcome
data$Creat.M12[!is.na(data$creat_m12_add)] <- data$creat_m12_add[!is.na(data$creat_m12_add)]
data$Creat.M3[!is.na(data$creat_m3_add)] <- data$creat_m3_add[!is.na(data$creat_m3_add)]
data$Creat.M1[!is.na(data$creat_m1_add)] <- data$creat_m1_add[!is.na(data$creat_m1_add)]
data$DFG.M12[!is.na(data$dfg_m12_add)] <- data$dfg_m12_add[!is.na(data$dfg_m12_add)]
data$DFG.M3[!is.na(data$dfg_m3_add)] <- data$dfg_m3_add[!is.na(data$dfg_m3_add)]
data$DFG.M1[!is.na(data$dfg_m1_add)] <- data$dfg_m1_add[!is.na(data$dfg_m1_add)]

# Baseline
data$creat.de.base[!is.na(data$creat.de.base_add)] <- data$creat.de.base_add[!is.na(data$creat.de.base_add)]
data$DFG.base[!is.na(data$DFG.base_add)] <- data$DFG.base_add[!is.na(data$DFG.base_add)]

# Dérivation des DFG en fonction des créatininémies
data$DFG.base.derived <- CKDEpi_RF.creat(data$creat.de.base/88.4, ifelse(data$Sexe..M., 1, 0), data$Age)
data$DFG.sortie.derived <- CKDEpi_RF.creat(data$créat.sortie/88.4, ifelse(data$Sexe..M., 1, 0), data$Age)
data$DFG.M1.derived <- CKDEpi_RF.creat(data$Creat.M1/88.4, ifelse(data$Sexe..M., 1, 0), data$Age)
data$DFG.M3.derived <- CKDEpi_RF.creat(data$Creat.M3/88.4, ifelse(data$Sexe..M., 1, 0), data$Age)
data$DFG.M12.derived <- CKDEpi_RF.creat(data$Creat.M12/88.4, ifelse(data$Sexe..M., 1, 0), data$Age)

data$DFG.M12.derived[is.na(data$DFG.M12.derived) & !is.na(data$DFG.M12)] <- data$DFG.M12[is.na(data$DFG.M12.derived) & !is.na(data$DFG.M12)]
data$DFG.M3.derived[is.na(data$DFG.M3.derived) & !is.na(data$DFG.M3)] <- data$DFG.M3[is.na(data$DFG.M3.derived) & !is.na(data$DFG.M3)]
data$DFG.M1.derived[is.na(data$DFG.M1.derived) & !is.na(data$DFG.M1)] <- data$DFG.M1[is.na(data$DFG.M1.derived) & !is.na(data$DFG.M1)]
data$DFG.sortie.derived[is.na(data$DFG.sortie.derived) & !is.na(data$DFG.sortie)] <- data$DFG.sortie[is.na(data$DFG.sortie.derived) & !is.na(data$DFG.sortie)]
data$DFG.base.derived[is.na(data$DFG.base.derived) & !is.na(data$DFG.base)] <- data$DFG.base[is.na(data$DFG.base.derived) & !is.na(data$DFG.base)]

# Npatient en factor pour convénience
data$npatient <- as.factor(data$npatient)

# Recodage MRC
data$MRC.new <- data$DFG.base.derived < 60
data$MRC.new[is.na(data$MRC.new)] <- data$MRC[is.na(data$MRC.new)]
data$MRC.new[is.na(data$MRC.new)] <- F # On impute F à 5 valeurs manquantes, on testera dans une analyse de sensibilité si ça change de les exclure