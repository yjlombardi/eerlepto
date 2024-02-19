library(mice, warn.conflicts = F)
library(MatchThem, warn.conflicts = F)
library(cobalt, warn.conflicts = F)
library(gtsummary, warn.conflicts = F)
library(survey, warn.conflicts = F)
library(dplyr, warn.conflicts = F)
set.seed(123)

# Données
d <- data
cols <- names(data)
cols <- cols[!grepl("date", cols)]
d <- d[,cols]

# Cutoff
cut <- 2

# Nombre d'imputation
m <- 100

# Nombre de coeurs
numcores <- 10

# Covariables cliniques
covar_clin <- c(
  "Age", "DFG.base.derived", "FC", "PAS", "Température", "Glasgow", "PO2", "Cancer"
)

# Covariables bio
covar_bio <- c(
  "Urée", "K.", "bicarbonates", "Na", "Bili.totale", "GB"
)

# Formule
formula <- e ~ 
  Age + FC + PAS + Température + Glasgow + PO2 + Urée + Na + K. + 
  bicarbonates + Bili.totale + GB + Cancer + DFG.base.derived

# Outcomes
# o1 = décès
d$o1 <- !is.na(d$JX.décès.par...à.entrée) | (!is.na(d$Décès.à.M12) & d$Décès.à.M12) | d$Décès | d$décès

# o2 = adapté de WRF
# pour les patients sans IRC -> DFG au dernier suivi < 60 ou HD
# pour les MRC stade III -> DFG au dernier suivi < 30 ou HD
# pour les MRC stade IV -> DFG au dernier suivi < 15 ou HD
# pour les MRC stade V -> HD

d$o2 <- NA
d$o2[d$o1] <- F
d$o2[is.na(d$o2) & !is.na(d$HDI.chronique) & d$HDI.chronique] <- T

d$last.DFG <- d$DFG.M12.derived
d$last.DFG[is.na(d$last.DFG)] <- d$DFG.M3.derived[is.na(d$last.DFG)] 
d$last.DFG[is.na(d$last.DFG)] <- d$DFG.M1.derived[is.na(d$last.DFG)] 
d$last.DFG[is.na(d$last.DFG)] <- d$DFG.sortie.derived[is.na(d$last.DFG)]

d$o2[is.na(d$o2) & !d$MRC.new & !is.na(d$MRC.new)] <- d$last.DFG[is.na(d$o2) & !d$MRC.new & !is.na(d$MRC.new)] < 60 
d$o2[is.na(d$o2) & d$MRC.new & !is.na(d$MRC.new) & d$DFG.base.derived < 60 & d$DFG.base.derived >= 30] <- d$last.DFG[is.na(d$o2) & d$MRC.new & !is.na(d$MRC.new) & d$DFG.base.derived < 60 & d$DFG.base.derived >= 30] < 30 
d$o2[is.na(d$o2) & d$MRC.new & !is.na(d$MRC.new) & d$DFG.base.derived < 30 & d$DFG.base.derived >= 15] <- d$last.DFG[is.na(d$o2) & d$MRC.new & !is.na(d$MRC.new) & d$DFG.base.derived < 30 & d$DFG.base.derived >= 15] < 15 

# Exposition
d$e <- ifelse(!is.na(d$Délai..J0.Hospit.EER.) & d$Délai..J0.Hospit.EER. <= cut, T, F)
exposure <- c("e")

# Critère composite
outcomes <- c("o1", "o2")

# Inclusion
exc <- (!is.na(d$Dialysé.Chronique) & d$Dialysé.Chronique == T) |
  (d$DFG.base.derived == 0 & !is.na(d$DFG.base.derived)) |
  d$Stade.KDIGO.AKI == FALSE |
  d$Urée > 40 |
  (d$K. > 6 & !is.na(d$K.)) |
  (d$bicarbonates < 11 & !is.na(d$bicarbonates)) 

d <- d[!exc,]

# On constitue le dataset pour imputation
dataset_complete <- d[,c(covar_clin, covar_bio, exposure, outcomes)]

# On constitue un dataset pour récupération manuelle des données de suivi manquante
# dataset_outcome_renal <- d[!d$o1 & !(!is.na(d$HDI.chronique) & d$HDI.chronique) ,c("npatient", "créat.sortie", "Creat.M1", "Creat.M3", "Creat.M12", "last.DFG")] 
# write.csv2(dataset_outcome_renal, "../output/outcome_renal.csv", row.names = F, fileEncoding = "latin1")

# On constitue un dataset pour récupération manuelle des données baseline manquantes
# dataset_baseline_renal <- d[!(!is.na(d$HDI.chronique) & d$HDI.chronique) ,c("npatient", "DFG.base", "creat.de.base")]
# write.csv2(dataset_baseline_renal, "../output/baseline_renal.csv", row.names = F, fileEncoding = "latin1")

# Imputation des données
imp <- futuremice(dataset_complete, parallelseed = 111, n.core = numcores, m = m)

# Pondération des datasets
weighted.datasets <- weightthem(formula, datasets = imp)

## Balance entre les groupes
newnames_full <- c("Age", "Baseline eGFR", "Heart rate", "SBP", "Temperature", "GCS < 15", "PO2", "Cancer", "CKD", "CKF", "BUN", "K", "Bicarbonates", "Na", "Bilirubin", "Leucocytes")
names(newnames_full) <- names(dataset_complete)[1:16]
newnames <- newnames_full[c(1:8,11:16)]

tiff("../output/std_plot.tiff", width = 2000, height = 2000, res = 400)
love.plot(weighted.datasets, stats = c("mean.diffs"),
          thresholds = c(m = .1), abs = F,
          binary = "std",
          var.order = "unadjusted",
          agg.fun = "mean", var.names = newnames_full)
dev.off()

tiff("../output/vr_plot.tiff", width = 2000, height = 2000, res = 400)
love.plot(weighted.datasets, stats = c("variance.ratios"),
          thresholds = c(v = 2), abs = F,
          binary = "std",
          var.order = "unadjusted",
          agg.fun = "mean", var.names = newnames)
dev.off()

## Outcome composite
# Dérivation des modèles pondérés
weighted.models <- with(weighted.datasets,
                        svyglm((o1 | o2) ~ e, family = quasibinomial()))

# Pool selon la règle de Rubin
weighted.results <- pool(weighted.models)

# Dérivation des modèles non-pondérés
unweighted.models <- with(imp,
                          glm((o1 | o2) ~ e, family = "quasibinomial"))

# Pool selon la règle de Rubin
unweighted.results <- pool(unweighted.models)

# Affichage
print("Unweighted")
resp <- paste(round(exp(summary(unweighted.results, conf.int = TRUE)[2,2]), 2), ' (', round(exp(summary(unweighted.results, conf.int = TRUE)[2,7]), 2), ' to ', round(exp(summary(unweighted.results, conf.int = TRUE)[2,8]), 2), ")", sep = "")
resp
p <- summary(unweighted.results, conf.int = TRUE)[2,6]
p

print("Weighted")
resp <- paste(round(exp(summary(weighted.results, conf.int = TRUE)[2,2]), 2), ' (', round(exp(summary(weighted.results, conf.int = TRUE)[2,7]), 2), ' to ', round(exp(summary(weighted.results, conf.int = TRUE)[2,8]), 2), ")", sep = "")
resp
p <- summary(weighted.results, conf.int = TRUE)[2,6]
p