library(gtsummary, warn.conflicts =  F)
library(survey, warn.conflicts = F)

imp_ex <- complete(weighted.datasets, "all")
imp_ex <- imp_ex[[1]]
names(imp_ex)[names(imp_ex) == "weights"] <- "ps_weights"
imp_ex$e <- as.factor(imp_ex$e)

design <- svydesign(data = imp_ex, ids = ~1)
tbl_svysummary(design, by = "e", digits = ~ 1) %>%
  add_difference(everything() ~ "smd")

design <- svydesign(data = imp_ex, ids = ~1, weights = ~ps_weights)
tbl_svysummary(design, by = "e", digits = ~ 1) %>%
  add_difference(everything() ~ "smd")