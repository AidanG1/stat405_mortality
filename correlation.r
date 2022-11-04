mortality_correlations <- query(
    "SELECT educ2003, ager27, placdth, weekday, mandeath, ucr39, race from mortality where mandeath is not NULL"
)

mortality_correlations$mandeath <- as.factor(mortality_correlations$mandeath)
# mortality_correlations$weekday <- as.factor(mortality_correlations$weekday)
mortality_correlations$ucr39 <- as.factor(mortality_correlations$ucr39)
# mortality_correlations$race <- as.factor(mortality_correlations$race)
mortality_correlations$ager27 <- as.factor(mortality_correlations$ager27)

dummies <- model.matrix(~ mandeath - 1, mortality_correlations)
dummies1 <- model.matrix(~ ager27 - 1, mortality_correlations)
# dummies2 <- model.matrix(~ ucr39 - 1, mortality_correlations)

cor_df <- data.frame(
    mortality_correlations[, !colnames(mortality_correlations) %in% c("mandeath", "ager27", "ucr39")],
    dummies, dummies1
)

cor(cor_df)
