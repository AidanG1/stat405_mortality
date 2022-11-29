# source("setup.r")

library(rpart)
library(rpart.plot)

df <- query("SELECT mandeath, educ2003, monthdth, sex, ager52, ager27, placdth, marstat, weekday, injwork, methdisp, autopsy, ucr358, ucr113, ucr39, racer5,
    CASE WHEN record_1 IS NOT NULL AND record_1 <> '' THEN 1 ELSE 0 END +
        CASE WHEN record_2 IS NOT NULL AND record_2 <> '' THEN 1 ELSE 0 END +
        CASE WHEN record_3 IS NOT NULL AND record_3 <> '' THEN 1 ELSE 0 END +
        CASE WHEN record_4 IS NOT NULL AND record_4 <> '' THEN 1 ELSE 0 END +
        CASE WHEN record_5 IS NOT NULL AND record_5 <> '' THEN 1 ELSE 0 END +
        CASE WHEN record_6 IS NOT NULL AND record_6 <> '' THEN 1 ELSE 0 END +
        CASE WHEN record_7 IS NOT NULL AND record_7 <> '' THEN 1 ELSE 0 END +
        CASE WHEN record_8 IS NOT NULL AND record_8 <> '' THEN 1 ELSE 0 END +
        CASE WHEN record_9 IS NOT NULL AND record_9 <> '' THEN 1 ELSE 0 END +
        CASE WHEN record_10 IS NOT NULL AND record_10 <> '' THEN 1 ELSE 0 END +
        CASE WHEN record_11 IS NOT NULL AND record_11 <> '' THEN 1 ELSE 0 END +
        CASE WHEN record_12 IS NOT NULL AND record_12 <> '' THEN 1 ELSE 0 END +
        CASE WHEN record_13 IS NOT NULL AND record_13 <> '' THEN 1 ELSE 0 END +
        CASE WHEN record_14 IS NOT NULL AND record_14 <> '' THEN 1 ELSE 0 END +
        CASE WHEN record_15 IS NOT NULL AND record_15 <> '' THEN 1 ELSE 0 END +
        CASE WHEN record_16 IS NOT NULL AND record_16 <> '' THEN 1 ELSE 0 END +
        CASE WHEN record_17 IS NOT NULL AND record_17 <> '' THEN 1 ELSE 0 END +
        CASE WHEN record_18 IS NOT NULL AND record_18 <> '' THEN 1 ELSE 0 END +
        CASE WHEN record_19 IS NOT NULL AND record_19 <> '' THEN 1 ELSE 0 END +
    CASE WHEN record_20 IS NOT NULL THEN 1 ELSE 0 END AS avg_record_count
     FROM mortality")

df$avg_record_count <- as.numeric(df$avg_record_count)

df$mandeath[is.na(df$mandeath)] <- 8

manners <- list(
    "1" = "Accident",
    "2" = "Suicide",
    "3" = "Homicide",
    "4" = "Pending Investigation",
    "5" = "Could Not Determine",
    "6" = "Self-Inflicted",
    "7" = "Natural",
    "8" = "Not Specified"
)

df %>% mutate(
    is_accident = mandeath == "1",
    is_suicide = mandeath == "2",
    is_homicide = mandeath == "3",
    is_pending_investigation = mandeath == "4",
    is_cnd = mandeath == "5",
    is_self_inflicted = mandeath == "6",
    is_natural = mandeath == "7",
    is_not_specified = mandeath == "8"
) -> df

manner_names <- unlist(lapply(
    df$mandeath,
    function(x) {
        manners[[paste0(x)]]
    }
))

df$manner_name <- manner_names
df$manner_name <- as.factor(df$manner_name)
df$mandeath <- as.factor(df$mandeath)
df$sex <- as.factor(df$sex)
df$educ2003 <- as.factor(df$educ2003)
df$monthdth <- as.factor(df$monthdth)
df$placdth <- as.factor(df$placdth)
df$weekday <- as.factor(df$weekday)
df$ucr358 <- as.factor(df$ucr358)
df$ucr113 <- as.factor(df$ucr113)
df$ucr39 <- as.factor(df$ucr39)
df$ager52 <- as.factor(df$ager52)
df$ager27 <- as.factor(df$ager27)
df$marstat <- as.factor(df$marstat)
df$racer5 <- as.factor(df$racer5)

ages_continuous <- list(
    "1" = 1 / (24 * 365),
    "2" = 2 / (24 * 365),
    "3" = 1 / 365,
    "4" = 2 / 365,
    "5" = 3 / 365,
    "6" = 4 / 365,
    "7" = 5 / 365,
    "8" = 6 / 365,
    "9" = 10 / 365,
    "10" = 17 / 365,
    "11" = 24 / 365,
    "12" = 1 / 12,
    "13" = 2 / 12,
    "14" = 3 / 12,
    "15" = 4 / 12,
    "16" = 5 / 12,
    "17" = 6 / 12,
    "18" = 7 / 12,
    "19" = 8 / 12,
    "20" = 9 / 12,
    "21" = 10 / 12,
    "22" = 11 / 12,
    "23" = 1,
    "24" = 2,
    "25" = 3,
    "26" = 4,
    "27" = 7,
    "28" = 12,
    "29" = 17,
    "30" = 22,
    "31" = 27,
    "32" = 32,
    "33" = 37,
    "34" = 42,
    "35" = 47,
    "36" = 52,
    "37" = 57,
    "38" = 62,
    "39" = 67,
    "40" = 72,
    "41" = 77,
    "42" = 82,
    "43" = 87,
    "44" = 92,
    "45" = 97,
    "46" = 102,
    "47" = 107,
    "48" = 112,
    "49" = 117,
    "50" = 122,
    "51" = 125,
    "52" = 0
)

df %>% mutate(ages_cont = ages_continuous[ager52]) -> df

df$ages_cont <- as.numeric(df$ages_cont)

# vars: ages_cont: continuous
#       avg_record_count: continuous
#       sex: 2 levels
#       educ2003: 9 levels
#       monthdth: 12 levels
#       placdth: 8 levels
#       weekday: 7 levels
#       ucr358: 347 levels
#       ucr113: 112 levels
#       ucr39: 39 levels
#       ager27: 27 levels
#       marstat: 5 levels
#       racer5: 4 levels

## Tree
trainIndex  <- sample(1:nrow(df), 0.8 * nrow(df))
train <- df[trainIndex,]
test <- df[-trainIndex,]
startTime <- Sys.time()
tree <- rpart(manner_name ~ ages_cont + avg_record_count + sex + educ2003 + placdth + marstat + racer5, data = train, control = rpart.control(cp = .001)) # to make model more accurate, can add in monthdth and ucr39 and decrease cp
endTime <- Sys.time()
print(endTime - startTime)
# printcp(tree, digits = 3)
best <- tree$cptable[which.min(tree$cptable[, "xerror"]), "CP"]
pruned_tree <- prune(tree, cp = best)
prp(pruned_tree,
    faclen = 0, # use full names for factor labels
    extra = 1, # display number of obs. for each terminal node
    roundint = F, # don't round to integers in output
    digits = 4
) # display 5 decimal places in output

t_pred <- predict(tree, test, type = "class")
confMat <- table(test$manner_name, t_pred)
accuracy <- sum(diag(confMat)) / sum(confMat)
accuracy
## Linear Models

# fit_is_accident <- lm(is_accident ~ ages_cont + avg_record_count + sex + educ2003 + monthdth + placdth + weekday + ucr39 + ager52 + marstat + racer5, data = df)
fit_is_accident <- lm(is_accident ~ ages_cont + avg_record_count + sex + educ2003 + monthdth + placdth + ucr39 + marstat + racer5, data = df_small)
fit_is_suicide <- lm(is_suicide ~ ages_cont + avg_record_count + sex + educ2003 + monthdth + placdth + ucr39 + marstat + racer5, data = df_small)
fit_is_homicide <- lm(is_homicide ~ ages_cont + avg_record_count + sex + educ2003 + monthdth + placdth + ucr39 + marstat + racer5, data = df_small)
fit_is_pending <- lm(is_pending_investigation ~ ages_cont + avg_record_count + sex + educ2003 + monthdth + placdth + ucr39 + marstat + racer5, data = df_small)
fit_is_cnd <- lm(is_cnd ~ ages_cont + avg_record_count + sex + educ2003 + monthdth + placdth + ucr39 + marstat + racer5, data = df_small)
fit_self_inflicted <- lm(is_self_inflicted ~ ages_cont + avg_record_count + sex + educ2003 + monthdth + placdth + ucr39 + marstat + racer5, data = df_small)
fit_natural <- lm(is_natural ~ ages_cont + avg_record_count + sex + educ2003 + monthdth + placdth + ucr39 + marstat + racer5, data = df_small)
fit_not_specified <- lm(is_not_specified ~ ages_cont + avg_record_count + sex + educ2003 + monthdth + placdth + ucr39 + marstat + racer5, data = df_small)
sum(df_small$is_accident)
summary(fit_is_accident)$adj.r.squared
sum(df_small$is_suicide)
summary(fit_is_suicide)$adj.r.squared
sum(df_small$is_homicide)
summary(fit_is_homicide)$adj.r.squared
sum(df_small$is_pending_investigation)
summary(fit_is_pending)$adj.r.squared
sum(df_small$is_cnd)
summary(fit_is_cnd)$adj.r.squared
sum(df_small$is_self_inflicted)
summary(fit_self_inflicted)$adj.r.squared
sum(df_small$is_natural)
summary(fit_natural)$adj.r.squared
sum(df_small$is_not_specified)
summary(fit_not_specified)$adj.r.squared
