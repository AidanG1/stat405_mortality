# source("setup.r")

df <- query(
    "SELECT mandeath, educ2003, monthdth, sex, ager52, ager27, placdth, marstat, weekday, injwork, methdisp, autopsy, ucr358, ucr113, ucr39, racer5,
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
     FROM mortality"
)

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


df$avg_record_count <- as.numeric(df$avg_record_count)

df$mandeath[is.na(df$mandeath)] <- 8

df %>%
    mutate(
        ages_cont = ages_continuous[ager52],
        manner_ = as.character(mandeath),
        age_ = as.character(ager52),
        educ_ = as.character(educ2003),
        place_ = as.character(placdth),
        race_ = as.character(racer5),
        is_accident = mandeath == "1",
        is_suicide = mandeath == "2",
        is_homicide = mandeath == "3",
        is_pending_investigation = mandeath == "4",
        is_cnd = mandeath == "5",
        is_self_inflicted = mandeath == "6",
        is_natural = mandeath == "7",
        is_not_specified = mandeath == "8"
    ) %>%
    rowwise() %>%
    mutate(
        manner_name = manners[[manner_]],
        age = ages[[age_]],
        education = educations[[educ_]],
        place = places[[place_]],
        race = races[[race_]]
    ) %>%
    select(!ends_with("_")) -> df

df$manner_name <- as.factor(df$manner_name)
# df$mandeath <- as.factor(df$mandeath)
df$sex <- as.factor(df$sex)
# df$educ2003 <- as.factor(df$educ2003)
df$monthdth <- as.factor(df$monthdth)
# df$placdth <- as.factor(df$placdth)
df$weekday <- as.factor(df$weekday)
df$ucr358 <- as.factor(df$ucr358)
df$ucr113 <- as.factor(df$ucr113)
df$ucr39 <- as.factor(df$ucr39)
# df$ager52 <- as.factor(df$ager52)
# df$ager27 <- as.factor(df$ager27)
df$marstat <- as.factor(df$marstat)
# df$racer5 <- as.factor(df$racer5)
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


## Random Forest
run_random_forest <<- function() {
    library(randomForest)
    trainIndex <- sample(1:nrow(df), 0.05 * nrow(df))
    train <- df[trainIndex, ]
    test <- df[-trainIndex, ]
    model1 <- randomForest(
        manner_name ~ ages_cont + avg_record_count + ucr39,
        data = train
    )
    model2 <- randomForest(
        manner_name ~ ages_cont + avg_record_count + sex + education + place + marstat + race + monthdth,
        data = train
    )
    t_pred1 <- predict(model1, test, type = "class")
    t_pred2 <- predict(model2, test, type = "class")
    confMat1 <- table(test$manner_name, t_pred1)
    confMat2 <- table(test$manner_name, t_pred2)
    accuracy1 <- sum(diag(confMat1)) / sum(confMat1)
    accuracy2 <- sum(diag(confMat2)) / sum(confMat2)
    endTime <- Sys.time()
    # print(endTime - startTime)
    c(accuracy1, accuracy2)
}

run_decision_tree <<- function() {
    # rename so prints well
    df %>% rename(
        `Continuous Age` = ages_cont,
        `Manner of Death` = manner_name,
        `Education` = education,
        `Average Record Count` = avg_record_count,
        `Sex` = sex,
        Education = education,
        Place = place,
        `Marital Status` = marstat,
        Race = race,
        `Month of Death` = monthdth
    ) -> renamed_df
    library(rpart)
    library(rpart.plot)
    trainIndex <- sample(1:nrow(renamed_df), 0.8 * nrow(renamed_df))
    train <- renamed_df[trainIndex, ]
    test <- renamed_df[-trainIndex, ]
    startTime <- Sys.time()
    tree <- rpart(`Manner of Death` ~ `Continuous Age` + `Average Record Count` + Sex + Education + Place + `Marital Status` + Race + `Month of Death`, data = train, control = rpart.control(cp = .001)) # to make model more accurate, can add in monthdth and ucr39 and decrease cp
    # tree <- rpart(manner_name ~ ages_cont + avg_record_count + ucr39, data = train, method = "class", control = rpart.control(cp = .5))
    # printcp(tree, digits = 3)
    best <- tree$cptable[which.min(tree$cptable[, "xerror"]), "CP"]
    pruned_tree <- prune(tree, cp = best)
    prp(pruned_tree,
        varlen = 0, # use full names for factor labels
        faclen = 0,
        extra = 1, # display number of obs. for each terminal node
        roundint = F, # don't round to integers in output
        digits = 4
    ) # display 5 decimal places in output

    t_pred <- predict(tree, test, type = "class")
    confMat <- table(test$`Manner of Death`, t_pred)
    accuracy <- sum(diag(confMat)) / sum(confMat)
    endTime <- Sys.time()
    # print(endTime - startTime)
    accuracy
}

## Linear Models
run_logistic_regression <<- function() {
    df %>% rename(
        `Continuous Age` = ages_cont,
        `Manner of Death` = manner_name,
        `Education` = education,
        `Average Record Count` = avg_record_count,
        `Sex` = sex,
        Education = education,
        Place = place,
        `Marital Status` = marstat,
        Race = race,
        `Month of Death` = monthdth
    ) -> renamed_df
    library(pscl)
    trainIndex <- sample(1:nrow(renamed_df), 0.5 * nrow(renamed_df))
    train <- renamed_df[trainIndex, ]
    test <- renamed_df[-trainIndex, ]
    fit_is_accident <- glm(is_accident ~ `Continuous Age` + `Average Record Count` + Sex + Education + `Month of Death` + Place + ucr39 + `Marital Status` + `Race`, data = train, family = "binomial")
    fit_is_suicide <- glm(is_suicide ~ `Continuous Age` + `Average Record Count` + Sex + Education + `Month of Death` + Place + ucr39 + `Marital Status` + `Race`, data = train, family = "binomial")
    fit_is_homicide <- glm(is_homicide ~ `Continuous Age` + `Average Record Count` + Sex + Education + `Month of Death` + Place + ucr39 + `Marital Status` + `Race`, data = train, family = "binomial")
    fit_is_pending_investigation <- glm(is_pending_investigation ~ `Continuous Age` + `Average Record Count` + Sex + Education + `Month of Death` + Place + ucr39 + `Marital Status` + `Race`, data = train, family = "binomial")
    fit_is_cnd <- glm(is_cnd ~ `Continuous Age` + `Average Record Count` + Sex + Education + `Month of Death` + Place + ucr39 + `Marital Status` + `Race`, data = train, family = "binomial")
    # fit_self_inflicted <- glm(is_self_inflicted ~ `Continuous Age` + `Average Record Count` + Sex + Education + `Month of Death` + Place + ucr39 + `Marital Status` + `Race`, data = train, family="binomial")
    fit_natural <- glm(is_natural ~ `Continuous Age` + `Average Record Count` + Sex + Education + `Month of Death` + Place + ucr39 + `Marital Status` + `Race`, data = train, family = "binomial")
    fit_not_specified <- glm(is_not_specified ~ `Continuous Age` + `Average Record Count` + Sex + Education + `Month of Death` + Place + ucr39 + `Marital Status` + `Race`, data = train, family = "binomial")
    test_sample <- sample(1:nrow(test), 0.005 * nrow(test))
    test_sample <- test[test_sample, ]
    predicted <- predict(fit_is_accident, test_sample, type = "response")
    g3 <- ggplot(
        data.frame(
            predicted = predicted,
            actual = test_sample$is_accident
        ),
        aes(x = predicted, y = actual)
    ) +
        geom_point() +
        geom_abline(intercept = 0, slope = 1, color = "red")

    mcfaddens <- data.frame(
        "Manners of Death" = c(
            "Accident", "Suicide", "Homicide", "Pending Investigation", "Could Not Determine", "Natural", "Not Specified"
        ),
        "McFadden R^2" = c(
            pR2(fit_is_accident)["McFadden"],
            pR2(fit_is_suicide)["McFadden"],
            pR2(fit_is_homicide)["McFadden"],
            pR2(fit_is_pending_investigation)["McFadden"],
            pR2(fit_is_cnd)["McFadden"],
            # pR2(fit_self_inflicted)["McFadden"],
            pR2(fit_natural)["McFadden"],
            pR2(fit_not_specified)["McFadden"]
        ),
        counts = c(
            sum(df$is_accident),
            sum(df$is_suicide),
            sum(df$is_homicide),
            sum(df$is_pending_investigation),
            sum(df$is_cnd),
            sum(df$is_natural),
            sum(df$is_not_specified)
        )
    )

    g1 <- ggplot(mcfaddens, aes(
        x = labels, y = rs, fill = labels
    )) +
        geom_bar(stat = "identity")

    g2 <- ggplot(mcfaddens, aes(
        x = counts, y = rs, color = labels
    )) +
        geom_point()

    list(g1, g2, g3)
}
