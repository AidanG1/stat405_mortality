deaths_by_age_sex <- query("select ager52, sex, count(*) as count from mortality group by ager52, sex order by ager52")

ages <- list(
    "1" = "Under 1 hour",
    "2" = "1-23 hours",
    "3" = "1 day",
    "4" = "2 days",
    "5" = "3 days",
    "6" = "4 days",
    "7" = "5 days",
    "8" = "6 days",
    "9" = "7-13 days",
    "10" = "14-20 days",
    "11" = "21-27 days",
    "12" = "1 month",
    "13" = "2 months",
    "14" = "3 months",
    "15" = "4 months",
    "16" = "5 months",
    "17" = "6 months",
    "18" = "7 months",
    "19" = "8 months",
    "20" = "9 months",
    "21" = "10 months",
    "22" = "11 months",
    "23" = "1 year",
    "24" = "2 years",
    "25" = "3 years",
    "26" = "4 years",
    "27" = "5-9 years",
    "28" = "10-14 years",
    "29" = "15-19 years",
    "30" = "20-24 years",
    "31" = "25-29 years",
    "32" = "30-34 years",
    "33" = "35-39 years",
    "34" = "40-44 years",
    "35" = "45-49 years",
    "36" = "50-54 years",
    "37" = "55-59 years",
    "38" = "60-64 years",
    "39" = "65-69 years",
    "40" = "70-74 years",
    "41" = "75-79 years",
    "42" = "80-84 years",
    "43" = "85-89 years",
    "44" = "90-94 years",
    "45" = "95-99 years",
    "46" = "100-104 years",
    "47" = "105-109 years",
    "48" = "110-114 years",
    "49" = "115-119 years",
    "50" = "120-124 years",
    "51" = "124+ years",
    "52" = "Age not stated"
)

deaths_by_age_sex <- deaths_by_age_sex %>% mutate(age = ages[ager52])

deaths_by_age_sex$age <- factor(
    deaths_by_age_sex$age,
    levels = unique(deaths_by_age_sex$age[
        order(deaths_by_age_sex$ager52, deaths_by_age_sex$sex)
    ])
)

sr <- (deaths_by_age_sex %>% filter(sex == "M") %>% select(count) / (deaths_by_age_sex %>% filter(sex == "F") %>% select(count) + deaths_by_age_sex %>% filter(sex == "M") %>% select(count)))$count

s_labels <- c()

for (val in sr) {
    s_labels <- c(s_labels, paste0(round(val, digits = 4), "%"), '')
}

ggplot(deaths_by_age_sex, aes(
    x = age, y = count, fill = sex
)) +
    geom_bar(stat = "identity") +
    geom_text(aes(label = s_labels), hjust=-0.1, angle = 90) +
    labs(x = "Age Range", y = "Number of Deaths") +
    ggtitle("Deaths by Age on a Log Scale with Percent Male") +
    theme(
        axis.text.x = element_text(angle = 60, hjust = 1, vjust = 0.5),
        axis.title.x = element_text(margin = margin(t = 1, unit = "cm")),
        legend.position = "bottom"
    ) +
    scale_y_log10()
