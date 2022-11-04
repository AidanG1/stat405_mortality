# boxplots by manner of death and age

manner_by_age <- query("select ager52, mandeath, count(*) as count from mortality group by ager52, mandeath order by mandeath")

# do rescale
fn <- function(x) log(x + 1)
manner_by_age$count <- fn(manner_by_age$count)

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

ages <- list(
    "1" = "Under 1 hour",
    "2" = "1-23 hours",
    "3" = "1 day",
    "4" = "2 days", "
    '5'= 3 days",
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

manner_by_age$mandeath[is.na(manner_by_age$mandeath)] <- 8

manner_by_age <- manner_by_age %>% mutate(manner = manners[mandeath], age = ages[ager52])

ggplot(manner_by_age, aes(ager52, mandeath)) +
    geom_tile(aes(fill = count), color = "aquamarine") +
    scale_fill_gradient(low = "aquamarine", high = "dodgerblue") +
    scale_x_continuous(breaks = seq(1, 52, 1), labels = ages) +
    scale_y_continuous(breaks = seq(1, 8, 1), labels = manners) +
    theme(
        axis.text.x = element_text(angle = 60, hjust = 1, vjust = 0.5),
        axis.title.x = element_text(margin = margin(t = 1, unit = "cm")),
    ) +
    ggtitle("Manner of Death by Age on a log scale") +
    labs(x = "Age", y = "Manner of Death")
