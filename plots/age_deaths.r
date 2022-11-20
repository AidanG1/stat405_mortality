deaths_by_age <- query("select ager52, count(*) as count from mortality group by ager52 order by ager52")

deaths_by_age <- cbind(deaths_by_age, data.frame(age = c("Under 1 hour", "1-23 hours", "1 day", "2 days", "3 days", "4 days", "5 days", "6 days", "7-13 days", "14-20 days", "21-27 days", "1 month", "2 months", "3 months", "4 months", "5 months", "6 months", "7 months", "8 months", "9 months", "10 months", "11 months", "1 year", "2 years", "3 years", "4 years", "5-9 years", "10-14 years", "15-19 years", "20-24 years", "25-29 years", "30-34 years", "35-39 years", "40-44 years", "45-49 years", "50-54 years", "55-59 years", "60-64 years", "65-69 years", "70-74 years", "75-79 years", "80-84 years", "85-89 years", "90-94 years", "95-99 years", "100-104 years", "105-109 years", "110-114 years", "115-119 years", "Age not stated")))

deaths_by_age$age <- factor(deaths_by_age$age, levels = deaths_by_age$age[order(deaths_by_age$ager52)])

ggplot(deaths_by_age, aes(x = age, y = count, fill = age)) +
    geom_bar(stat = "identity") +
    labs(x = "Age Range", y = "Number of Deaths") +
    ggtitle("Deaths by Age") +
    theme(
        axis.text.x = element_text(angle = 60, hjust = 1, vjust = 0.5),
        axis.title.x = element_text(margin = margin(t = 1, unit = "cm")),
        legend.position = "bottom"
    )
