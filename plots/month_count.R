month_counts <- query("select monthdth, count(*) as count from mortality group by monthdth order by monthdth;")

days <- c(
    31, # J
    28, # F
    31, # Mar
    30, # A
    31, # May
    30, # June
    31, # July
    31, # August
    30, # Sep
    31, # Oct
    30, # Nov
    31 # Dec
)

names <- c(
    "January",
    "February",
    "March",
    "April",
    "May",
    "June",
    "July",
    "August",
    "September",
    "October",
    "November",
    "December"
)

month_counts %>% mutate(count = count / days[monthdth], monthdth_name = names[monthdth]) -> month_counts
month_counts$monthdth_name <- factor(month_counts$monthdth_name, levels = month_counts$monthdth_name[order(month_counts$monthdth)])

ggplot(month_counts) +
    geom_bar(aes(x = monthdth_name, weight = count, fill = monthdth_name)) +
    labs(
        title = "Deaths per Day in Month",
        x = "Month", y = "Deaths per day"
    ) +
    theme(
        axis.text.x = element_text(angle = 60, hjust = 1, vjust = 1)
    ) +
    scale_fill_discrete(guide = "none")
