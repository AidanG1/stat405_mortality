month_counts <- query("select monthdth, mandeath, count(*) as count from mortality group by monthdth, mandeath order by monthdth;")

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

month_counts$mandeath[is.na(month_counts$mandeath)] <- 8
month_counts %>% mutate(count_scaled = count / days[monthdth], monthdth_name = names[monthdth]) -> month_counts

manner_names <- unlist(lapply(
    month_counts$mandeath,
    function(x) {
        manners[[paste0(x)]]
    }
))

month_counts$manner_name <- manner_names

month_counts$monthdth_name <- factor(month_counts$monthdth_name, levels = unique(month_counts$monthdth_name[order(month_counts$monthdth)]))

# ggplot(month_counts, aes(x = monthdth, y = count_scaled, group = mandeath, color = manner_name)) +
#     geom_line()

# ggplot(month_counts %>% filter(mandeath == 3), aes(x = monthdth, y = count_scaled)) +
#     geom_line()

ggplot(month_counts, aes(monthdth, count_scaled, color = manner_name)) +
    geom_line() +
    # scale_x_discrete(limits = month_counts$monthdth_name) +
    facet_wrap(vars(manner_name), scales = "free") +
    labs(
        x = "Month", y = "Deaths per day"
    ) +
    ggtitle("Deaths per Day in Month by Manner")
