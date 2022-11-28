df_month_counts <<- query("select monthdth, count(*) as count from mortality group by monthdth order by monthdth;")

df_month_counts_filtered <<- query("select monthdth, count(*) as count from mortality where mandeath != 7 group by monthdth order by monthdth;")

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

month_count_plot <<- function(df, title = "Deaths per Day in Month") {
    df %>% mutate(count_scaled = count / days[monthdth], monthdth_name = names[monthdth]) -> df
    df$monthdth_name <- factor(df$monthdth_name, levels = df$monthdth_name[order(df$monthdth)])

    ggplot(df) +
        geom_bar(aes(x = monthdth_name, weight = count_scaled, fill = monthdth_name)) +
        labs(
            x = "Month", y = "Deaths per day"
        ) +
        ggtitle(title) +
        theme(
            axis.text.x = element_text(angle = 60, hjust = 1, vjust = 1)
        ) +
        scale_fill_discrete(guide = "none")
}

month_count_table <<- function() {
    months_counts <- df_month_counts %>% mutate(count_scaled = count / days[monthdth], monthdth_name = names[monthdth])
    months_counts_filtered <- df_month_counts_filtered %>% mutate(count_scaled = count / days[monthdth], monthdth_name = names[monthdth])
    merge(months_counts, months_counts_filtered, by = "monthdth_name") %>% select(monthdth_name, count_scaled.x, count_scaled.y) %>% rename("Month" = monthdth_name, "Death Count" = count_scaled.x, "Death Count Excluding Natural" = count_scaled.y) %>% kable()
}