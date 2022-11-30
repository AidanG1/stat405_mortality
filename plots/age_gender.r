deaths_by_age_sex <- query("select ager52, sex, count(*) as count from mortality group by ager52, sex order by ager52")

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
    s_labels <- c(s_labels, paste0(round(val, digits = 4), "%"), "")
}

deaths_by_age_gender_plot <- function() {
    g <- ggplot(deaths_by_age_sex, aes(
        x = age, y = count, fill = sex
    )) +
        geom_bar(stat = "identity") +
        geom_text(aes(y = 1, label = s_labels), hjust = -0.1, angle = 90) +
        labs(x = "Age Range", y = "Number of Deaths") +
        ggtitle("Deaths by Age on a Log Scale with Percent Male") +
        theme(
            axis.text.x = element_text(angle = 60, hjust = 1, vjust = 0.5),
            axis.title.x = element_text(margin = margin(t = 1, unit = "cm")),
            legend.position = "bottom"
        ) +
        scale_y_log10()
    g
}
