# boxplots by manner of death and age

manner_by_age <- query("select ager52, mandeath, count(*) as count from mortality group by ager52, mandeath order by mandeath")

# do rescale
fn <- function(x) log(x + 1)
manner_by_age$count <- fn(manner_by_age$count)

manner_by_age$mandeath[is.na(manner_by_age$mandeath)] <- 8

manner_by_age <- manner_by_age %>% mutate(manner = manners[mandeath], age = ages[ager52])

manner_by_age_plot <- function() {
    g <- ggplot(manner_by_age, aes(ager52, mandeath)) +
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
    g
}
