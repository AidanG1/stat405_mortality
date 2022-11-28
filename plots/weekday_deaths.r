deaths_by_day <- query("select weekday, count(*) as count from mortality where weekday != 9 group by weekday order by weekday")

deaths_by_day <- cbind(deaths_by_day, data.frame(day = c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday")))

deaths_by_day_plot <<- function() {
    ggplot(deaths_by_day, aes(x = day, y = count, fill = day)) +
        geom_bar(stat = "identity") +
        labs(x = "Day of the Week", y = "Number of Deaths") +
        ggtitle("Deaths by Day of the Week")
}

# sort df by weekday
deaths_by_day$day <- factor(deaths_by_day$day, levels = deaths_by_day$day[order(deaths_by_day$weekday)])

deaths_by_day_plot_scaled <<- function() {
    ggplot(deaths_by_day, aes(x = day, y = count - 400000, fill = day)) +
        geom_bar(stat = "identity") +
        labs(x = "Day of the Week", y = "Number of Deaths - 400000") +
        ggtitle("Deaths by Day of the Week (Scaled)")
}
