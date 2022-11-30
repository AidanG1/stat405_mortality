cancer <- query("
SELECT Question, avg(Data_value) as avg_data, Break_Out FROM brfss
WHERE Question REGEXP 'cancer|depression|tobacco|heart' AND Sample_Size > 50
      AND Data_value IS NOT NULL AND Break_Out_Category = 'Age Group'
      AND Response = 'Yes'
GROUP BY Break_Out, Question;
")
cancer$Break_Out <- as.factor(cancer$Break_Out)


cancer_free_text_plot <- function() {
    g <- ggplot(cancer, aes(group = Question)) +
        geom_line(aes(x = Break_Out, y = avg_data, color = Question, size = I(2))) +
        ggtitle("Conditions by Age Group") +
        labs(
            x = "Age Group", y = "Percent of Age Group"
        ) +
        theme(
            legend.position = "bottom",
            legend.direction = "vertical"
        )
    g
}
