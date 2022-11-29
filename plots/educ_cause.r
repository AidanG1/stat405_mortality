educ_cause <- query("
SELECT educ2003, ucr39, count(*) as count
FROM mortality
GROUP BY educ2003, ucr39
ORDER BY educ2003;")

education_tags <- c(
    "8th grade or less",
    "9 - 12th grade, no diploma",
    "high school graduate or GED completed",
    "some college credit, but no degree",
    "Associate degree",
    "Bachelor's degree",
    "Master's degree",
    "Doctorate or professional degree",
    "Unknown"
)

cause_tags <- c(
    "Tuberculosis",
    "Syphilis",
    "Human immunodeficiency virus (HIV) disease",
    "Malignant neoplasms",
    "Malignant neoplasm of stomach",
    "Malignant neoplasms of colon, rectum and anus",
    "Malignant neoplasm of pancreas",
    "Malignant neoplasms of trachea, bronchus and lung",
    "Malignant neoplasm of breast",
    "Malignant neoplasms of cervix uteri, corpus uteri and ovary",
    "Malignant neoplasm of prostate",
    "Malignant neoplasms of urinary tract",
    "Non-Hodgkin's lymphoma",
    "Leukemia",
    "Other malignant neoplasms",
    "Diabetes mellitus",
    "Alzheimer's disease",
    "Major cardiovascular diseases",
    "Diseases of heart",
    "Hypertensive heart disease with or without renal disease",
    "Ischemic heart diseases",
    "Other diseases of heart",
    "Essential",
    "Cerebrovascular diseases",
    "Atherosclerosis",
    "Other diseases of circulatory system",
    "Influenza and pneumonia",
    "Chronic lower respiratory diseases",
    "Peptic ulcer",
    "Chronic liver disease and cirrhosis",
    "Nephritis, nephrotic syndrome, and nephrosis",
    "Pregnancy, childbirth and the puerperium",
    "Certain conditions originating in the perinatal period",
    "Congenital malformations, deformations and chromosomal abnormalities",
    "Sudden infant death syndrome",
    "Symptoms, signs and abnormal clinical and laboratory findings, not elsewhere classified ",
    "All other diseases (Residual)",
    "Motor vehicle accidents",
    "All other and unspecified accidents and adverse effects",
    "Intentional self-harm (suicide)",
    "Assault (homicide)",
    "All other external causes"
)

totals <- tapply(educ_cause$count, list(educ_cause$educ2003), sum)

educ_cause %>% mutate(
    education = education_tags[educ2003],
    cause = cause_tags[ucr39],
    prop = unname(count / totals[educ2003])
) -> educ_cause

plot_educ_cause <- function() {
    educ_cause %>% mutate(cause = str_trunc(cause, 23)) %>% 
    ggplot() +
        geom_count(aes(y = cause, x = education, size = prop, color = educ2003)) +
        ggtitle("Cause of Death by Education Level") + 
        labs(
            x = "Education Level",
            y = "Cause of Death"
        ) +
        theme(
            axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)
        ) +
        guides(size = guide_legend(
            title = "Proportion",
            override.aes = list(color = "#56B1F7")
        )) +
        scale_color_gradient(guide = "none") +
        scale_x_discrete(limits = education_tags)
}

educ_cause_table <- function() {
    educ_cause %>% select(education, cause, prop) %>% spread(education, prop) %>% 
        kable()
}

# get the biggest disease outliers by education
# View(educ_cause %>% group_by(ucr39) %>% summarise((quantile(prop, 0.75) - quantile(prop, 0.25)) / median(prop), (max(prop) - min(prop)) / median(prop)))
