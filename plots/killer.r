draw_person <- function(vp, head_scale, neck_scale,
                        left_arm_scale, right_arm_scale,
                        left_leg_scale, right_leg_scale,
                        colors = "black") {
    colors <- rep(colors, length.out = 6)
    pushViewport(vp)
    pushViewport(viewport(width = 0.6, height = 0.6))
    neck_end <- 11 / 16 + 1 / 16 * neck_scale

    ## Head
    radius <- 0.125 * head_scale
    grid.circle(x = 0.5, y = neck_end + radius, r = radius, gp = gpar(col = colors[1]))
    delta <- 1 / 50
    start_x <- 1 / 2 - 0.29 * radius ## 1/28
    start_x_right <- 1 / 2 + 0.29 * radius
    ## start_y <- 3/4 + 1/8 + 1/24
    start_y <- neck_end + 1.1 * radius
    grid.lines(c(start_x - delta, start_x + delta),
        c(start_y + delta, start_y - delta),
        gp = gpar(col = colors[1])
    )
    grid.lines(c(start_x - delta, start_x + delta),
        c(start_y - delta, start_y + delta),
        gp = gpar(col = colors[1])
    )
    grid.lines(c(start_x_right - delta, start_x_right + delta),
        c(start_y + delta, start_y - delta),
        gp = gpar(col = colors[1])
    )
    grid.lines(c(start_x_right - delta, start_x_right + delta),
        c(start_y - delta, start_y + delta),
        gp = gpar(col = colors[1])
    )

    ## Neck
    grid.lines(c(0.5, 0.5), c(11 / 16, neck_end),
        gp = gpar(col = colors[2])
    )

    ## Left arm
    grid.lines(c(0.5, 0.5 - 0.25 * left_arm_scale),
        c(0.5, 0.5 + 0.25 * left_arm_scale),
        gp = gpar(col = colors[3])
    )

    ## Right arm
    grid.lines(c(0.5, 0.5 + 0.25 * right_arm_scale),
        c(0.5, 0.5 + 0.25 * right_arm_scale),
        gp = gpar(col = colors[4])
    )

    ## Left leg
    grid.lines(c(0.5, 0.5 - 1 / 8 * left_leg_scale),
        c(5 / 16, 5 / 16 - 5 / 16 * left_leg_scale),
        gp = gpar(col = colors[5])
    )

    ## Right leg
    grid.lines(c(0.5, 0.5 + 1 / 8 * right_leg_scale),
        c(5 / 16, 5 / 16 - 5 / 16 * right_leg_scale),
        gp = gpar(col = colors[6])
    )

    ## Torso
    grid.lines(c(0.5, 0.5), c(5 / 16, 11 / 16), gp = gpar(col = "black"))
    popViewport()
    popViewport()
}

# classifications: mandeath & ager12 & martial status
df <- query("select
    mandeath,
    ager12,
    avg(CASE WHEN record_1 IS NOT NULL AND record_1 <> '' THEN 1 ELSE 0 END +
        CASE WHEN record_2 IS NOT NULL AND record_2 <> '' THEN 1 ELSE 0 END +
        CASE WHEN record_3 IS NOT NULL AND record_3 <> '' THEN 1 ELSE 0 END +
        CASE WHEN record_4 IS NOT NULL AND record_4 <> '' THEN 1 ELSE 0 END +
        CASE WHEN record_5 IS NOT NULL AND record_5 <> '' THEN 1 ELSE 0 END +
        CASE WHEN record_6 IS NOT NULL AND record_6 <> '' THEN 1 ELSE 0 END +
        CASE WHEN record_7 IS NOT NULL AND record_7 <> '' THEN 1 ELSE 0 END +
        CASE WHEN record_8 IS NOT NULL AND record_8 <> '' THEN 1 ELSE 0 END +
        CASE WHEN record_9 IS NOT NULL AND record_9 <> '' THEN 1 ELSE 0 END +
        CASE WHEN record_10 IS NOT NULL AND record_10 <> '' THEN 1 ELSE 0 END +
        CASE WHEN record_11 IS NOT NULL AND record_11 <> '' THEN 1 ELSE 0 END +
        CASE WHEN record_12 IS NOT NULL AND record_12 <> '' THEN 1 ELSE 0 END +
        CASE WHEN record_13 IS NOT NULL AND record_13 <> '' THEN 1 ELSE 0 END +
        CASE WHEN record_14 IS NOT NULL AND record_14 <> '' THEN 1 ELSE 0 END +
        CASE WHEN record_15 IS NOT NULL AND record_15 <> '' THEN 1 ELSE 0 END +
        CASE WHEN record_16 IS NOT NULL AND record_16 <> '' THEN 1 ELSE 0 END +
        CASE WHEN record_17 IS NOT NULL AND record_17 <> '' THEN 1 ELSE 0 END +
        CASE WHEN record_18 IS NOT NULL AND record_18 <> '' THEN 1 ELSE 0 END +
        CASE WHEN record_19 IS NOT NULL AND record_19 <> '' THEN 1 ELSE 0 END +
    CASE WHEN record_20 IS NOT NULL THEN 1 ELSE 0 END) AS avg_record_count
from mortality group by mandeath, ager12")

df$mandeath[is.na(df$mandeath)] <- 8

ager12s <- list(
    "1" = "Under 1 year",
    "2" = "1-4 years",
    "3" = "5-14 years",
    "4" = "15-24 years",
    "5" = "25-34 years",
    "6" = "35-44 years",
    "7" = "45-54 years",
    "8" = "55-64 years",
    "9" = "65-74 years",
    "10" = "75-84 years",
    "11" = "85 years and over",
    "12" = "Age not stated"
)

mandeaths <- list(
    "1" = "Accident",
    "2" = "Suicide",
    "3" = "Homicide",
    "4" = "Pending Investigation",
    "5" = "Could Not Determine",
    "6" = "Self-inflicted",
    "7" = "Natural",
    "8" = "Not Specified"
)

df <- df %>% mutate(manner = mandeaths[mandeath], age = ager12s[ager12])

df$age <- factor(df$age, levels = unique(df$age[order(df$ager12)]))

df$manner <- factor(df$manner, levels = unique(df$manner[order(df$mandeath)]))

rows <- 3
cols <- 4
title <- "Title"
labels.x <- c("A", "B", "C", "D")
labels.y <- c("A", "B", "C")

grid.newpage()
grid.lines(c(0.1, 0.9), c(0.1, 0.1))
grid.lines(c(0.1, 0.1), c(0.1, 0.9))
grid.text(title,
    x = 0.125, y = 0.925, just = c("left", "bottom"),
    gp = gpar(fontsize = 18)
)
pushViewport(viewport(
    x = 0.1, y = 0.1, just = c("left", "bottom"),
    width = 0.8, 0.8
))
ly <- grid.layout(rows, cols)
pushViewport(viewport(layout = ly))
for (i in 1:rows) {
    pushViewport(viewport(layout.pos.row = i))
    grid.text(labels.y[i], x = unit(-1, "lines"))
    popViewport()
    for (j in 1:cols) {
        pushViewport(viewport(layout.pos.col = j))
        grid.text(labels.x[j], y = unit(-1, "lines"))
        popViewport()
        vp <- viewport(layout.pos.row = i, layout.pos.col = j)
        draw_person(
            vp, i, 1, 1, 1, 1, 1,
            c("green", "red", "magenta", "orange", "pink", "blue")
        )
    }
}
