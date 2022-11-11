draw_person <- function(vp, head_scale, neck_scale,
                        left_arm_scale, right_arm_scale,
                        left_leg_scale, right_leg_scale,
                        colors = "black") {
    colors <- rep(colors, length.out = 6)
    pushViewport(vp)
    pushViewport(viewport(width = 0.6, height = 0.6))
    neck_end <- 11/16 + 1/16*neck_scale
    
    ## Head
    radius <- 0.125 * head_scale
    grid.circle(x = 0.5, y = neck_end + radius, r = radius, gp = gpar(col = colors[1]))
    delta <- 1/50
    start_x <- 1/2 - 0.29*radius ## 1/28
    start_x_right <- 1/2 + 0.29*radius
    ## start_y <- 3/4 + 1/8 + 1/24
    start_y <- neck_end + 1.1*radius
    grid.lines(c(start_x - delta, start_x + delta),
               c(start_y + delta, start_y - delta),
               gp = gpar(col = colors[1]))
    grid.lines(c(start_x - delta, start_x + delta),
               c(start_y - delta, start_y + delta),
               gp = gpar(col = colors[1]))
    grid.lines(c(start_x_right - delta, start_x_right + delta),
               c(start_y + delta, start_y - delta),
               gp = gpar(col = colors[1]))
    grid.lines(c(start_x_right - delta, start_x_right + delta),
               c(start_y - delta, start_y + delta),
               gp = gpar(col = colors[1]))
    
    ## Neck
    grid.lines(c(0.5, 0.5), c(11/16, neck_end),
               gp = gpar(col = colors[2]))
    
    ## Left arm
    grid.lines(c(0.5, 0.5 - 0.25 * left_arm_scale),
               c(0.5, 0.5 + 0.25 * left_arm_scale), gp = gpar(col = colors[3]))
    
    ## Right arm
    grid.lines(c(0.5, 0.5 + 0.25 * right_arm_scale),
               c(0.5, 0.5 + 0.25 * right_arm_scale), gp = gpar(col = colors[4]))
    
    ## Left leg
    grid.lines(c(0.5, 0.5 - 1/8*left_leg_scale),
               c(5/16, 5/16 - 5/16*left_leg_scale), gp = gpar(col = colors[5]))
    
    ## Right leg
    grid.lines(c(0.5, 0.5 + 1/8*right_leg_scale),
               c(5/16, 5/16 - 5/16*right_leg_scale), gp = gpar(col = colors[6]))
    
    ## Torso
    grid.lines(c(0.5, 0.5), c(5/16, 11/16), gp = gpar(col = "black"))
    popViewport()
    popViewport()
}


rows <- 3; cols <- 4
title <- "Title"
labels.x <- c("A", "B", "C", "D")
labels.y <- c("A", "B", "C")

grid.newpage()
grid.lines(c(0.1, 0.9), c(0.1, 0.1))
grid.lines(c(0.1, 0.1), c(0.1, 0.9))
grid.text(title, x = 0.125, y = 0.925, just = c("left", "bottom"),
          gp = gpar(fontsize = 18))
pushViewport(viewport(x = 0.1, y = 0.1, just = c("left", "bottom"),
                      width = 0.8, 0.8))
ly = grid.layout(rows, cols)
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
        draw_person(vp, i, 1, 1, 1, 1, 1,
                    c("green", "red", "magenta", "orange", "pink", "blue"))
    }
}
