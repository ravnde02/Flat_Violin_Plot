require(ggplot2)
require(ggsignif)
require(dplyr)
require(EnvStats)

"%||%" <- function(a, b) {
  if (!is.null(a)) a else b
}

geom_flat_violin <- function(mapping = NULL, data = NULL, stat = "ydensity",
                        position = "dodge", trim = TRUE, scale = "area",
                        show.legend = NA, inherit.aes = TRUE, ...) {
  layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomFlatViolin,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      trim = trim,
      scale = scale,
      ...
    )
  )
}

GeomFlatViolin <-
  ggproto("GeomFlatViolin", Geom,
          setup_data = function(data, params) {
            data$width <- data$width %||%
              params$width %||% (resolution(data$x, FALSE) * 2)

            # ymin, ymax, xmin, and xmax define the bounding rectangle for each group
            data %>%
              group_by(group) %>%
              mutate(ymin = min(y),
                     ymax = max(y),
                     xmin = x + width/2.5,
                     xmax = x)
          },

          draw_group = function(data, panel_scales, coord) {
            # Find the points for the line to go all the way around
            data <- transform(data, 
                              xmaxv = x,
                              xminv = x + violinwidth * (xmin - x))

            # Make sure it's sorted properly to draw the outline
            newdata <- rbind(plyr::arrange(transform(data, x = xminv), y),
                             plyr::arrange(transform(data, x = xmaxv), -y))

            # Close the polygon: set first and last point the same
            # Needed for coord_polar and such
            newdata <- rbind(newdata, newdata[1,])

            ggplot2:::ggname("geom_flat_violin", GeomPolygon$draw_panel(newdata, panel_scales, coord))
          },

          draw_key = draw_key_polygon,

          default_aes = aes(weight = 1, colour = "grey20", fill = "white",
          	size = 0.5, alpha = NA),

          required_aes = c("x", "y")
)

flatviolinplot_line <- function(data, title, ylab, xlab) {
	xaxes <- data[,1]
	yaxes <- data[,2]
	
	violin <- ggplot(data, aes(x = xaxes, y = yaxes, fill = xaxes)) + 
	geom_flat_violin(trim = FALSE, na.rm = TRUE, lwd = 0.25) +
	geom_boxplot(width = 0.1, fill = "white", lwd = 1) +
	labs(title = title, x = xlab, y = ylab) +
	theme_minimal() +
	theme(legend.position = "none", axis.title = element_text(size = 14),
			axis.text = element_text(size = 12, face = "plain"), 
			plot.title = element_text(hjust = 0.5, face = "bold", size = 18),
			axis.line = element_line(colour = "black", size = 0.5))
violin
}