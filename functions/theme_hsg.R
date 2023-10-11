#' define theme with HSG colors 
#' 

# COLOR: add, remove, or edit the colors to fit your scheme. Names should be

#' hsg Theme Color Palette
#'
#' @format character vector of hex code strings
#' @export
#'
#' @examples
#' hsg_theme_colors
#'
hsg_theme_colors <- c(
  text    = '#2d5c3d',
  panel   = '#a7c9b6',
  border  = '#2d5c3d',
  lighter = '#eff3f1',
  light   = '#ccded4',
  medium  = '#7ea88f',
  dark    = '#2d5c3d',
  darker  = "#021a0c"
)

# THEME: rename function and theme() arguments according to your theme design, feel free to edit this how you would like

#' HSG Inspired Theme
#'
#' @param hsg_font should `theme_hsg` use Google Font's Sansita Swashed? Default is `TRUE`.
#' @param ... additional parameters to pass to `ggplot2::theme()`
#'
#' @return a `ggplot2` `theme` element
#' @export
#'
#' @examples
#' library(ggplot2)
#'
#' ggplot(data = data.frame(x = rnorm(50, 0, 1), y = rnorm(50,0,1)), aes(x = x, y = y)) +
#'   geom_smooth(method = 'lm') +
#'   geom_point() +
#'   labs(title = 'hsg Scatter Plot') +
#'   theme_hsg()
#'
theme_hsg <- function(...){
  
  # CUSTOM FONT: add a custom font from google fonts
  
  # CUSTOM THEME:
  ggplot2::theme(
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank(),
    text = element_text(color = hsg_theme_colors["text"], family = "sans"),
    title = element_text(size=20),
    panel.background = element_rect(fill = hsg_theme_colors["lighter"]),
    panel.border = element_rect(fill = NA, color = hsg_theme_colors["border"],linewidth=1.2),
    axis.title = element_text(size=17),
    axis.text = element_text(size=13,color = hsg_theme_colors["text"]),
    axis.ticks = element_line(color = hsg_theme_colors["darker"],linewidth=1),
    legend.background = element_rect(fill = hsg_theme_colors["lighter"], colour = hsg_theme_colors["border"]),
    legend.box.background = element_rect(colour = hsg_theme_colors["border"]),
    strip.background = element_rect(fill = hsg_theme_colors["lighter"], colour = hsg_theme_colors["border"]),
    strip.text = element_text(colour = hsg_theme_colors["darker"]),
    ...
  )
}

# COLOR SCALES: Make pretty color scales

#' hsg Inspired Color Scales
#'
#' @param ... Additional arguments to pass to `ggplot2::scale_[fill/color]_gradient()`
#'
#' @return a `ggplot` scale object
#'
#' @rdname scale_hsg
#' @export
#'
#' @examples
#' library(ggplot2)
#'
#' ggplot(mpg) +
#'   geom_point(aes(y = class, x = hwy, color = cyl)) +
#'   labs(title="MPG by Vehicle Type",
#'        caption="Source: mpg",
#'        x = "City Mileage",
#'        color ="# Cylinders") +
#'   scale_color_hsg()
#'
scale_fill_hsg <- function(...) {
  ggplot2::scale_fill_gradient(low = '#a7c9b6', high = "#021a0c", ...)
}

#' @rdname scale_hsg
#' @export
scale_color_hsg <- function(...) {
  ggplot2::scale_color_gradient(low = '#a7c9b6', high = "#021a0c", ...)
}

