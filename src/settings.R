
# gdtools::sys_fonts()
# library(cairo)
# library(extrafont)
# font_import(pattern = ".*Adventor.*", prompt = FALSE)
# loadfonts(quiet = TRUE)
# loadfonts(device="postscript", quiet = TRUE)

# Settings
fig_height <- 6 # inches
fig_aspect_ratio_pres <- 16/9

theme_paper <- hrbrthemes::theme_ipsum(base_family = "Arial") %+replace%
    theme(
        plot.title = element_text(size = rel(2), face = "plain", hjust = 0, margin = margin(0,0,5,0)),
        plot.subtitle = element_text(size = rel(1), face = "plain", hjust = 0, margin = margin(0,0,5,0)),
        legend.title = element_blank(),
        legend.position = c(0.15,0.9),
        legend.background = element_rect(fill = "white", color = "grey80"),
        legend.margin = margin(1,5,5,5),
        # legend.box.margin = margin(0,0,0,0),
        legend.direction = "vertical",
        # legend.justification = c(0,0),
        # legend.box.just = "left",
        # legend.text.align = 0,
        # legend.spacing = unit(0, "pt")
        # axis.title = element_text(size = rel(1.5)),
        # axis.text = element_text(size = rel(1)),

        # Panels
        # panel.grid.major = element_line(linetype = "dotted", color = "grey60", size = 0.2),
        # panel.grid.minor = element_line(linetype = "dotted", color = "grey80", size = 0.2)
    )

theme_set(theme_paper)
