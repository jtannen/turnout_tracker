library(ggthemes)
library(colorspace)

asnum <- function(x) as.numeric(as.character(x))

strip_leading_zero <- function(x) gsub("^(0|\\s)+", "", x)
pretty_time <- function(x) strip_leading_zero(format(x, "%I:%M %p"))

safe_load <- function(file){
  e <- new.env()
  load(file, envir = e)
  if(length(e) == 1) return(e[[names(e)]]) else return(as.list(e))
}

format_name <- function(x){
  x <- gsub("(^\\s+)|(\\s+$)", "", x)
  x <- gsub("\\s+", " ", x)
  x <- gsub("\\b([[:alpha:]])([[:alpha:]]*)\\b", "\\U\\1\\L\\2", x, perl = TRUE)
  x <- gsub("\\b([A-Za-z?-??-??-?])([A-Za-z?-??-??-?]+)\\b", "\\U\\1\\L\\2", x, perl = TRUE)
  x <- gsub("\\bMc([a-z])", "Mc\\U\\1", x, perl=TRUE)
  x <- gsub(" Of ", " of ", x)
  x <- gsub(" The ", " the ", x)
  x <- gsub(" In ", " in ", x)
  return(x)
}


get_last_name <- function(x){
  gsub(".*\\s(\\S+)$", "\\1", x)
}

# from ggthemes_data$fivethirtyeight
dark_grey <- '#3C3C3C'
medium_grey <- '#D2D2D2'

theme_sixtysix <- function (base_size = 12, base_family = "sans") {
  (theme_foundation(base_size = base_size, base_family = base_family) + 
     theme(
       line = element_line(colour = "black"), 
       rect = element_rect(
         fill = "white", 
         linetype = 0, 
         colour = NA
       ), 
       text = element_text(colour = dark_grey), 
       axis.ticks = element_blank(), axis.line = element_blank(), 
       legend.background = element_rect(), legend.position = "bottom", 
       legend.direction = "horizontal", 
       legend.box = "horizontal", 
       panel.grid = element_line(colour = NULL), 
       panel.grid.major = element_line(colour = medium_grey), 
       panel.grid.minor = element_blank(), 
       plot.title = element_text(
         hjust = 0, 
         size = rel(1.5), 
         face = "bold"
       ), 
       plot.margin = unit(
         c(1, 1, 1, 1), 
         "lines"
       ), 
       strip.background = element_rect()
     )
  )
}

theme_map_sixtysix <- function (base_size = 12, base_family = "sans") {
	(theme_sixtysix(base_size = base_size, base_family = base_family) %+replace% 
        theme(line = element_blank(), axis.line = element_blank(), axis.text = element_blank(), 
            axis.ticks = element_blank(), axis.title = element_blank(), 
            panel.background = element_blank(), panel.border = element_blank(), 
            panel.grid = element_blank(), panel.spacing = unit(0, 
                "lines"), plot.background = element_blank(), 
            legend.justification = c(0, 0),
            legend.direction = "vertical",
            legend.position = c(0.7, 0.1)))}

library(png)
library(gridExtra)
library(grid)

try(
  logo <- readPNG("C:/Users/Jonathan Tannen/Dropbox/sixty_six/admin_scripts/images/logo_plus_text.png"),
  silent=TRUE
)

red <- "#FF5675"
light_blue = "#EDF7FF"
red_hcl <- as(hex2RGB(red), 'polarLUV')
colors_hcl <- red_hcl@coords[rep(1,5),]
colors_hcl[,'H'] <- c(5, 253, 65, 171, 300)
colors_hcl <- polarLUV(colors_hcl)
colors_66 <- hex(colors_hcl, fixup = TRUE)

drab_red <- hex(polarLUV(H = 5, L = 100, C = 0), fixup = TRUE)
strong_red <- hex(polarLUV(H = 5, L = 50, C = 100), fixup = TRUE)
drab_blue <- hex(polarLUV(H = 253, L = 100, C = 0), fixup = TRUE)
strong_blue <- hex(polarLUV(H = 253, L = 50, C = 100), fixup = TRUE)
drab_orange <- hex(polarLUV(H = 65, L = 100, C = 0), fixup = TRUE)
strong_orange <- hex(polarLUV(H = 65, L = 50, C = 100), fixup = TRUE)
drab_green <- hex(polarLUV(H = 171, L = 100, C = 0), fixup = TRUE)
strong_green <- hex(polarLUV(H = 171, L = 50, C = 100), fixup = TRUE)
drab_purple <- hex(polarLUV(H = 310, L = 100, C = 0), fixup = TRUE)
strong_purple <- hex(polarLUV(H = 310, L = 50, C = 100), fixup = TRUE)
strong_grey <- hex(polarLUV(H = 5, L = 50, C = 0))


light_red <- hex(polarLUV(H = 5, L = 60, C = 100), fixup = TRUE)
light_blue <- hex(polarLUV(H = 253, L = 60, C = 100), fixup = TRUE)
light_orange <- hex(polarLUV(H = 65, L = 60, C = 100), fixup = TRUE)
light_green <- hex(polarLUV(H = 171, L = 60, C = 100), fixup = TRUE)
light_purple <- hex(polarLUV(H = 310, L = 60, C = 100), fixup = TRUE)
light_grey <- hex(polarLUV(H = 5, L = 60, C = 0))


med_purple <- hex(polarLUV(H = 310, L = 100, C = 50), fixup = TRUE)
