library(tidyverse)
library(camcorder)
library(magick)
library(ggimage)
library(ggtext)
library(ggfx)

# Bring in data
menken_raw <- read_csv("alan_menken_raw.csv")

menken <- 
  menken_raw %>% 
  select(track.uri,
         track.name,
         track.popularity,
         track.album.name,
         mode_name:project,
         key_mode
         ) %>% 
  separate_longer_delim(lyricist, delim = ";") %>% 
  filter(duplicate == "no")

top_lyricists <- 
  menken %>% 
  count(lyricist, project) %>% 
  summarise(n_project = n(),
            n_song = sum(n),
            .by = lyricist) %>% 
  arrange(-n_song) %>% 
  top_n(n = 7, n_song)

# It might make sens for the Glenn Slater line to be 10 inches long
# So, the Howard Ashman line should be 10 * 64 / 164 = 3.90
(10 * 64 / 164) %>% round(digits = 1)
# The Tim Rice line should be 10 * 54 / 164 = 3.3
(10 * 54 / 164) %>% round(digits = 1)
# The Stephen Schwartz line should be 10 * 53 / 164 = 3.2
(10 * 53 / 164) %>% round(digits = 1)
# The David Spencer line should be 10 * 50 / 164 = 3.0
(10 * 50 / 164) %>% round(digits = 1)
# The David Zippel line should be 10 * 32 / 164 = 2.0
(10 * 32 / 164) %>% round(digits = 1)
# The Lynn Ahrens line should be 10 * 24 / 164 = 1.5
(10 * 24 / 164) %>% round(digits = 1)
# The Jack Feldman line should be 10 * 23 / 164 = 1.4
(10 * 23 / 164) %>% round(digits = 1)

# Set the margins in Logic so that the padding + margin (i.e., 
# the white space) is 0.25 inches on the left and right. Then add 
# 0.5 inches of buffer to the size that it should be.

# Check most common keys for each lyricist
menken %>% 
  group_by(lyricist) %>% 
  filter(!is.na(key_mode)) %>% 
  count(key_mode, sort = TRUE) %>% 
  slice_max(n, n = 1) %>% 
  arrange(-n) %>% View()
# Glenn Slater = F
# Howard Ashman = F
# Tim Rice = C
# Stephen Schwartz = C
# David Spencer = F
# David Zippel = C
# Lynn Ahrens = C
# Jack Feldman = G


# Set up visualization window
temp_directory <- tempdir()

gg_record(
  dir = file.path(temp_directory, "recording100"), # where to save the recording
  device = "png", # device to use to save images
  width = 10,      # width of saved image
  height = 5,     # height of saved image
  units = "in",   # units for width and height
  dpi = 320       # dpi to use when saving image
)

gg_resize_film(
  width = 10,
  height = 7,
  units = "in",
  dpi = 300 #change to something at least 2e3 when actually ready to save
)


# Add images to dataframe
plot_data <- 
  top_lyricists %>% 
  mutate(image_path = paste0("sheet music snippets/pictures/",
                                tolower(lyricist), ".pdf")) %>% 
  mutate(n_project_unicode = stringi::stri_unescape_unicode(paste0("\\u", "E08", n_project))) %>% 
  mutate(n_song_unicode = str_replace_all(as.character(n_song), 
                                          "\\d",
                                          \(x) {stringi::stri_unescape_unicode(paste0("\\u", "E08", x))}
                                          ))


# Get Alan Menken caricature from website
caricature <- magick::image_read("https://www.alanmenken.com/assets/images/biography/alan.png")
caricature <- grid::rasterGrob(caricature, interpolate=TRUE)


# Create title
title <- paste0("<span>Music by Alan Menken</span>","<br>", 
                "<span style = 'color:#D7263D;'>Lyrics by...</span>")


# Create subtitle
subtitle <- "Song count by collaborating lyricist"


# Create music call-out
music_callout <- "Each bar is decorated with a melody from an Alan Menken song written by that lyricist. Can you name each song?"


# Create key signature call-out
key_sig_callout <- "Each staff uses the most common key signature per recording."


# Create social caption
# Credit to @tashapiro for the social_caption function
source("social-caption.R")
caption <- paste0("<span style = 'font-family:Times;'>**Sources: Spotify, Wikipedia**  <span><br>",
                 social_caption(icon_color ="#D7263D", 
                                bg_color = NA, 
                                font_color = "black",
                                font_family="Times",
                                twitter = "@Philli_Phresh",
                                github = "philliphresh",
                                linkedin="phillip-sanderell"))


# Create disclaimer
disclaimer <- 
  paste0("The wonderful caricature is from alanmenken.com", "\n",
         "All creative rights belong to their original creator(s).")


# Create plot
plot_data %>% 
  mutate(lyricist = fct_reorder(lyricist, n_song)) %>% 
  ggplot(aes(n_song, lyricist)) +
  # Song count
  geom_text(aes(label = n_song_unicode), family = "Bravura", 
            size = 5, nudge_x = 4, nudge_y = .6, 
            hjust = .5, vjust = 1) +
  # Project count
  geom_text(aes(label = n_project_unicode), family = "Bravura",
            size = 5, nudge_x = 4, nudge_y = .46, 
            hjust = .5, vjust = 1) +
  # Add song count annotation
  annotate(geom = "text", label = "songs", x = 75, y = 6.2, 
           size= 4, family = "Times", fontface = "bold", hjust = 0) +
  # Add project count annotation
  annotate(geom = "text", label = "projects", x = 75, y = 5.8, 
           size= 4, family = "Times", fontface = "bold", hjust = 0) +
  geom_textbox(label = "(stage and screen productions of the same story count as only one project)", x = 75, y = 5.6, width = .13,
           family = "Times", box.colour = NA, fill = NA, hjust = 0,
           box.padding = unit(rep(0,4), "pt"), vjust = 1) +
  # Add song annotation arrow
  geom_curve(aes(x=74.5, y=6.15, xend=70, yend=6.05), linewidth = .5, 
             curvature = 0, 
             arrow = arrow(angle = 25, length = unit(0.01, "npc"), 
                           type = "closed")) +
  # Add project annotation arrow
  geom_curve(aes(x=74.5, y=5.8, xend=70, yend=5.9), linewidth = .5, 
             curvature = 0, 
             arrow = arrow(angle = 25, length = unit(0.01, "npc"), 
                           type = "closed")) +
  # Bar overlay
  geom_rect(aes(xmin=0, xmax=n_song + 1, #6
                ymin=    as.numeric(lyricist) - .32,
                ymax= as.numeric(lyricist) + .3),
            alpha = .15, fill = "#5C3E1E") +
  # Add Alan Menken caricature
  annotation_custom(caricature, xmin=25, xmax=Inf, ymin=-Inf, ymax=4.4) +
  # Add piano
  geom_path(aes(x, y), data = data.frame(
    x = c(90, 160, 250), y = c(0, 3, 0)),
    color = "black") +
  # Music call-out
  geom_textbox(label = music_callout, family = "Times", width = .3,
               x = 145, y = 6, box.colour = NA, fill = NA) +
  # Key signature call-out
  geom_textbox(label = key_sig_callout, family="Times", width = .17,
               x = 53, y = 1.0, box.colour = NA, fill = NA) +
  # Add caption
  geom_richtext(label = caption, x = 181, y = 1, hjust = 1, 
                fill = NA, label.size = 0, size = 3.5) +
  # Disclaimer
  annotate("text", 179, .3, label = disclaimer, family = "Times",
           hjust = 1, vjust = 0, lineheight = .7, size = 3.5) +
  # Lyricist name
  geom_text(aes(label = lyricist),
            size = 5, hjust = 1, nudge_x = 1,
            nudge_y = .45, color = "#D7263D", family = "Times", fontface = "bold") +
  # Music snippet
  ggfx::as_reference(
  geom_image(aes(x = 0, lyricist, image = image_path),
             by = "height", hjust = 0, nudge_x = -4,
             size = 0.235, nudge_y = -.2,
             asp = 1.4,
             image_fun = \(x) {magick::image_transparent(x, color = "white") %>% 
                 magick::image_scale("x300")}),
  id = "staves"
  ) +
  ggfx::with_blend(
    geom_polygon(aes(x, y), colour = NA, fill = '#5C3E1E', 
                 alpha = .9,
                 data = data.frame(x = c(0, 0, 200, 200),
                                   y = c(0, 8,   8,   0))),
    bg_layer = "staves",
    blend_type = 'in'
  ) +
  # Name underline
  geom_segment(aes(x=0, xend=n_song + 1, 
                   y=    as.numeric(lyricist) + .3, 
                   yend= as.numeric(lyricist) + .3),
               color = "#D7263D") +
  # Add title and subtitle
  labs(title = title,
       subtitle = subtitle) +
  # Theme edits
  coord_cartesian(xlim = c(0, 175), ylim = c(.6, 7), clip = "off") +
  theme_void() +
  theme(
    plot.background = element_rect(color = NA, fill = "#f5ebe0"), 
    plot.title = element_markdown(size = 30, family = "Zeyada", 
                                  hjust = .5, lineheight = .8),
    plot.subtitle = element_text(family = "Times", hjust = .063, 
                                 size = 5*.pt, lineheight = .5),
    plot.margin = margin(20))

# Save
ggsave("menken-lyricists.png",
  width = 10,
  height = 7,
  units = "in",
  dpi = 1000
)
