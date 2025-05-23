# ─── Libraries ────────────────────────────────────────────────────────────────
library(tidyverse)     # Data manipulation and plotting
library(tidytext)      # Text analysis
library(ggbump)        # Bump charts
library(ggridges)      # Ridgeline plots
library(wordcloud)     # Word clouds
library(RColorBrewer)  # Color palettes
library(widyr)         # Pairwise counts
library(circlize)      # Chord diagrams
library(patchwork)     # Arrange graphs
library(gridExtra)     # Grid layout
library(grid)          # Grid graphics
library(gridGraphics)  # Grid graphics
library(ggplot2)
library(png)
library(cowplot)       # Combine plots
library(showtext)      # Fonts

# ─── Load Hamlet Data ─────────────────────────────────────────────────────────
load('01_hamlet.RData')

# ─── Identify Top 6 Characters by Dialogue Volume ────────────────────────────
top_characters <- hamlet %>%
  filter(character != "[stage direction]") %>%
  count(character, sort = TRUE) %>%
  slice_max(n, n = 6)

main_characters <- top_characters$character

# Standardized color palette for all graphs with more visually appealing colors
character_colors <- c(
  "Hamlet" = "#D81B60",       # Vibrant crimson 
  "King Claudius" = "#1E88E5", # Royal blue
  "Lord Polonius" = "#FFC107", # Amber
  "Horatio" = "#004D40",      # Deep teal
  "Laertes" = "#FB8C00",      # Vibrant orange
  "Ophelia" = "#8E24AA"       # Rich purple
)





# ─── Tokenize Full Play Text ──────────────────────────────────────────────────
hamlet_tokens <- hamlet %>%
  unnest_tokens(word, dialogue) %>%
  anti_join(stop_words, by = "word")

# ─── Load Font ─────────────────────────────────────────────────
font_add_google("EB Garamond", "Garamond")  # More elegant serif font for literary analysis
font_add_google("Coiny", "coiny")
showtext_auto()

# ─── Compute Total Lines and Order Characters ─────────────────────────────────
character_totals <- hamlet %>%
  filter(character %in% main_characters) %>%
  count(character, name = "total_lines") %>%
  arrange(desc(total_lines))

character_totals <- character_totals %>%
  mutate(character = factor(character, levels = rev(character_totals$character)))




###### IMPORTANT!
###### ─── Speaking Timeline Data with Line Count ───────────────────────────────────
character_timeline <- hamlet %>%
  filter(character %in% main_characters) %>%
  group_by(character, act, scene) %>% # THIS IS BY SCENE NOT BY ACT! SO EACH CHARACTER HAS ONLY ONE START AND END PER SCENE
  summarise(first_line = min(line_number, na.rm = TRUE),
            last_line = max(line_number, na.rm = TRUE), # na.rm = TRUE ignores NA values
            lines_in_scene = n(),  # Count of lines in each scene
            .groups = 'drop') %>% # Drop group attribute for compatibility with ggplot2
  left_join(character_totals, by = "character") %>% 
  mutate(character = factor(character, levels = levels(character_totals$character)))

# ─── Act Boundaries (Start/End Lines for Each Act) ────────────────────────────
act_boundaries <- hamlet %>%
  group_by(act) %>%
  summarise(start_line = min(line_number, na.rm = TRUE),
            end_line = max(line_number, na.rm = TRUE),
            .groups = "drop") %>%
  mutate(mid_line = (start_line + end_line) / 2)

# ─── Enhanced Theme for All Plots ─────────────────────────────────────────────
hamlet_theme <- function() {
  theme_minimal(base_family = "Garamond", base_size = 16) +  # Slightly larger base size
    theme(
      plot.title = element_text(size = 26, face = "bold", margin = margin(b = 15), color = "black"),
      plot.subtitle = element_text(size = 16, hjust = 0.5, margin = margin(b = 20), color = "black"),
      
      axis.title = element_text(size = 22, face = "bold", color = "black"),  # Bigger axis labels
      axis.text = element_text(size = 20, color = "black"),                  # Bigger axis numbers/labels
      
      panel.grid.major = element_line(color = "gray90", size = 0.3),
      panel.grid.minor = element_blank(),
      
      strip.text = element_text(size = 16, face = "bold", color = "black"),  # Character names (if faceted)
      
      plot.background = element_rect(fill = "#F0F8FF", color = NA),
      panel.background = element_rect(fill = "#F0F8FF", color = NA),
      legend.background = element_rect(fill = "#F0F8FF", color = NA),
      
      legend.position = "bottom",
      plot.margin = margin(20, 20, 20, 20)
    )
}





# ─── Plot Gantt Chart ───────────────────────────────────────
gantt_hamlet <- ggplot(character_timeline, aes(y = character, xmin = first_line, xmax = last_line,
                                               color = character)) +
  # Thicker lines for visibility
  geom_linerange(size = 20, position = position_dodge(width = 0.8), alpha = 0.8) +
  # Use start_line to mark act boundaries
  geom_vline(data = act_boundaries, aes(xintercept = start_line),
             color = "gray60", linetype = "dashed", size = 0.8) +
  geom_text(data = act_boundaries,
            aes(x = mid_line, y = length(main_characters) + 0.5, label = paste(act)),
            inherit.aes = FALSE,
            family = "Garamond", fontface = "bold", size = 5, hjust = 0.5) +
  # facet_wrap(~ act, scales = "free_x", nrow = 1) +
  scale_color_manual(values = character_colors, guide = "none") +
  scale_x_continuous(breaks = seq(0, 4000, 500), limits = c(0, 4000), expand = c(0.01, 0.01)) +
  hamlet_theme() +
  labs(
    title = "Speaking Timeline of Hamlet's Primary Characters",
    subtitle = "Characters ordered by total lines spoken | Dashed lines indicate act boundaries",
    x = "Line Number", y = "Character"
  ) +
  theme(
    plot.title = element_text(hjust = 0.5, family = "Garamond"),
    axis.text.x = element_text(angle = 0, hjust = 0.5, family = "Garamond"),
    panel.spacing = unit(1.5, "lines"),  # Add more space between facets
    axis.title.x = element_text(margin = margin(t = 10), size = 22, face = "bold", family = "Garamond"),
    strip.text = element_text(size = 10, face = "bold", family = "Garamond")  # Larger act numbers
  )





# ─── Sentiment Analysis: Hamlet's Lines ────────────────────────────────────────

####### IMPORTANT!!!
####### Tokenize Hamlet's dialogue and remove stop words
hamlet_tokens_hamlet <- hamlet %>%
  filter(character == "Hamlet") %>% 
  unnest_tokens(word, dialogue) %>%
  mutate(word = tolower(word)) %>%
  anti_join(stop_words, by = "word") # anti_join removes stop words, defined from the tidytext package. list of common words that are not useful for analysis
  # in tidytext package, there is a list of common words that are not useful for analysis. from common sources like the stop words list from the tm package

# Load Bing Lexicon
bing_lexicon <- get_sentiments("bing")

# Analyze sentiment by act
hamlet_sentiment_by_act <- hamlet_tokens_hamlet %>%
  inner_join(bing_lexicon, by = "word") %>%
  count(act, sentiment)

# Analyze sentiment by act AND scene
hamlet_sentiment_by_scene <- hamlet_tokens_hamlet %>%
  inner_join(bing_lexicon, by = "word") %>%
  count(act, scene, sentiment)

# Calculate net sentiment (positive - negative) by scene
hamlet_net_sentiment_by_scene <- hamlet_sentiment_by_scene %>%
  pivot_wider(names_from = sentiment, values_from = n, values_fill = 0) %>%
  mutate(
    net_sentiment = positive - negative,
    sentiment_ratio = ifelse(positive + negative > 0, positive / (positive + negative), 0)
  )

# Calculate net sentiment by act
hamlet_net_sentiment_by_act <- hamlet_sentiment_by_act %>%
  pivot_wider(names_from = sentiment, values_from = n, values_fill = 0) %>%
  mutate(
    net_sentiment = positive - negative,
    sentiment_ratio = ifelse(positive + negative > 0, positive / (positive + negative), 0)
  )

####### IMPORTANT!!!
hamlet_sentiment_by_line <- hamlet %>%
  filter(character == "Hamlet") %>%
  # Important: Group by the original line_number from the dataset
  unnest_tokens(word, dialogue) %>%
  mutate(word = tolower(word)) %>%
  anti_join(stop_words, by = "word") %>%
  inner_join(get_sentiments("bing"), by = "word") %>%
  mutate(sentiment_value = ifelse(sentiment == "positive", 1, -1)) %>%
  group_by(line_number, act, scene) %>%
  summarize(
    line_sentiment = sum(sentiment_value),
    positive_words = sum(sentiment == "positive"),
    negative_words = sum(sentiment == "negative"),
    .groups = "drop"
  )

# Calculate cumulative sentiment over lines
cumulative_sentiment_by_line <- hamlet_sentiment_by_line %>%
  arrange(line_number) %>%  
  mutate(cumulative_sentiment = cumsum(line_sentiment))

# Calculate act midpoints
act_boundaries <- act_boundaries %>%
  mutate(mid_line = (start_line + end_line) / 2)

# Visualize cumulative sentiment by line with centered Act labels

# Add labeled sentiment column
cumulative_sentiment_by_line <- cumulative_sentiment_by_line %>%
  mutate(sentiment_label = case_when(
    line_sentiment > 0 ~ "Positive",
    line_sentiment < 0 ~ "Negative",
    TRUE ~ "Neutral"
  ))


# ─── Plot Cumulative Sentiment by Line ────────────────────────────────────────
# Plot with labeled colors
line_sentiment_plot <- cumulative_sentiment_by_line %>%
  ggplot(aes(x = line_number, y = cumulative_sentiment)) +
  geom_line(color = "#555555", linewidth = 1) +
  geom_point(aes(color = sentiment_label), size = 1.5) +
  geom_vline(data = act_boundaries, aes(xintercept = start_line),
             color = "gray60", linetype = "dashed", linewidth = 0.8) +
  geom_text(data = act_boundaries, 
            aes(x = mid_line, label = paste(act)),
            y = Inf, vjust = 1, family = "Garamond", size = 5, fontface = "bold") +
  scale_color_manual(values = c("Positive" = "#00FF00", 
                                "Negative" = "#FF0000", 
                                "No Speech" = "#555555"),
                     name = "Line Sentiment") +
  scale_x_continuous(breaks = seq(0, 4000, 500), limits = c(0, 4000), expand = c(0.01, 0.01)) +
  labs(title = "Hamlet's Cumulative Sentiment Progression",
       x = "Line Number",
       y = "Cumulative Sentiment") +
  hamlet_theme() +
  theme(
    legend.position = "bottom",
    legend.text = element_text(size = 20, family = "Garamond"),
    legend.title = element_text(size = 20, face = "bold", family = "Garamond"),
    axis.text = element_text(family = "Garamond"),
    axis.title.x = element_text(margin = margin(t = 10), size = 22, face = "bold", family = "Garamond"),
    plot.title = element_text(hjust = 0.5, family = "Garamond")
  )

# ─── Chord Diagram - Create as a separate file first ────────────────────────────

####### IMPORTANT!!!
# Create co-occurrence pairs across the entire play
character_pairs_full <- hamlet %>%
  filter(character %in% main_characters) %>%
  distinct(act, scene, character) %>% # distinct() ensures each character only appears once per scene
  unite(scene_id, act, scene, remove = FALSE) %>% # unite() combines act and scene into a single identifier
  # find pairs of characters that appear in the same scene
  pairwise_count(character, scene_id, sort = TRUE, upper = FALSE) # this process aggregates the data to find the number of scenes where each pair of characters appear together


####### IMPORTANT!!!
####### Create interaction matrix - it is a matrix of pairwise counts. pairwise means between two characters
matrix_data_full <- xtabs(n ~ item1 + item2, data = character_pairs_full) #xtabs() creates a matrix from the pairwise counts

# Set up the PNG file for the chord diagram
png("chord_plot.png", width = 800, height = 800, res = 120, bg = "#F0F8FF")
par(bg = "#F0F8FF", mar = c(1, 1, 3, 1), family = "serif")

# Clear previous circular plot settings
circos.clear()
# Configure the circos plot
circos.par(gap.after = 18, start.degree = 90)

# Draw the chord diagram
chordDiagram(matrix_data_full,
             transparency = 0.3,
             grid.col = character_colors,
             annotationTrack = c("grid"),
             directional = TRUE, # Add arrows to show direction
             direction.type = "arrows",
             link.arr.type = "big.arrow",
             link.sort = TRUE,
             link.largest.ontop = TRUE,
             preAllocateTracks = 1)

# Add title
title("Character Interactions in Hamlet", 
      cex.main = 1.5, font.main = 2, 
      col.main = "#333333")

# Add better character labels
circos.trackPlotRegion(track.index = 1, panel.fun = function(x, y) {
  name <- get.cell.meta.data("sector.index")
  xlim = get.cell.meta.data("xlim")
  circos.rect(xlim[1], 0, xlim[2], 0.8, 
              col = character_colors[name], border = "black")
  circos.text(CELL_META$xcenter, 1.2, name,
              facing = "bending", niceFacing = TRUE,
              adj = c(0.5, 0), cex = 1.0, col = "black", 
              font = 2)
}, bg.border = NA)

dev.off()
circos.clear()

# Read in the generated chord diagram
chord_img <- readPNG("chord_plot.png")
chord_grob <- grid::rasterGrob(chord_img, interpolate = TRUE)
chord_plot <- wrap_elements(full = chord_grob)

# ─── Final Layout with Better Proportions ─────────────────────────────────────

# Create subtitle text
plot_subtitle <- textGrob(
  "
  Hamlet, Shakespeare's longest play, is renowned 
  for its eponymous character's dramatic monologues. 
  The data confirms that Hamlet speaks the most and 
  highlights the play's increasingly darker tone  
  throughout the acts. Notably, his  departure 
  to England in Act IV corresponds with a decline  
  in his lines. The character interactions further    
  underscore Hamlet's introspective nature and the 
  limited presence of female characters.
  ",
  gp = gpar(fontsize = 26, fontfamily = "Garamond", col = "black"),
  hjust = 0,
  x = unit(0.02, "npc") 
)  

# Create subtitle panel
subtitle_panel <- ggdraw() +
  draw_grob(plot_subtitle) +
  theme(
    plot.margin = margin(b = 20, t = -5, l = 20, r = 20),
    plot.background = element_rect(fill = "#F0F8FF", color = NA)
  )


# Stack these title elements above the chord diagram
left_column <- subtitle_panel / chord_plot

# Create space for better alignment
space <- plot_spacer() + 
  theme(plot.background = element_rect(fill = "#F0F8FF", color = NA))

# Combine plots with adjusted heights for the right column
right_column <- gantt_hamlet / line_sentiment_plot + 
  plot_layout(heights = c(0.65, 0.35))  # Make Gantt chart 65% of the height

# Combine left and right columns
combined_plot <- left_column | space | right_column

# Final layout adjustments
final_plot <- combined_plot + 
  plot_layout(widths = c(1, 0.1, 2.0)) +
  plot_annotation(
    title = "Dialogue and Sentiment Analysis in Shakespeare's Hamlet",
    caption = "Data Analytics with R, Group 3",
    theme = hamlet_theme() + 
      theme(
        plot.title = element_text(size = 34, face = "bold", family = "Garamond", margin = margin(b = 15)),
        plot.margin = margin(t = 40, r = 20, b = 20, l = 20)  # Add top margin (t = 30)
      )
  )

# Print the final visualization
print(final_plot)

# Save with higher resolution
ggsave("hamlet_analysis_visualization.pdf", final_plot, width = 28, height = 18, limitsize = FALSE, bg = "#F0F8FF")