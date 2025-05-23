# Hamlet R Visualization

## Overview
This repository contains an R script that creates a comprehensive visualization of Shakespeare's Hamlet. The visualization analyzes character dialogue patterns, sentiment progression, and character interactions throughout the play.

![Hamlet Visualization](hamlet_analysis_visualization.pdf)

## Features
The visualization consists of three main components:

1. **Character Speaking Timeline**: A Gantt chart showing when the top 6 characters speak throughout the play's line numbers, divided by acts.
2. **Hamlet's Sentiment Analysis**: A line graph showing the cumulative sentiment (positive/negative) of Hamlet's dialogue as the play progresses.
3. **Character Interactions**: A chord diagram illustrating which characters appear in scenes together, showing the relationships and interactions between major characters.

## Data Source
The analysis uses the `hamlet.RData` file, which contains structured data from Shakespeare's Hamlet, including:
- Line-by-line dialogue
- Character information
- Act and scene organization

## Dependencies
This project requires the following R packages:
```R
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
```

## How It Works

### 1. Data Preparation
The script begins by loading the Hamlet dataset and identifying the top 6 characters by dialogue volume:
```R
top_characters <- hamlet %>%
  filter(character != "[stage direction]") %>%
  count(character, sort = TRUE) %>%
  slice_max(n, n = 6)

main_characters <- top_characters$character
```

A consistent color palette is established for all visualizations:
```R
character_colors <- c(
  "Hamlet" = "#D81B60",       # Vibrant crimson 
  "King Claudius" = "#1E88E5", # Royal blue
  "Lord Polonius" = "#FFC107", # Amber
  "Horatio" = "#004D40",      # Deep teal
  "Laertes" = "#FB8C00",      # Vibrant orange
  "Ophelia" = "#8E24AA"       # Rich purple
)
```

### 2. Speaking Timeline Analysis
The script creates a timeline of when characters speak throughout the play:
```R
character_timeline <- hamlet %>%
  filter(character %in% main_characters) %>%
  group_by(character, act, scene) %>%
  summarise(first_line = min(line_number, na.rm = TRUE),
            last_line = max(line_number, na.rm = TRUE),
            lines_in_scene = n(),
            .groups = 'drop')
```

This is visualized as a Gantt chart using ggplot2, showing when each character speaks across the play's timeline.

### 3. Sentiment Analysis
The script conducts sentiment analysis on Hamlet's dialogue using the "bing" lexicon:
```R
hamlet_tokens_hamlet <- hamlet %>%
  filter(character == "Hamlet") %>% 
  unnest_tokens(word, dialogue) %>%
  mutate(word = tolower(word)) %>%
  anti_join(stop_words, by = "word")

# Analyze sentiment
hamlet_sentiment_by_line <- hamlet_tokens_hamlet %>%
  inner_join(get_sentiments("bing"), by = "word") %>%
  mutate(sentiment_value = ifelse(sentiment == "positive", 1, -1)) %>%
  group_by(line_number, act, scene) %>%
  summarize(
    line_sentiment = sum(sentiment_value),
    positive_words = sum(sentiment == "positive"),
    negative_words = sum(sentiment == "negative"),
    .groups = "drop"
  )
```

The cumulative sentiment is calculated and visualized as a line graph, showing how Hamlet's emotional state progresses throughout the play.

### 4. Character Interaction Analysis
The script analyzes which characters appear in scenes together:
```R
character_pairs_full <- hamlet %>%
  filter(character %in% main_characters) %>%
  distinct(act, scene, character) %>%
  unite(scene_id, act, scene, remove = FALSE) %>%
  pairwise_count(character, scene_id, sort = TRUE, upper = FALSE)
```

This is visualized as a chord diagram using the circlize package, showing the frequency and strength of interactions between characters.

### 5. Final Visualization
The three visualizations are combined using the patchwork package:
```R
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
        plot.margin = margin(t = 40, r = 20, b = 20, l = 20)
      )
  )
```

The final visualization is saved as a PDF:
```R
ggsave("hamlet_analysis_visualization.pdf", final_plot, width = 28, height = 18, limitsize = FALSE, bg = "#F0F8FF")
```

## How to Run
1. Ensure you have R installed with all the required packages
2. Place the `01_hamlet.RData` file in the same directory as the script
3. Run the entire script in R or RStudio
4. The final visualization will be saved as `hamlet_analysis_visualization.pdf`

## Insights
The visualization reveals several interesting patterns:
- Hamlet dominates the dialogue with significantly more lines than any other character
- Hamlet's sentiment becomes increasingly negative as the play progresses
- There's a notable absence of Hamlet's dialogue in Act IV when he is sent to England
- The chord diagram shows the complex web of character interactions, with Hamlet at the center of most interactions

## Contributors
This project was created by Group 3 for the Data Analytics with R course.
