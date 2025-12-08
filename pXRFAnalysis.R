library(tidyverse)

setwd("/Users/jesse/Desktop/Soils Manresa")
df <- read_csv("Manresa leaf group (soils class) pXRF.csv",
               show_col_types = FALSE)

# data cleaning + grouping
df_clean <- df %>%
  mutate(across(everything(), ~str_replace(.x, "< LOD", "0"))) %>%
  mutate(across(-ID, as.numeric))

elements <- df_clean$ID[seq(1, nrow(df_clean), by = 2)]
sample_cols <- colnames(df_clean)[-1]

group_map <- tibble(
  Pattern = c("^L-BFA", "^L-BFC", "^L-PC", "^L-PFC1$", 
              "^Apple-Leaves-Standard$", "^L-PFA1$|^L-PFA2$"),
  Label = c("Manresa Birch",
            "Farm Creek Birch",
            "Manresa Phragmites",
            "Farm Creek Phragmites",
            "Apple Leaves Standard",
            "Manresa Phragmites")
)

assign_group <- function(sample_name) {
  match <- group_map %>% 
    filter(str_detect(sample_name, Pattern)) %>% 
    pull(Label)
  if (length(match) == 0) return(NA_character_)
  return(match)
}

sample_groups <- tibble(
  Sample = sample_cols,
  Group = map_chr(sample_cols, assign_group)
)


# plotting
if (!dir.exists("plots")) dir.create("plots")

for (el in elements) {
  
  message("Plotting:", el)
  
  # extract element values, reshape, join
  values <- df_clean %>% filter(ID == el)
  
  long_df <- values %>%
    pivot_longer(cols = all_of(sample_cols), names_to = "Sample", values_to = "Value") %>%
    left_join(sample_groups, by = "Sample") %>%
    filter(!is.na(Group))
  
  # compute means & SE
  plot_df <- long_df %>%
    group_by(Group) %>%
    summarise(
      Mean = mean(Value, na.rm = TRUE),
      SE = ifelse(n() > 1, sd(Value, na.rm = TRUE) / sqrt(n()), NA_real_),
      .groups = "drop"
    )
  
  
  desired_order <- c(
    "Apple Leaves Standard",
    "Farm Creek Birch",
    "Manresa Birch",
    "Farm Creek Phragmites", 
    "Manresa Phragmites"
  )
  
  plot_df <- plot_df %>%
    mutate(Group = factor(Group, levels = desired_order))
  
  # leaves vs. stems for coloring
  plot_df <- plot_df %>%
    mutate(
      Type = case_when(
        Group %in% c("Apple Leaves Standard",
                     "Farm Creek Birch",
                     "Manresa Birch") ~ "Deciduous leaves",
        Group %in% c("Manresa Phragmites", "Farm Creek Phragmites") ~ "Phragmites stems",
        TRUE ~ "Other"
      )
    )

  p <- ggplot(plot_df, aes(x = Group, y = Mean, fill = Type)) +
    geom_col() +
    
    geom_errorbar(
      data = plot_df %>% filter(!is.na(SE)),
      aes(ymin = Mean - SE, ymax = Mean + SE),
      width = 0.2
    ) +
    
    scale_fill_manual(
      values = c(
        "Deciduous leaves" = "darkgreen",
        "Phragmites stems" = "orange"
      ),
      name = ""
    ) +
    
    coord_cartesian(ylim = c(0, NA)) +
    labs(title = paste("Element:", el),
         y = "Concentration (% by mass)",
         x = "") +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 60, hjust = 1))
  
  ggsave(
    filename = paste0("plots/", el, "_barplot.png"),
    plot = p,
    width = 10,
    height = 6,
    dpi = 300
  )
}
