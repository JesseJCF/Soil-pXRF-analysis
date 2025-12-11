library(tidyverse)

setwd("")

df <- read_csv("Manresa leaf group (soils class) pXRF.csv",
               show_col_types = FALSE)

# ============================================================
# Data cleaning + grouping
# ============================================================

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


# ============================================================
# Plotting (Clustered bars: Birch leaves vs Phragmites stems)
# Apple Standard REMOVED
# ============================================================

if (!dir.exists("plots")) dir.create("plots")

for (el in elements) {
  
  message("Plotting:", el)
  
  # extract element values, reshape, join
  values <- df_clean %>% filter(ID == el)
  
  long_df <- values %>%
    pivot_longer(cols = all_of(sample_cols), names_to = "Sample", values_to = "Value") %>%
    left_join(sample_groups, by = "Sample") %>%
    filter(!is.na(Group)) %>%
    filter(Group != "Apple Leaves Standard")   # <-- remove Apple Leaves Standard
  
  # Add cluster (Birch leaves vs Phragmites stems)
  long_df <- long_df %>%
    mutate(
      Cluster = case_when(
        Group %in% c("Farm Creek Birch", "Manresa Birch") ~ "Birch leaves",
        Group %in% c("Farm Creek Phragmites", "Manresa Phragmites") ~ "Phragmites stems"
      ),
      Site = case_when(
        str_detect(Group, "Manresa") ~ "Manresa",
        str_detect(Group, "Farm Creek") ~ "Farm Creek"
      )
    )
  
  # Compute means & SE
  plot_df <- long_df %>%
    group_by(Cluster, Site) %>%
    summarise(
      Mean = mean(Value, na.rm = TRUE),
      SE = ifelse(n() > 1, sd(Value, na.rm = TRUE) / sqrt(n()), NA_real_),
      .groups = "drop"
    )
  
  # Order clusters
  plot_df$Cluster <- factor(plot_df$Cluster, levels = c("Birch leaves", "Phragmites stems"))
  
  # ---- Clustered bar fix: shared dodge ----
  dodge <- position_dodge(width = 0.6)
  
  p <- ggplot(plot_df, aes(x = Cluster, y = Mean, fill = Site)) +
    geom_col(
      position = dodge,
      width = 0.55           # <--- prevents overlap
    ) +
    
    geom_errorbar(
      aes(ymin = Mean - SE, ymax = Mean + SE),
      position = dodge,
      width = 0.15
    ) +
    
    scale_fill_manual(
      values = c("Manresa" = "orange", "Farm Creek" = "darkgreen"),
      name = "Site"
    ) +
    
    coord_cartesian(ylim = c(0, NA)) +
    labs(title = paste("Element:", el),
         y = "Concentration (% by mass)",
         x = "") +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 0, hjust = 0.5))
  
  ggsave(
    filename = paste0("plots/", el, "_clustered_barplot.png"),
    plot = p,
    width = 10,
    height = 6,
    dpi = 300
  )
}



# ============================================================
# Results Table: Manresa vs Farm Creek by Cluster (Birch vs Phragmites)
# ============================================================

results <- list()

for (el in elements) {
  
  # extract element values and join grouping info
  values <- df_clean %>% filter(ID == el)
  
  long_df <- values %>%
    pivot_longer(cols = all_of(sample_cols), names_to = "Sample", values_to = "Value") %>%
    left_join(sample_groups, by = "Sample") %>%
    filter(!is.na(Group)) %>%
    filter(Group != "Apple Leaves Standard") %>%
    mutate(
      Cluster = case_when(
        Group %in% c("Farm Creek Birch", "Manresa Birch") ~ "Birch",
        Group %in% c("Farm Creek Phragmites", "Manresa Phragmites") ~ "Phragmites"
      ),
      Site = case_when(
        str_detect(Group, "Manresa") ~ "Manresa",
        str_detect(Group, "Farm Creek") ~ "Farm Creek"
      )
    )
  
  # Process Birch and Phragmites separately
  for (cl in c("Birch", "Phragmites")) {
    
    df_cl <- long_df %>% filter(Cluster == cl)
    
    if (nrow(df_cl) > 0) {
      
      # compute means
      means <- df_cl %>%
        group_by(Site) %>%
        summarise(Mean = mean(Value, na.rm = TRUE), .groups = "drop")
      
      manresa_mean <- means$Mean[means$Site == "Manresa"]
      farm_mean <- means$Mean[means$Site == "Farm Creek"]
      
      # perform t-test only if both groups have >1 sample
      sig <- NA
      if (length(df_cl$Value[df_cl$Site == "Manresa"]) > 1 &&
          length(df_cl$Value[df_cl$Site == "Farm Creek"]) > 1) {
        
        tt <- t.test(Value ~ Site, data = df_cl, var.equal = FALSE)
        sig <- ifelse(tt$p.value < 0.05, "yes", "no")
      }
      
      results[[paste0(el, "_", cl)]] <- tibble(
        Element_Cluster = paste0(el, "_", cl),
        `Manresa mean` = manresa_mean,
        `Farm Creek mean` = farm_mean,
        Significance = sig
      )
    }
  }
}

results_table <- bind_rows(results)

# Save CSV
write_csv(results_table, "Manresa_vs_FarmCreek_cluster_results_table.csv")

print(results_table)

