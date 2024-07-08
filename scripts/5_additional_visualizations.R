library(tidyverse)
library(ggplot2)
library(ggpubr)
library(ggalluvial)
library(gtsummary)
library(patchwork)

# unadjusted data
data_bup_long <- readRDS(here::here("data/processed/data_bup_long.rds")) |>
    rename("theta" = "avg",
           "lower" = "low",
           "upper" = "high",
           "race" = "xrace")

data_met_long <- readRDS(here::here("data/processed/data_met_long.rds")) |>
    rename("theta" = "avg",
           "lower" = "low",
           "upper" = "high",
           "race" = "xrace")

data_bup_long_ungrouped <- readRDS(here::here("data/processed/data_bup_long_ungrouped.rds"))

data_met_long_ungrouped <- readRDS(here::here("data/processed/data_met_long_ungrouped.rds"))

data_met_bin_unadj <- readRDS(here::here("data/processed/met_binom_df.rds"))

data_bup_bin_unadj <- readRDS(here::here("data/processed/bup_binom_df.rds"))

data_comb_bin_unadj <- readRDS(here::here("data/processed/comb_binom_df.rds"))


## Methods Plots


custom_colors_4 <- c("Non-Hispanic White, Uncensored" = "#e31a1c", "Non-Hispanic Black, Uncensored" = "#1f78b4", 
                     "Hispanic, Uncensored" = "darkseagreen4", 
                     "Censored" = "darkorchid4") # color scheme, can adjust

data_bup_long_censored <- data_bup_long_ungrouped |>
    select(who, week, xrace, dose_this_week, censor) |>
    filter(week <= 4) |>
    mutate(dose_this_week = ifelse(is.na(dose_this_week), 0, dose_this_week),
           xrace = case_when(xrace == "1" ~ "Non-Hispanic White, Uncensored",
                             xrace == "2" ~ "Non-Hispanic Black, Uncensored",
                             xrace == "3" ~ "Hispanic, Uncensored", 
                             TRUE ~ xrace),
           xrace = ifelse(censor == 0, "Censored", xrace),
                      xrace = factor(xrace, levels = c("Non-Hispanic Black, Uncensored", 
                                            "Hispanic, Uncensored", 
                                            "Non-Hispanic White, Uncensored", 
                                            "Censored"))) |>
    arrange(week, xrace) 

bup_alluvial <- ggplot(data_bup_long_censored,
       aes(x = week, stratum = xrace, alluvium = who,
           fill = xrace, label = xrace)) +
  geom_flow() +
  geom_stratum(alpha = .75) +
  geom_text(stat = "stratum", aes(label = after_stat(count)),
            family = "Times New Roman") +
    scale_fill_manual(values = custom_colors_4) +
    theme_void()  +
  guides(fill = guide_legend(title = "")) + labs(x = "Week") +
    theme(axis.title.x = element_text(),
          axis.text.x = element_text(),
        text = element_text(size=10, family = "Times New Roman"), 
        legend.position = "none") +
      labs(title = "Buprenorphine")

data_met_long_censored <- data_met_long_ungrouped |>
    select(who, week, xrace, dose_this_week, censor) |>
    filter(week <= 4) |>
    mutate(dose_this_week = ifelse(is.na(dose_this_week), 0, dose_this_week),
           xrace = case_when(xrace == "1" ~ "Non-Hispanic White, Uncensored",
                             xrace == "2" ~ "Non-Hispanic Black, Uncensored",
                             xrace == "3" ~ "Hispanic, Uncensored", 
                             TRUE ~ xrace),
           xrace = ifelse(censor == 0, "Censored", xrace),
           xrace = factor(xrace, levels = c("Non-Hispanic Black, Uncensored", 
                                            "Hispanic, Uncensored", 
                                            "Non-Hispanic White, Uncensored", 
                                            "Censored"))) |>
    arrange(week, xrace)

met_alluvial <- ggplot(data_met_long_censored,
       aes(x = week, stratum = xrace, alluvium = who,
           fill = xrace, label = xrace)) +
  geom_flow() +
  geom_stratum(alpha = .75) +
  geom_text(stat = "stratum", aes(label = after_stat(count)), 
            family = "Times New Roman") +
    scale_fill_manual(values = custom_colors_4) +
    theme_void()  +
  guides(fill = guide_legend(title = "")) + labs(x = "Week") +
    theme(axis.title.x = element_text(),
          axis.text.x = element_text(),
        text = element_text(size=10, family = "Times New Roman"), 
          legend.box.spacing = unit(-10, "pt")) +
      labs(title = "Methadone")

alluvial_final <- bup_alluvial + met_alluvial +
  plot_layout(ncol=2,widths=c(0.5, 0.5),
              guides = 'collect') + plot_annotation(tag_levels = 'A') & 
    theme(plot.tag = element_text(size = 12))

Cairo::CairoPS(file = here::here("figures/alluvial_plot.eps"), width = 7, height = 7)
print(alluvial_final)
dev.off()

#ggsave(here::here("figures/alluvial_plot.eps"), device = cairo_ps, colormodel = "rgb")

custom_colors_violin <- c("Non-Hispanic White" = "#e31a1c", "Non-Hispanic Black" = "#1f78b4", "Hispanic" = "darkseagreen4") # color scheme, can adjust

data_bup_long_df <- data_bup_long_ungrouped |>
    filter(week <= 4, censor == 1) |>
    rename("Race" = "xrace") |>
    mutate(Race = case_when(Race == "1" ~ "Non-Hispanic White",
                                        Race == "2" ~ "Non-Hispanic Black",
                                        Race == "3" ~ "Hispanic")) |>
    mutate(Race = factor(Race, levels = c("Non-Hispanic Black", "Hispanic", "Non-Hispanic White")))

data_met_long_df <- data_met_long_ungrouped |>
    filter(week <= 4, censor == 1) |>
    rename("Race" = "xrace") |>
    mutate(Race = case_when(Race == "1" ~ "Non-Hispanic White",
                                        Race == "2" ~ "Non-Hispanic Black",
                                        Race == "3" ~ "Hispanic")) |>
    mutate(Race = factor(Race, levels = c("Non-Hispanic Black", "Hispanic", "Non-Hispanic White")))

violin_bup <- ggplot(data_bup_long_df, aes(x = Race, y = dose_this_week, fill = Race)) +
    geom_violin(alpha = 0.75, show.legend = FALSE) +
    #geom_boxplot(width=0.1, color="black", show.legend = FALSE) +
    facet_grid(~ week, switch = "y") +
    scale_fill_manual(values = custom_colors_violin) +
    theme_minimal() +
    labs(title = "Buprenorphine",
         x = "Study Week",
         y = "Dose (mg)"
    ) + 
    theme(axis.text.x=element_blank(),
          axis.ticks.x=element_blank(),
          text = element_text(size = 10, family = "Times New Roman"),
          panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank()) +
    stat_summary(fun = "mean",
                 geom = "crossbar", 
                 width = 0.5,
                 linewidth = 0.25,
                 colour = "black") + 
    labs(fill = "Race-ethnicity")

violin_bup_stratified <- ggplot(data_bup_long_df, aes(x = Race, y = dose_this_week, fill = Race)) +
    geom_violin(alpha = 0.75, show.legend = FALSE) +
    #geom_boxplot(width=0.1, color="black", show.legend = FALSE) +
    facet_grid(project ~ week, switch = "y") +
    scale_fill_manual(values = custom_colors_violin) +
    theme_minimal() +
    labs(title = "Buprenorphine",
         x = "Study Week",
         y = expression(atop("Dose (mg)", "Trial"))
         #y = "Dose (mg)"
    ) + 
    theme(axis.title.x=element_blank(),
          axis.text.x=element_blank(),
          axis.ticks.x=element_blank(),
          text = element_text(size = 10, family = "Times New Roman"),
          panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank(), 
          strip.placement = "outside",
          strip.background = element_blank(),
          #strip.text.y.left = element_text(angle = 0),
          strip.text.x = element_blank()) + 
    stat_summary(fun = "mean",
                 geom = "crossbar", 
                 width = 0.5,
                 linewidth = 0.25,
                 colour = "black") + 
    labs(fill = "Race-ethnicity")

violin_met <- ggplot(data_met_long_df, aes(x = Race, y = dose_this_week, fill = Race)) +
    geom_violin(alpha = 0.75, show.legend = FALSE) +
    #geom_boxplot(width=0.1, color="black", show.legend = FALSE) +
    facet_wrap(~week, ncol = 4,
               strip.position = "bottom") +
    scale_fill_manual(values = custom_colors_violin) +
    theme_minimal() +
    labs(title = "Methadone",
         x = "Study Week",
         y = "Dose (mg)") + 
    theme(axis.text.x=element_blank(),
          axis.ticks.x=element_blank(),
          text = element_text(size = 10, family = "Times New Roman"),
          legend.position = "none",
          panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank()) +
    stat_summary(fun = "mean",
                 geom = "crossbar", 
                 width = 0.5,
                 linewidth = 0.25,
                 colour = "black") + 
    labs(fill = "Race-ethnicity")

violin_plot <- violin_bup + violin_met + 
    plot_annotation(tag_levels = 'A') + plot_layout(ncol = 1) & 
    theme(plot.tag = element_text(size = 12),
          panel.border = element_rect(colour = "black", fill=NA))

Cairo::CairoPS(file = here::here("figures/violin_plot.eps"), width = 7, height = 7)
print(violin_plot)
dev.off()

#ggsave(here::here("figures/violin_plot.eps"))

Cairo::CairoPS(file = here::here("figures/violin_bup_stratified.eps"), width = 7, height = 7)
print(violin_bup_stratified)
dev.off()

#ggsave(here::here("figures/violin_plot_stratified.eps"))


## Misc. Plots


bup_sites <- readRDS(here::here("data/processed/bup_sites.rds"))
met_sites <- readRDS(here::here("data/processed/met_sites.rds"))

bup_site_prop_plot <- ggplot(bup_sites, aes(x = week, y = freq, fill = xrace)) +
  geom_bar(stat = "identity", position = "stack") +
  facet_wrap(~site, scales = "fixed", ncol = 4) +
  labs(title = "Proportion of Race Groups by Site (Bup)", fill = "Race") +
  scale_fill_brewer(palette = "Set1", labels = c("White", "Black", "Hispanic")) +
  theme_minimal() + 
    theme(axis.title.x = element_blank(), axis.title.y = element_blank(),
          plot.title = element_text(size = 50)) 

met_site_prop_plot <- ggplot(met_sites, aes(x = week, y = freq, fill = xrace)) +
  geom_bar(stat = "identity", position = "stack") +
  facet_wrap(~site, scales = "free_x", ncol = 4) +
  labs(title = "Proportion of Race Groups by Site (Met)", fill = "Race") +
  scale_fill_brewer(palette = "Set1", labels = c("White", "Black", "Hispanic")) +
  theme_minimal() + 
    theme(axis.title.x = element_blank(), axis.title.y = element_blank())

bup_site_count_plot <- ggplot(bup_sites, aes(x = week, y = count, fill = xrace)) +
  geom_bar(stat = "identity", position = "stack") +
  facet_wrap(~site, scales = "fixed", ncol = 4) +
  labs(title = "Count of Race Groups by Site (Bup)", fill = "Race") +
  scale_fill_brewer(palette = "Set1", labels = c("White", "Black", "Hispanic")) +
  theme_minimal() + 
    theme(axis.title.x = element_blank(), axis.title.y = element_blank(),
          plot.title = element_text(size = 50))

met_site_count_plot <- ggplot(met_sites, aes(x = week, y = count, fill = xrace)) +
  geom_bar(stat = "identity", position = "stack") +
  facet_wrap(~site, scales = "free_x", ncol = 5) +
  labs(title = "Count of Race Groups by Site (Met)", fill = "Race") +
  scale_fill_brewer(palette = "Set1", labels = c("White", "Black", "Hispanic")) + 
  theme_minimal() + 
    theme(axis.title.x = element_blank(), axis.title.y = element_blank())

#ggsave("bup_site_prop_plot.png", plot = bup_site_prop_plot, width = 20, height = 30, bg="white", dpi = 320)
#ggsave("met_site_prop_plot.png", plot = met_site_prop_plot, bg="white")
#ggsave("bup_site_count_plot.png", plot = bup_site_count_plot, width = 20, height = 30, bg="white", dpi = 320)
#ggsave("met_site_count_plot.png", plot = met_site_count_plot, bg="white")

