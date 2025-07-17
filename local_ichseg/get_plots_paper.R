library(dplyr)
library(ggplot2)
library(ggpubr)
library(ggsci)
library(scales)
library(showtext)


# Add Computer Modern font using the regular and bold versions
font_add("Computer Modern", regular = "/Users/rushil/Downloads/cmu-serif/cmunrm.ttf", bold = "/Users/rushil/Downloads/cmu-serif/cmunbx.ttf")
showtext_auto()

# --- FIGURE 1 ---
df <- data.frame(
  Method = c("V1", "Robust", "Synthstrip", "HD_CT-BET", "CT-BET", "Brainchop"),
  Pass2_Failure_Rate = c(
    7.457428068,
    0.29359953,
    76.51203758,
    27.12859659,
    38.1443299,
    17.06791936
  )
)

df2 <- df %>%
  arrange(Pass2_Failure_Rate) %>%
  mutate(Method = factor(Method, levels = Method))

fill_cols <- c(
  "V1"         = "#444448",
  "Robust"     = "#666666",
  "Synthstrip" = "#888888",
  "HD_CT-BET"  = "#AAAAAA",
  "CT-BET"     = "#CCCCCC",
  "Brainchop"  = "#EEEEEE"
)

ggplot(df2, aes(x = Method, y = Pass2_Failure_Rate, fill = Method)) +
  # solid baseline
  geom_hline(yintercept = 0, linetype = "solid", color = "black") +
  # bars with thicker borders
  geom_col(width = 0.6, color = "black", size = 0.8, alpha = 0.9) +
  # bigger bold labels just above each bar
  geom_text(
    aes(label = sprintf("%.2f", Pass2_Failure_Rate)),
    position = position_dodge(width = 0.6),
    vjust    = -0.3,
    fontface = "bold",
    size     = 5,
    family   = "Computer Modern"
  ) +
  scale_fill_manual(values = fill_cols) +
  scale_y_continuous(
    expand = expansion(add = c(0, 5)),
    breaks = pretty_breaks(n = 10)
  ) +
  theme_minimal(base_size = 14, base_family = "Computer Modern") +
  theme(
    panel.grid.major.x  = element_line(color = "grey80"),
    panel.grid.major.y  = element_line(color = "grey80"),
    panel.grid.minor    = element_blank(),
    axis.line.x         = element_line(color = "black"),
    axis.ticks.x        = element_line(color = "black"),
    axis.line.y         = element_blank(),
    axis.title.x        = element_text(face = "bold", margin = margin(t = 10)),
    axis.title.y        = element_text(face = "bold", margin = margin(r = 10)),
    axis.text.x         = element_text(size = 12, color = "black"),
    axis.text.y         = element_text(face = "bold", size = 12, color = "black"),
    legend.position     = "none"
  ) +
  labs(
    x = "Method",
    y = "Pass 2 Failure Rate (%)"
  )

ggsave("pass2_failure_rate_plot.pdf", 
       width = 8, height = 6, units = "in", 
       dpi = 300, device = "pdf")

# --- FIGURE 2 ---
library(dplyr)
library(tidyr)
library(ggplot2)
library(scales)

# 1) Your wide‐form data
df_rates <- data.frame(
  Method               = c("Robust", "V1", "Synthstrip", "HD_CT-BET", "CT-BET", "Brainchop"),
  Pass_1_Failure_Rate  = c(0.626345665, 16.87218634, 96.30064592, 99.74554707, 70.30927835, 85.53532981),
  Pass_2_Failure_Rate  = c(0.29359953,   7.457428068, 76.51203758, 27.12859659, 38.14432990, 17.06791936),
  stringsAsFactors     = FALSE
)

# 2) Compute Method order by ascending Pass 2 rate
method_order <- df_rates %>%
  arrange(Pass_1_Failure_Rate) %>%
  pull(Method)

# 3) Melt to long form, applying that order
df_long <- df_rates %>% 
  pivot_longer(
    starts_with("Pass_"),
    names_to  = "Pass",
    values_to = "Rate"
  ) %>% 
  mutate(
    Pass = recode(
      Pass,
      Pass_1_Failure_Rate = "Pass 1",
      Pass_2_Failure_Rate = "Pass 2"
    ),
    Method = factor(Method, levels = method_order),    # <— use new ordering
    Pass   = factor(Pass, levels = c("Pass 1","Pass 2"))
  )

# 4) Greyscale fills for the two passes
fill_pass <- c("Pass 1" = "#666666", "Pass 2" = "#EEEEEE")

# 5) Plot exactly as before
p2 <- ggplot(df_long, aes(x = Method, y = Rate, fill = Pass)) +
  geom_hline(yintercept = 0, linetype="solid", color="black") +
  geom_col(
    position = position_dodge(width = 0.7),
    width    = 0.6,
    color    = "black",
    size     = 0.8,
    alpha    = 0.9
  ) +
  geom_text(
    aes(label = sprintf("%.2f", Rate)),
    position = position_dodge(width = 0.7),
    vjust    = -0.5,
    fontface = "bold",
    family = "Computer Modern",
    size     = 3.5
  ) +
  scale_fill_manual(values = fill_pass, name = NULL) +
  scale_y_continuous(
    expand = expansion(add = c(0,5)),
    breaks = pretty_breaks(10)
  ) +
  theme_minimal(base_size = 14, base_family = "Computer Modern") +
  theme(
    panel.grid.major.x = element_line(color="grey80"),
    panel.grid.major.y = element_line(color="grey80"),
    panel.grid.minor   = element_blank(),
    axis.line.x        = element_line(color="black"),
    axis.ticks.x       = element_line(color="black"),
    axis.line.y        = element_blank(),
    axis.title.x       = element_text(face="bold", margin=margin(t=10)),
    axis.title.y       = element_text(face="bold", margin=margin(r=10)),
    axis.text.x        = element_text(size=12, color="black"),
    axis.text.y        = element_text(face="bold", size=12, color="black"),
    legend.position    = "bottom"
  ) +
  labs(
    x = "Method",
    y = "Failure Rate (%)"
  )

# 6) Save
ggsave("/Users/rushil/ichseg/local_results/failure_rate_plot.pdf", p2,
       width = 8, height = 6, units = "in",
       dpi = 300, device = "pdf")

#--- Figure 3 ----#
df_modes <- data.frame(
  Method              = c("v1",        "robust"),
  Neck_Rate           = c(1.644157369, 0.058719906),
  Holes_Rate          = c(4.012526913, 0.234879624),
  Nonbrain_Rate       = c(3.386181249, 0.000000000),
  Multiple_Rate       = c(1.526717557, 0.000000000),
  stringsAsFactors    = FALSE
)

# 2) Pivot to long form
df_modes_long <- df_modes %>%
  pivot_longer(
    cols      = -Method,
    names_to  = "Failure_Mode",
    values_to = "Rate"
  ) %>%
  mutate(
    # nicer factor levels & labels
    Failure_Mode = factor(Failure_Mode,
      levels = c("Neck_Rate","Holes_Rate","Nonbrain_Rate","Multiple_Rate"),
      labels = c("Inferior overextension",
                 "Segmentation omissions",
                 "Non-neural inclusion",
                 "Multiple concurrent errors")
    ),
    Method = factor(Method, levels = c("v1","robust"))
  )

# 3) Greyscale fills matching your scheme
fill_cols2 <- c("v1" = "#444448", "robust" = "#AAAAAA")

# 4) Build the plot
p3 <- ggplot(df_modes_long, aes(x = Failure_Mode, y = Rate, fill = Method)) +
  geom_hline(yintercept = 0, linetype = "solid", color = "black") +
  geom_col(
    position = position_dodge(width = 0.7),
    width    = 0.6,
    color    = "black",
    size     = 0.8,
    alpha    = 0.9
  ) +
  geom_text(
    aes(label = sprintf("%.2f", Rate)),
    position = position_dodge(width = 0.7),
    vjust    = -0.3,
    fontface = "bold",
    size     = 5
  ) +
  scale_fill_manual(values = fill_cols2, name = NULL) +
  scale_y_continuous(
    expand = expansion(add = c(0, 1)),
    breaks = pretty_breaks(6)
  ) +
  theme_minimal(base_size   = 14,
                base_family = "serif") +
  theme(
    panel.grid.major.x = element_line(color = "grey80"),
    panel.grid.major.y = element_line(color = "grey80"),
    panel.grid.minor   = element_blank(),
    axis.line.x        = element_line(color = "black"),
    axis.ticks.x       = element_line(color = "black"),
    axis.line.y        = element_blank(),
    axis.title.x       = element_blank(),
    axis.title.y       = element_text(face = "bold", margin = margin(r = 10)),
    axis.text.x        = element_text(face = "bold", size = 12, color = "black",
                                      angle = 45, hjust = 1),
    axis.text.y        = element_text(face = "bold", size = 12, color = "black"),
    legend.position    = "bottom",
    legend.text        = element_text(size = 12, family = "serif")
  ) +
  labs(
    y = "Pass 2 Failure Rate (%)"
  )

# 5) Save at the same single-column width
ggsave("figure3_error_mode_breakdown.pdf", p3,
       width  = 85, height = 60, units = "mm",
       device = cairo_pdf, family = "serif")

# --- FIGURE 3: Pass 2 Error-Mode Breakdown (v1 vs robust) ---

df_modes <- data.frame(
  Method           = c("v1",        "robust"),
  Neck_Rate        = c(1.644157369, 0.058719906),
  Holes_Rate       = c(4.012526913, 0.234879624),
  Nonbrain_Rate    = c(3.386181249, 0.000000000),
  Multiple_Rate    = c(1.526717557, 0.000000000),
  stringsAsFactors = FALSE
)

# 2) Pivot & relabel—inject newlines into long labels
df_modes_long <- df_modes %>%
  tidyr::pivot_longer(
    cols      = -Method,
    names_to  = "Failure_Mode",
    values_to = "Rate"
  ) %>%
  mutate(
    Failure_Mode = factor(
      Failure_Mode,
      levels = c("Neck_Rate","Holes_Rate","Nonbrain_Rate","Multiple_Rate"),
      labels = c(
        "Inferior\noverextension",
        "Segmentation\nomissions",
        "Non-neural\ninclusion",
        "Multiple concurrent\nerrors"
      )
    ),
    Method = factor(Method, levels = c("v1","robust"))
  )

# 3) Greyscale fills
fill_cols2 <- c("v1" = "#444448", "robust" = "#AAAAAA")

# 4) Plot
p3 <- ggplot(df_modes_long, aes(x = Failure_Mode, y = Rate, fill = Method)) +
  geom_hline(yintercept = 0, linetype = "solid", color = "black") +
  geom_col(
    position = position_dodge(width = 0.7),
    width    = 0.6,
    color    = "black",
    size     = 0.8,
    alpha    = 0.9
  ) +
  geom_text(
    aes(label = sprintf("%.2f", Rate)),
    position = position_dodge(width = 0.7),
    vjust    = -0.3,
    fontface = "bold",
    family   = "Computer Modern",
    size     = 5
  ) +
  scale_fill_manual(values = fill_cols2, name = NULL) +
  scale_y_continuous(
    expand = expansion(add = c(0, 1)),
    breaks = pretty_breaks(6)
  ) +
  theme_minimal(base_size   = 14,
                base_family = "Computer Modern") +
  theme(
    text               = element_text(family = "Computer Modern"),
    panel.grid.major.x = element_line(color = "grey80"),
    panel.grid.major.y = element_line(color = "grey80"),
    panel.grid.minor   = element_blank(),
    axis.line.x        = element_line(color = "black"),
    axis.ticks.x       = element_line(color = "black"),
    axis.line.y        = element_blank(),
    axis.title.x       = element_blank(),
    axis.title.y       = element_text(face = "bold", size = 14, margin = margin(r = 10)),
    axis.text.x        = element_text(face = "bold", size = 12, color = "black", 
                                      family = "Computer Modern"),
    axis.text.y        = element_text(face = "bold", size = 12, color = "black", 
                                      family = "Computer Modern"),
    legend.position    = "bottom",
    legend.text        = element_text(size = 12, family = "Computer Modern")
  ) +
  labs(
    y = "Pass 2 Failure Rate (%)"
  )

# 5) Save—single‐column width (85 mm × 60 mm)
ggsave("/Users/rushil/ichseg/local_results/figure3_error_mode_breakdown_v1_vs_robust.pdf", p3,
       width = 8, height = 6, units = "in",
       dpi = 300, device = "pdf")

# --- FIGURE 4: Subgroup Pass 2 Failure Rates (v1 vs robust) ---

font_add(
  family  = "Computer Modern",
  regular = "/Users/rushil/Downloads/cmu-serif/cmunrm.ttf",
  bold    = "/Users/rushil/Downloads/cmu-serif/cmunbx.ttf"
)
showtext_auto()

# 1) Data: subgroup failure rates
df4 <- data.frame(
  Method                   = c("v1",           "robust"),
  Artifact_Failure_Rate    = c(22.22222222,    3.703703704),
  Craniotomy_Failure_Rate  = c(17.5,           0.0),
  CTA_Failure_Rate         = c(30.0,           0.0),
  stringsAsFactors         = FALSE
)

# 2) Pivot to long form & relabel subsets
df4_long <- df4 %>%
  pivot_longer(
    cols      = -Method,
    names_to  = "Subset",
    values_to = "Rate"
  ) %>%
  mutate(
    Subset = recode(Subset,
                    Artifact_Failure_Rate   = "Artifact scans",
                    Craniotomy_Failure_Rate = "Post-craniotomy CT",
                    CTA_Failure_Rate        = "CT Angiography"
    ),
    Subset = factor(Subset,
                    levels = c("Artifact scans","Post-craniotomy CT","CT Angiography")
    ),
    Method = factor(Method, levels = c("v1","robust"))
  )

# 3) Greyscale fills
fill_cols2 <- c("v1" = "#444448", "robust" = "#AAAAAA")

# 4) Plot with the same styling as Figure 3

p4 <- ggplot(df4_long, aes(x = Subset, y = Rate, fill = Method)) +
  geom_hline(yintercept = 0, linetype = "solid", color = "black") +
  geom_col(
    position = position_dodge(width = 0.7),
    width    = 0.6,
    color    = "black",
    size     = 0.8,
    alpha    = 0.9
  ) +
  geom_text(
    aes(label = sprintf("%.2f", Rate)),
    position = position_dodge(width = 0.7),
    vjust    = -0.3,
    fontface = "bold",
    family   = "Computer Modern",
    size     = 5
  ) +
  scale_fill_manual(values = fill_cols2, name = NULL) +
  scale_y_continuous(
    expand = expansion(add = c(0, 5)),
    breaks = pretty_breaks(8)
  ) +
  theme_minimal(base_size   = 14,
                base_family = "Computer Modern") +
  theme(
    text               = element_text(family = "Computer Modern"),
    panel.grid.major.x = element_line(color = "grey80"),
    panel.grid.major.y = element_line(color = "grey80"),
    panel.grid.minor   = element_blank(),
    axis.line.x        = element_line(color = "black"),
    axis.ticks.x       = element_line(color = "black"),
    axis.line.y        = element_blank(),
    axis.title.x       = element_blank(),
    axis.title.y       = element_text(face = "bold", size = 14, margin = margin(r = 10)),
    axis.text.x        = element_text(face = "bold", size = 12, color = "black", family = "Computer Modern"),
    axis.text.y        = element_text(face = "bold", size = 12, color = "black", family = "Computer Modern"),
    legend.position    = "bottom",
    legend.text        = element_text(size = 12, family = "Computer Modern")
  ) +
  labs(
    y = "Pass 2 Failure Rate (%)"
  )


ggsave("/Users/rushil/ichseg/local_results/figure4_subgroup_failure_rates.pdf", p4,
       width = 8, height = 6, units = "in",
       dpi = 300, device = "pdf")
