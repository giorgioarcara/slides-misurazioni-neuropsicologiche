# Adattato da: 2026 - Applied Psychometrics/Slides-Applied-Psychometrics/8.3_Regression_based_norms.qmd
# Genera le due figure statiche (etichette in italiano) per la nuova sezione
# "La regressione come base per le regression-based norms" (Blocco 2, punto 16)

library(ggplot2)

set.seed(123)

COL_DARK   <- "#23373B"
COL_ACCENT <- "#EB811B"
COL_MID    <- "#14B5D0"
COL_RED    <- "#c0392b"

theme_clean <- function(base = 11) {
  theme_minimal(base_size = base) +
    theme(
      plot.background  = element_rect(fill = "white", colour = NA),
      panel.background = element_rect(fill = "white", colour = NA),
      panel.grid.minor = element_blank(),
      panel.grid.major = element_line(colour = "grey90"),
      axis.title       = element_text(size = base - 1),
      axis.text        = element_text(size = base - 2),
      plot.title       = element_text(size = base, face = "bold"),
      plot.subtitle    = element_text(size = base - 2, colour = "grey45"),
      legend.position  = "none"
    )
}

# Simula dati normativi (età, punteggio)
N <- 220
df <- data.frame(eta = round(runif(N, 20, 80)))
df$punteggio <- 60 - 0.28 * df$eta + rnorm(N, 0, 6)

fit <- lm(punteggio ~ eta, data = df)
df$previsto <- fitted(fit)
df$residuo  <- residuals(fit)
df$z_residuo <- scale(df$residuo)[, 1]

# soggetto da evidenziare nel grafico del residuo singolo
idx <- which(df$residuo < quantile(df$residuo, 0.05) & df$eta > 35 & df$eta < 55)[1]
pt <- df[idx, ]

# --- Figura 1: residuo di un singolo soggetto ---
figD <- ggplot(df, aes(x = eta, y = punteggio)) +
  geom_point(colour = COL_DARK, alpha = 0.25, size = 1.8) +
  geom_smooth(method = "lm", formula = y ~ x, se = FALSE, colour = COL_ACCENT, linewidth = 1.2) +
  geom_point(aes(x = pt$eta, y = pt$previsto), colour = COL_ACCENT, size = 3.5, shape = 18) +
  geom_segment(aes(x = pt$eta, xend = pt$eta, y = pt$previsto, yend = pt$punteggio),
               colour = COL_RED, linewidth = 0.8,
               arrow = arrow(ends = "both", length = unit(0.08, "inches"), type = "closed")) +
  geom_point(aes(x = pt$eta, y = pt$punteggio), colour = COL_RED, size = 3.5) +
  annotate("text", x = pt$eta + 2.5, y = (pt$previsto + pt$punteggio) / 2.5,
           label = "residuo\n(osservato - previsto)", colour = COL_RED, size = 3, hjust = 0) +
  labs(title = "Il residuo per un soggetto",
       subtitle = "Quanto si discosta il punteggio osservato da quello atteso?",
       x = "Età (anni)", y = "Punteggio al test") +
  theme_clean()

ggsave("regressione_residuo_singolo.png", figD, width = 5.0, height = 2.7, dpi = 300, bg = "white")

# --- Figura 2: distribuzione dei residui standardizzati ---
figF <- ggplot(df, aes(x = z_residuo)) +
  geom_histogram(aes(y = after_stat(density)), bins = 24, fill = "grey72", colour = "white") +
  stat_function(fun = dnorm, colour = COL_DARK, linewidth = 1) +
  geom_vline(xintercept = -1.5, linetype = "dashed", colour = COL_RED, linewidth = 0.9) +
  geom_vline(xintercept = -2, linetype = "dotted", colour = COL_RED, linewidth = 0.9) +
  annotate("text", x = -1.5 + 0.08, y = 0.44, label = "z = -1.5", colour = COL_RED, hjust = 0, size = 3) +
  annotate("text", x = -2 - 0.8, y = 0.44, label = "z = -2", colour = COL_RED, hjust = 0, size = 3) +
  labs(title = "Distribuzione dei punteggi aggiustati",
       subtitle = "Residui standardizzati: media = 0, DS = 1",
       x = "Residuo standardizzato (z)", y = "Densità") +
  theme_clean()

ggsave("regressione_residui_distribuzione.png", figF, width = 4.8, height = 2.7, dpi = 300, bg = "white")

cat("Figure generate con successo.\n")
