library(ggplot2)
library(scatterplot3d)
library(dplyr)

# Dane

df <- data.frame(
  Miesiąc  = c("Styczeń","Luty","Marzec","Kwiecień",
               "Maj","Czerwiec","Lipiec","Sierpień",
               "Wrzesień","Październik","Listopad","Grudzień"),
  Sprzedaż = c(40,55,30,65,50,45,70,60,35,80,55,75)
)

df$Miesiąc <- factor(df$Miesiąc,
                     levels = c("Styczeń","Luty","Marzec","Kwiecień",
                                "Maj","Czerwiec","Lipiec","Sierpień",
                                "Wrzesień","Październik","Listopad","Grudzień"))


# Wykres 2D

wyk_2d <- df %>%
  ggplot(aes(x = Miesiąc, y = Sprzedaż)) +
  geom_col(fill = "steelblue") +
  coord_cartesian(expand = FALSE) +
  theme_minimal(base_size = 14) +
  labs(
    title = "Miesięczna sprzedaż",
    x     = "Miesiąc",
    y     = "Sprzedaż"
  ) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  )


# Tworzenie wykresu 3D

par(mar = c(6, 4, 4, 2) + 0.1)

x  <- seq_len(nrow(df))
y0 <- rep(0, length(x))
z  <- df$Sprzedaż
zs <- pretty(c(0, z))


s3d <- scatterplot3d(
  x, y0, z,
  type        = "h",
  lwd         = 50,
  color       = "steelblue",
  angle       = 55,
  xlab        = "Miesiąc",
  ylab        = "",
  zlab        = "Sprzedaż",
  x.ticklabs  = rep("", length(x)),
  y.ticklabs  = "",
  z.ticklabs  = zs, 
  zlim        = c(0, max(zs)),
  grid        = TRUE,
  box         = TRUE,
  axis        = TRUE
)


s3d$points3d(x, y0, z, type="h", lwd=3, col="steelblue")

coords <- s3d$xyz.convert(x, y0, rep(0, length(x)))
text(
  x      = coords$x,
  y      = coords$y - 0.10 * diff(par("usr")[3:4]),
  labels = df$Miesiąc,
  srt    = 45,
  adj    = c(1,1),
  cex    = 0.7,
  xpd    = NA
)

title(main = "Miesięczna sprzedaż")