# PANELDATA
# -------------------
library(plm)

# Innlesning av data: OBS! Linken i videoen fungerer ikke så du må laste ned
# panel_liten.csv fra modulen selv og så lese inn på vanlig måte:
df <- read.csv("panel_liten.csv")


# Oversetter til panel data frame
p.df <- pdata.frame(df,
                    index = c("id" ,"year"))


# Fixed effect model
reg.fe <- plm(lnwg ~ lnhr,
              data = p.df,
              model = "within")
summary(reg.fe)
fixef(reg.fe)

# random effect model
reg.re <- plm(lnwg ~ lnhr,
              data = p.df,
              model = "random")
summary(reg.re)


# Test av H0: begge modeller gyldige mot H1: en av dem er ikke.
phtest(reg.fe, reg.re)

