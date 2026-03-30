# Load libraries
library(readr)
library(ggplot2)
library(ggfortify)
library(qqplotr)
library(rrr)
library(olsrr)
library(dplyr)
library(performance)
library(car)
library(patchwork)

# Load data
data = read_csv("Data/consolidated_data.csv")
descriptions = read_csv("Data/indicator_descriptions.csv")

data = data %>%
  rename(
    drink_water = SH.H2O.BASW.ZS,       # People with Basic Drinking Water (% of population)
    rural_pop = SP.RUR.TOTL.ZS,         # Rural Population (% of population)
    mfg_valAdd = NV.IND.MANF.ZS,        # Manufacturing (value added, % of GDP)
    ag_exp_raw = TX.VAL.AGRI.ZS.UN,     # Agricultural raw materials exports (% of merchandise exports)
    oil_rents = NY.GDP.PETR.RT.ZS,      # Oil rents (% of GDP)
    labor_male = SL.TLF.CACT.MA.ZS,     # Labor force participation rate, male (% of male population ages 15+)
    labor_intEd = SL.TLF.INTM.ZS,       # Labor force with intermediate education (% of total working-age population with intermediate education)
    urban_pop = SP.URB.TOTL.IN.ZS,      # Urban Population (% of population)
    child_labor = SL.TLF.0714.ZS,       # Children in Employment (% of Children)
    labor_female = SL.TLF.CACT.FE.ZS,   # Labor force participation rate, female (% of female population ages 15+)
    mineral_rents = NY.GDP.MINR.RT.ZS,  # Mineral rents (% of GDP)
    ore_met_exp = TX.VAL.MMTL.ZS.UN,    # Ores and metals exports (% of merchandise exports)
    gni_pcap = NY.GNP.PCAP.KD,          # GNI per Capita (adj. 2015 USD)
    arable_land = AG.LND.ARBL.ZS,       # Arable Land (% of total land)
    trade = NE.TRD.GNFS.ZS,             # Trade (% of GDP)
    have_elec = EG.ELC.ACCS.ZS,         # Access to electricity (% of population)
    labor_basicEd = SL.TLF.BASC.ZS,     # Labor force with basic education (% of total working-age population with basic education)
    labor_advEd = SL.TLF.ADVN.ZS,       # Labor force with advanced education (% of total working-age population with advanced education)
    rich_money = SI.DST.10TH.10,        # Income share held by highest 10%
    primary_cmplt = SE.PRM.CMPT.ZS      # Primary completion rate, total (% of relevant age group)
  )

# Exploratory Plots
if (FALSE){
ggplot(data, mapping = aes(x = drink_water, y = gni_pcap, color = Region))+
  geom_point() +
  labs(title = "People with Basic Drinking Water (% of population) vs. \nGNI per Capita (adj. 2015 USD)",
       x="People with Basic Drinking Water (% of population)",
       y="GNI per Capita (adj. 2015 USD)") +
  theme(plot.title = element_text(hjust = 0.5))

ggplot(data, mapping = aes(x = have_elec, y = gni_pcap, color = Region))+
  geom_point() +
  labs(title = "Access to electricity (% of population) vs. GNI per Capita (adj. 2015 USD)",
       x="Access to electricity (% of population)",
       y="GNI per Capita (adj. 2015 USD)") +
  theme(plot.title = element_text(hjust = 0.5))

ggplot(data, mapping = aes(x = rural_pop, y = gni_pcap, color = Region))+
  geom_point() +
  labs(title = "Rural Population (% of population) vs. \nGNI per Capita (adj. 2015 USD)",
       x="Rural Population (% of population)",
       y="GNI per Capita (adj. 2015 USD)") +
  theme(plot.title = element_text(hjust = 0.5))

ggplot(data, mapping = aes(x = urban_pop, y = gni_pcap, color = Region))+
  geom_point() +
  labs(title = "Urban Population (% of population) vs. GNI per Capita (adj. 2015 USD)",
       x="Urban Population (% of population)",
       y="GNI per Capita (adj. 2015 USD)") +
  theme(plot.title = element_text(hjust = 0.5))

ggplot(data, mapping = aes(x = arable_land, y = gni_pcap, color = Region))+
  geom_point() +
  labs(title = "Arable Land (% of total land) vs. GNI per Capita (adj. 2015 USD)",
       x="Arable Land (% of total land)",
       y="GNI per Capita (adj. 2015 USD)") +
  theme(plot.title = element_text(hjust = 0.5))

ggplot(data, mapping = aes(x = trade, y = gni_pcap, color = Region))+
  geom_point() +
  labs(title = "Trade (% of GDP) vs. GNI per Capita (adj. 2015 USD)",
       x="Trade (% of GDP)",
       y="GNI per Capita (adj. 2015 USD)") +
  theme(plot.title = element_text(hjust = 0.5))

ggplot(data, mapping = aes(x = mfg_valAdd, y = gni_pcap, color = Region))+
  geom_point() +
  labs(title = "Manufacturing (value added, % of GDP) vs. \nGNI per Capita (adj. 2015 USD)",
       x="Manufacturing (value added, % of GDP)",
       y="GNI per Capita (adj. 2015 USD)") +
  theme(plot.title = element_text(hjust = 0.5))

ggplot(data, mapping = aes(x = ag_exp_raw, y = gni_pcap, color = Region))+
  geom_point() +
  labs(title = "Agricultural raw materials \nexports (% of merchandise exports) vs. \nGNI per Capita (adj. 2015 USD)",
       x="Agricultural raw materials exports (% of merchandise exports)",
       y="GNI per Capita (adj. 2015 USD)") +
  theme(plot.title = element_text(hjust = 0.5))

ggplot(data, mapping = aes(x = ore_met_exp, y = gni_pcap, color = Region))+
  geom_point() +
  labs(title = "Ores and metals exports \n(% of merchandise exports) vs. \nGNI per Capita (adj. 2015 USD)",
       x="Ores and metals exports (% of merchandise exports)",
       y="GNI per Capita (adj. 2015 USD)") +
  theme(plot.title = element_text(hjust = 0.5))

ggplot(data, mapping = aes(x = mineral_rents, y = gni_pcap, color = Region))+
  geom_point() +
  labs(title = "Mineral rents (% of GDP) vs. \nGNI per Capita (adj. 2015 USD)",
       x="Mineral rents (% of GDP)",
       y="GNI per Capita (adj. 2015 USD)") +
  theme(plot.title = element_text(hjust = 0.5))

ggplot(data, mapping = aes(x = oil_rents, y = gni_pcap, color = Region))+
  geom_point() +
  labs(title = "Oil rents (% of GDP) vs. \nGNI per Capita (adj. 2015 USD)",
       x="Oil rents (% of GDP)",
       y="GNI per Capita (adj. 2015 USD)") +
  theme(plot.title = element_text(hjust = 0.5))

ggplot(data, mapping = aes(x = child_labor, y = gni_pcap, color = Region))+
  geom_point() +
  labs(title = "Children in Employment (% of Children) vs. GNI per Capita (adj. 2015 USD)",
       x="Children in Employment (% of Children)",
       y="GNI per Capita (adj. 2015 USD)") +
  theme(plot.title = element_text(hjust = 0.5))

ggplot(data, mapping = aes(x = labor_male, y = gni_pcap, color = Region))+
  geom_point() +
  labs(title = "Labor force participation rate, male \n(% of male population ages 15+) vs. \nGNI per Capita (adj. 2015 USD)",
       x="Labor force participation rate, male (% of male population ages 15+)",
       y="GNI per Capita (adj. 2015 USD)") +
  theme(plot.title = element_text(hjust = 0.5))

ggplot(data, mapping = aes(x = labor_female, y = gni_pcap, color = Region))+
  geom_point() +
  labs(title = "Labor force participation rate, female \n(% of female population ages 15+) vs. GNI per Capita (adj. 2015 USD)",
       x="Labor force participation rate, female \n(% of female population ages 15+)",
       y="GNI per Capita (adj. 2015 USD)") +
  theme(plot.title = element_text(hjust = 0.5))

ggplot(data, mapping = aes(x = primary_cmplt, y = gni_pcap, color = Region))+
  geom_point() +
  labs(title = "Primary completion rate, total \n(% of relevant age group) vs. \nGNI per Capita (adj. 2015 USD)",
       x="Primary completion rate, total \n(% of relevant age group)",
       y="GNI per Capita (adj. 2015 USD)") +
  theme(plot.title = element_text(hjust = 0.5))

ggplot(data, mapping = aes(x = labor_basicEd, y = gni_pcap, color = Region))+
  geom_point() +
  labs(title = "Labor force with basic education (% of total \nworking-age population with basic education) vs.\n GNI per Capita (adj. 2015 USD)",
       x="Labor force with basic education (% of total \nworking-age population with basic education)",
       y="GNI per Capita (adj. 2015 USD)") +
  theme(plot.title = element_text(hjust = 0.5))

ggplot(data, mapping = aes(x = labor_intEd, y = gni_pcap, color = Region))+
  geom_point() +
  labs(title = "Labor force with intermediate education (% of total \nworking-age population with intermediate education) vs. \nGNI per Capita (adj. 2015 USD)",
       x="Labor force with intermediate education (% of total \nworking-age population with intermediate education)",
       y="GNI per Capita (adj. 2015 USD)") +
  theme(plot.title = element_text(hjust = 0.5), axis.title.x = element_text(hjust = 0.5), axis.title.y = element_text(hjust = 0.5))

ggplot(data, mapping = aes(x = labor_advEd, y = gni_pcap, color = Region))+
  geom_point() +
  labs(title = "Labor force with advanced education (% of total \nworking-age population with advanced education) vs.\n GNI per Capita (adj. 2015 USD)",
       x="Labor force with advanced education (% of total \nworking-age population with advanced education)",
       y="GNI per Capita (adj. 2015 USD)") +
  theme(plot.title = element_text(hjust = 0.5))

ggplot(data, mapping = aes(x = rich_money, y = gni_pcap, color = Region))+
  geom_point() +
  labs(title = "Income share held by highest 10% vs. GNI per Capita (adj. 2015 USD)",
       x="Income share held by highest 10%",
       y="GNI per Capita (adj. 2015 USD)") +
  theme(plot.title = element_text(hjust = 0.5))
}
#if (FALSE){
# Generate linear models for initial tests
m1 = lm(gni_pcap ~ drink_water, data = data)    # Both significant but linear gives nonsense income for low access to water. Try exp, ^2. Kept exp.
m2 = lm(gni_pcap ~ rural_pop, data = data)      # Both significant, useful but will try exp, ^2. Kept exp.
m3 = lm(gni_pcap ~ mfg_valAdd, data = data)     # Insignificant slope, low adj R^2. Discard.
m4 = lm(gni_pcap ~ ag_exp_raw, data = data)     # Insignificant slope, low adj R^2. Try exponential. Kept Exp
m5 = lm(gni_pcap ~ oil_rents, data = data)      # Insignificant slope, low adj R^2. Try exponential. Discarded both
m6 = lm(gni_pcap ~ labor_male, data = data)     # Both this and female labor participation (m10) aren't good models. Discard
m7 = lm(gni_pcap ~ labor_intEd, data = data)    # Significant slope, insignificant intercept. Low adj R^2 of 0.045. Discard but maybe bring back.
m8 = lm(gni_pcap ~ urban_pop, data = data)      # Significant slope and intercept, trying ^2, exp. Kept Exp as it was best
m9 = lm(gni_pcap ~ child_labor, data = data)    # Significant slope & intercept, but exponential is a better choice.
m10 = lm(gni_pcap ~ labor_female, data = data)  # Both this and male labor participation (m6) aren't good models. Discard
m11 = lm(gni_pcap ~ mineral_rents, data = data) # Insignificant slope, low adj R^2. Try exponential. Discarded both
m12 = lm(gni_pcap ~ ore_met_exp, data = data)   # Insignificant slope, low adj R^2. Try exponential. Discarded both
m13 = lm(gni_pcap ~ arable_land, data = data)   # Insignificant slope, low adj R^2. Try exponential. Discarded both
m14 = lm(gni_pcap ~ trade, data = data)         # Insignificant slope, low adj R^2. Discard.
m15 = lm(gni_pcap ~ have_elec, data = data)     # Both significant but linear gives nonsense income for low access to elec. Try exp, ^2. Kept exp.
m16 = lm(gni_pcap ~ labor_basicEd, data = data) # Significant intercept, insignificant slop. Tiny adj R^2. Discard
m17 = lm(gni_pcap ~ labor_advEd, data = data)   # Discard, exponential is better.
m18 = lm(gni_pcap ~ rich_money, data = data)    # No correlation (near zero slope). Discard.
m19 = lm(gni_pcap ~ primary_cmplt, data = data) # Discard, exponential is better.

# Test nonlinear models:
m20 = lm(log(gni_pcap) ~ primary_cmplt, data = data) # original: m19. Keep
m21 = lm(log(gni_pcap) ~ labor_advEd, data = data)   # original: m17. Keep
m22 = lm(log(gni_pcap) ~ child_labor, data = data)   # original: m9.  Keep

m23 = lm(log(gni_pcap) ~ ag_exp_raw, data = data)    # original: m4.  Just barely insignificant, 0.0518. Keep for now but subject to removal
#m24 = lm(log(gni_pcap) ~ oil_rents, data = data)     # original: m5.  Insignificant
#m25 = lm(log(gni_pcap) ~ mineral_rents, data = data) # original: m11. Insignificant
#m26 = lm(log(gni_pcap) ~ ore_met_exp, data = data)   # original: m12. Insignificant
#m24 = lm(log(gni_pcap) ~ arable_land, data = data)   # original: m13.  Insignificant
#m24 = lm(sqrt(gni_pcap) ~ urban_pop, data = data)     # original: m8. Improvement to adj R^2, slope a bit more significant, intercept not significant. Discard.
m24 = lm(log(gni_pcap) ~ urban_pop, data = data)     # original: m8.  Performed best. Keep

m25 = lm(log(gni_pcap) ~ drink_water, data = data)  # original: m1. Best
#m26 = lm(sqrt(gni_pcap) ~ drink_water, data = data)  # original: m1. Discard
m26 = lm(log(gni_pcap) ~ have_elec, data = data)    # original: m15. Best
#m29 = lm(sqrt(gni_pcap) ~ have_elec, data = data)    # original: m15. Discard
m27 = lm(log(gni_pcap) ~ rural_pop, data = data)    # original: m2. Best
#m32 = lm(sqrt(gni_pcap) ~ rural_pop, data = data)    # original: m2. Discard

# Generate summaries and analysis plots:
for (i in 1:27){
  model = paste0("m", i)
  m = get(model)
  
  print(i)
  print(summary(m))
  
  #label = ggplot() + annotate("text", x = 0.5, y = 0.5, label = model, size = 20) + theme_void()
  #ap = autoplot(m, which = 1:6, ncol = 2, label.size = 3)
  #cm = check_model(m)
  
  #print(label)
  #print(ap)
  #print(cm)
  
  #ggsave(filename = paste0("Plots/", model, "_ap.png"), plot = ap, width = 12, height = 8)
  #ggsave(filename = paste0("Plots/", model, "_cm.png"), plot = plot(cm), width = 12, height = 8)
}

water_plot = ggplot(data, mapping = aes(x = drink_water, y = gni_pcap, color = Region))+
  geom_point() +
  geom_smooth(method = "lm", formula = y ~ x,
              aes(linetype = "Linear (m1)"), color = "black") +
  geom_line(data = transform(data, pred = exp(predict(m25))), 
            aes(y = pred, linetype = "Exponential (m25)"), color = "red", size = 1) +
  labs(title = "(m1, m25) People with Basic Drinking Water (% of population) vs. \nGNI per Capita (adj. 2015 USD)",
       x="People with Basic Drinking Water (% of population)",
       y="GNI per Capita (adj. 2015 USD)") +
  theme(plot.title = element_text(hjust = 0.5))

elec_plot = ggplot(data, mapping = aes(x = have_elec, y = gni_pcap, color = Region))+
  geom_point() +
  geom_smooth(method = "lm", formula = y ~ x,
              aes(linetype = "Linear (m15)"), color = "black") +
  geom_line(data = transform(data, pred = exp(predict(m26))), 
            aes(y = pred, linetype = "Exponential (m26)"), color = "red", size = 1) +
  labs(title = "(m15, m26) Access to electricity (% of population) vs. GNI per Capita (adj. 2015 USD)",
       x="Access to electricity (% of population)",
       y="GNI per Capita (adj. 2015 USD)") +
  theme(plot.title = element_text(hjust = 0.5))

rural_plot = ggplot(data, mapping = aes(x = rural_pop, y = gni_pcap, color = Region))+
  geom_point() +
  geom_smooth(method = "lm", formula = y ~ x,
              aes(linetype = "Linear (m2)"), color = "black") +
  geom_line(data = transform(data, pred = exp(predict(m27))), 
            aes(y = pred, linetype = "Exponential (m27)"), color = "red", size = 1) +
  labs(title = "(m2, m27) Rural Population (% of population) vs. \nGNI per Capita (adj. 2015 USD)",
       x="Rural Population (% of population)",
       y="GNI per Capita (adj. 2015 USD)") +
  theme(plot.title = element_text(hjust = 0.5))

urban_plot = ggplot(data, mapping = aes(x = urban_pop, y = gni_pcap, color = Region))+
  geom_point() +
  geom_smooth(method = "lm", formula = y ~ x,
              aes(linetype = "Linear (m8)"), color = "black") +
  geom_line(data = transform(data, pred = exp(predict(m24))), 
            aes(y = pred, linetype = "Exponential (m24)"), color = "red", size = 1) +
  labs(title = "(m8, m24) Urban Population (% of population) vs. GNI per Capita (adj. 2015 USD)",
       x="Urban Population (% of population)",
       y="GNI per Capita (adj. 2015 USD)") +
  theme(plot.title = element_text(hjust = 0.5))

#ggplot(data, mapping = aes(x = arable_land, y = gni_pcap, color = Region))+
#  geom_point() +
#  geom_smooth(method = "lm", formula = y ~ x,
#              aes(linetype = "Linear (m13)"), color = "black") +
#  geom_line(data = transform(data, pred = exp(predict(m24))), 
#            aes(y = pred, linetype = "Exponential (m24)"), color = "red", size = 1) +
#  labs(title = "(m13, m24) Arable Land (% of total land) vs. GNI per Capita (adj. 2015 USD)",
#       x="Arable Land (% of total land)",
#       y="GNI per Capita (adj. 2015 USD)") +
#  theme(plot.title = element_text(hjust = 0.5))

#ggplot(data, mapping = aes(x = trade, y = gni_pcap, color = Region))+
#  geom_point() +
#  geom_smooth(method = "lm", formula = y ~ x, color = "black") +
#  labs(title = "(m14) Trade (% of GDP) vs. GNI per Capita (adj. 2015 USD)",
#       x="Trade (% of GDP)",
#       y="GNI per Capita (adj. 2015 USD)") +
#  theme(plot.title = element_text(hjust = 0.5))

#ggplot(data, mapping = aes(x = mfg_valAdd, y = gni_pcap, color = Region))+
#  geom_point() +
#  geom_smooth(method = "lm", formula = y ~ x, color = "black") +
#  labs(title = "(m3) Manufacturing (value added, % of GDP) vs. \nGNI per Capita (adj. 2015 USD)",
#       x="Manufacturing (value added, % of GDP)",
#       y="GNI per Capita (adj. 2015 USD)") +
#  theme(plot.title = element_text(hjust = 0.5))

agExp_plot = ggplot(data, mapping = aes(x = ag_exp_raw, y = gni_pcap, color = Region))+
  geom_point() +
  geom_smooth(method = "lm", formula = y ~ x,
              aes(linetype = "Linear (m4)"), color = "black") +
  geom_line(data = transform(data, pred = exp(predict(m23))), 
            aes(y = pred, linetype = "Exponential (m23)"), color = "red", size = 1) +
  labs(title = "(m4, m23) Agricultural raw materials \nexports (% of merchandise exports) vs. \nGNI per Capita (adj. 2015 USD)",
       x="Agricultural raw materials exports (% of merchandise exports)",
       y="GNI per Capita (adj. 2015 USD)") +
  theme(plot.title = element_text(hjust = 0.5))

#ggplot(data, mapping = aes(x = ore_met_exp, y = gni_pcap, color = Region))+
#  geom_point() +
#  geom_smooth(method = "lm", formula = y ~ x,
#              aes(linetype = "Linear (m12)"), color = "black") +
#  geom_line(data = transform(data, pred = exp(predict(m26))), 
#            aes(y = pred, linetype = "Exponential (m26)"), color = "red", size = 1) +
#  labs(title = "(m12, m26) Ores and metals exports \n(% of merchandise exports) vs. \nGNI per Capita (adj. 2015 USD)",
#       x="Ores and metals exports (% of merchandise exports)",
#       y="GNI per Capita (adj. 2015 USD)") +
#  theme(plot.title = element_text(hjust = 0.5))

#ggplot(data, mapping = aes(x = mineral_rents, y = gni_pcap, color = Region))+
#  geom_point() +
#  geom_smooth(method = "lm", formula = y ~ x,
#              aes(linetype = "Linear (m11)"), color = "black") +
#  geom_line(data = transform(data, pred = exp(predict(m25))), 
#            aes(y = pred, linetype = "Exponential (m25)"), color = "red", size = 1) +
#  labs(title = "(m11, m25) Mineral rents (% of GDP) vs. \nGNI per Capita (adj. 2015 USD)",
#       x="Mineral rents (% of GDP)",
#       y="GNI per Capita (adj. 2015 USD)") +
#  theme(plot.title = element_text(hjust = 0.5))

#ggplot(data, mapping = aes(x = oil_rents, y = gni_pcap, color = Region))+
#  geom_point() +
#  geom_smooth(method = "lm", formula = y ~ x,
#              aes(linetype = "Linear (m5)"), color = "black") +
#  geom_line(data = transform(data, pred = exp(predict(m24))), 
#            aes(y = pred, linetype = "Exponential (m24)"), color = "red", size = 1) +
#  labs(title = "(m5, m24) Oil rents (% of GDP) vs. \nGNI per Capita (adj. 2015 USD)",
#       x="Oil rents (% of GDP)",
#       y="GNI per Capita (adj. 2015 USD)") +
#  theme(plot.title = element_text(hjust = 0.5))

childLabor_plot = ggplot(data, mapping = aes(x = child_labor, y = gni_pcap, color = Region))+
  geom_point() +
  geom_smooth(method = "lm", formula = y ~ x,
              aes(linetype = "Linear (m9)"), color = "black") +
  geom_line(data = transform(data, pred = exp(predict(m22))), 
            aes(y = pred, linetype = "Exponential (m22)"), color = "red", size = 1) +
  labs(title = "(m9, m22) Children in Employment (% of Children) vs. GNI per Capita (adj. 2015 USD)",
       x="Children in Employment (% of Children)",
       y="GNI per Capita (adj. 2015 USD)") +
  theme(plot.title = element_text(hjust = 0.5))

#ggplot(data, mapping = aes(x = labor_male, y = gni_pcap, color = Region))+
#  geom_point() +
#  geom_smooth(method = "lm", formula = y ~ x, color = "black") +
#  labs(title = "(m6) Labor force participation rate, male \n(% of male population ages 15+) vs. \nGNI per Capita (adj. 2015 USD)",
#       x="Labor force participation rate, male (% of male population ages 15+)",
#       y="GNI per Capita (adj. 2015 USD)") +
#  theme(plot.title = element_text(hjust = 0.5))

#ggplot(data, mapping = aes(x = labor_female, y = gni_pcap, color = Region))+
#  geom_point() +
#  geom_smooth(method = "lm", formula = y ~ x, color = "black") +
#  labs(title = "(m10) Labor force participation rate, female \n(% of female population ages 15+) vs. GNI per Capita (adj. 2015 USD)",
#       x="Labor force participation rate, female \n(% of female population ages 15+)",
#       y="GNI per Capita (adj. 2015 USD)") +
#  theme(plot.title = element_text(hjust = 0.5))

#ggplot(data, mapping = aes(x = labor_basicEd, y = gni_pcap, color = Region))+
#  geom_point() +
#  geom_smooth(method = "lm", formula = y ~ x, color = "black") +
#  labs(title = "(m16) Labor force with basic education (% of total \nworking-age population with basic education) vs.\n GNI per Capita (adj. 2015 USD)",
#       x="Labor force with basic education (% of total \nworking-age population with basic education)",
#       y="GNI per Capita (adj. 2015 USD)") +
#  theme(plot.title = element_text(hjust = 0.5))

#ggplot(data, mapping = aes(x = labor_intEd, y = gni_pcap, color = Region))+
#  geom_point() +
#  geom_smooth(method = "lm", formula = y ~ x, color = "black") +
#  labs(title = "(m7) Labor force with intermediate education (% of total \nworking-age population with intermediate education) vs. \nGNI per Capita (adj. 2015 USD)",
#       x="Labor force with intermediate education (% of total \nworking-age population with intermediate education)",
#       y="GNI per Capita (adj. 2015 USD)") +
#  theme(plot.title = element_text(hjust = 0.5), axis.title.x = element_text(hjust = 0.5), axis.title.y = element_text(hjust = 0.5))

advEd_plot = ggplot(data, mapping = aes(x = labor_advEd, y = gni_pcap, color = Region))+
  geom_point() +
  geom_smooth(method = "lm", formula = y ~ x,
    aes(linetype = "Linear (m17)"), color = "black") +
  geom_line(data = transform(data, pred = exp(predict(m21))), 
            aes(y = pred, linetype = "Exponential (m21)"), color = "red", size = 1) +
  labs(title = "(m17, m21) Labor force with advanced education (% of total \nworking-age population with advanced education) vs.\n GNI per Capita (adj. 2015 USD)",
       x="Labor force with advanced education (% of total \nworking-age population with advanced education)",
       y="GNI per Capita (adj. 2015 USD)") +
  theme(plot.title = element_text(hjust = 0.5))

#ggplot(data, mapping = aes(x = rich_money, y = gni_pcap, color = Region))+
#  geom_point() +
#  geom_smooth(method = "lm", formula = y ~ x, color = "black") +
#  labs(title = "(m18) Income share held by highest 10% vs. GNI per Capita (adj. 2015 USD)",
#       x="Income share held by highest 10%",
#       y="GNI per Capita (adj. 2015 USD)") +
#  theme(plot.title = element_text(hjust = 0.5))

primaryCmplt_plot = ggplot(data, aes(x = primary_cmplt, y = gni_pcap)) +
  geom_point(aes(color = Region), alpha = 0.5) +
  geom_smooth(method = "lm", formula = y ~ x, 
              aes(linetype = "Linear (m19)"), color = "black") +
  geom_line(data = transform(data, pred = exp(predict(m20))), 
            aes(y = pred, linetype = "Exponential (m20)"), color = "red", size = 1) +
  labs(title = "(m19, m20) Primary completion rate, total \n(% of relevant age group) vs. \nGNI per Capita (adj. 2015 USD)",
       x = "Primary Completion Rate (%)",
       y = "GNI per Capita (adj. 2015 USD)",
       linetype = "Model Type") +
  theme(plot.title = element_text(hjust = 0.5))

# Save Plots
if (FALSE){
ggsave("Plots/water.png", plot = water_plot, width = 10, height = 6)
ggsave("Plots/elec.png", plot = elec_plot, width = 10, height = 6)
ggsave("Plots/rural.png", plot = rural_plot, width = 10, height = 6)
ggsave("Plots/urban.png", plot = urban_plot, width = 10, height = 6)
ggsave("Plots/agExp.png", plot = agExp_plot, width = 10, height = 6)
ggsave("Plots/childLabor.png", plot = childLabor_plot, width = 10, height = 6)
ggsave("Plots/advEd.png", plot = advEd_plot, width = 10, height = 6)
ggsave("Plots/primaryCmplt.png", plot = primaryCmplt_plot, width = 10, height = 6)
}
#} # If false end

####
# FINAL SIGNIFICANT MODEL LIST:
# m20, m21, m22, m23, m24, m25, m26, m27
###

# Test nonlinear models:
m20 = lm(log(gni_pcap) ~ primary_cmplt, data = data) # original: m19. Keep
m21 = lm(log(gni_pcap) ~ labor_advEd, data = data)   # original: m17. Keep
m22 = lm(log(gni_pcap) ~ child_labor, data = data)   # original: m9.  Keep
m23 = lm(log(gni_pcap) ~ ag_exp_raw, data = data)    # original: m4.  Just barely insignificant, 0.0518. Keep for now but subject to removal
m24 = lm(log(gni_pcap) ~ urban_pop, data = data)     # original: m8.  Performed best. Keep
m25 = lm(log(gni_pcap) ~ drink_water, data = data)  # original: m1. Best
m26 = lm(log(gni_pcap) ~ have_elec, data = data)    # original: m15. Best
m27 = lm(log(gni_pcap) ~ rural_pop, data = data)    # original: m2. Best

# Generate summaries and analysis plots:
for (i in 20:27){
  model = paste0("m", i)
  m = get(model)
  
  print(i)
  print(summary(m))
  
  #label = ggplot() + annotate("text", x = 0.5, y = 0.5, label = model, size = 20) + theme_void()
  #ap = autoplot(m, which = 1:6, ncol = 2, label.size = 3)
  #cm = check_model(m)
  
  #print(label)
  #print(ap)
  #print(cm)
  
  #ggsave(filename = paste0("Plots/", model, "_ap.png"), plot = ap, width = 12, height = 8)
  #ggsave(filename = paste0("Plots/", model, "_cm.png"), plot = plot(cm), width = 12, height = 8)
}

# Issues observed in autoplot and check_model outputs:
# m20 no issues observed
# m21 residuals vs. fitted isn't great, with small peak and then drop. But still okay. Everything else is fine.
# m22 no issues observed
# m23 no issues observed but large clustering of data at low values of ag exports, sparse elsewhere
# m24 no issues observed
# m25 slight upward skew for residuals on both ends of predictor range but looks good otherwise.
# m26 no issues observed but large cluster of data at high values of electricity access, not sparse elsewhere though. Doesn't seem to violate homoscedasticity.
# m27 no issues observed

#####
# MUTATE TO LOG FOR GNI SO WE HAVE EXPONENTIAL MODELS FOR EACH
#####

data = data %>%
  mutate(log_gni = log(gni_pcap))

# Put together a candidate model and analyze using olsrr.
candidate_model = lm(log_gni ~ primary_cmplt + labor_advEd + child_labor + ag_exp_raw + urban_pop + drink_water + have_elec + rural_pop + Region, data = data)

modelSubsets = ols_step_best_subset(candidate_model)
print(modelSubsets)
#plot(modelSubsets)

# Save analysis as image
if (FALSE){
analysis_df <- modelSubsets$metrics

p1 <- ggplot(analysis_df, aes(x = n, y = rsquare)) + geom_line(color="blue") + geom_point() + labs(title="R-Square")
p2 <- ggplot(analysis_df, aes(x = n, y = adjr)) + geom_line(color="blue") + geom_point() + labs(title="Adj. R-Square")
p3 <- ggplot(analysis_df, aes(x = n, y = cp)) + geom_line(color="blue") + geom_point() + labs(title="Mallows' Cp")
p4 <- ggplot(analysis_df, aes(x = n, y = aic)) + geom_line(color="blue") + geom_point() + labs(title="AIC")
p5 <- ggplot(analysis_df, aes(x = n, y = sbic)) + geom_line(color="blue") + geom_point() + labs(title="SBIC")
p6 <- ggplot(analysis_df, aes(x = n, y = sbc)) + geom_line(color="blue") + geom_point() + labs(title="SBC/BIC")
full_analysis_plot <- (p1 + p2 + p3) / (p4 + p5 + p6)

ggsave("Plots/BestSubsetAnalysis.png", plot = full_analysis_plot, width = 15, height = 10, dpi = 300)
}

# Best Subsets Regression                                             
#----------------------------------------------------------------------------------------------------------------
#  Model Index    Predictors
#----------------------------------------------------------------------------------------------------------------
# 1         drink_water                                                                                       
# 2         have_elec rural_pop                                                                               
# 3         labor_advEd urban_pop have_elec                                                                   
# 4         labor_advEd urban_pop have_elec Region                                                            
# 5         labor_advEd drink_water have_elec rural_pop Region                                                
# 6         labor_advEd ag_exp_raw urban_pop drink_water have_elec Region                                     
# 7         labor_advEd child_labor ag_exp_raw urban_pop drink_water have_elec Region                         
# 8         primary_cmplt labor_advEd child_labor ag_exp_raw drink_water have_elec rural_pop Region           
# 9         primary_cmplt labor_advEd child_labor ag_exp_raw urban_pop drink_water have_elec rural_pop Region 
#----------------------------------------------------------------------------------------------------------------

# Model 3 has best SBC, AIC, and near the plateau of adj. R^2.
# Model 5 has second best AIC, not being much higher than 3, but SBC for model 5 is significantly higher. Does add some adj. R^2 but not worth it because of SBC.
# All predictors for 3 and 5 DO NOT violate any of the OLS assumptions, but have_elec is borderline for homoscedasticity. Have_water is better for homoscedasticity, but both seem okay.
# Choose model 3.

# Check summary, anova, and multicolinearity for 3 and 5.
final_model_3 = lm(log_gni ~ labor_advEd + urban_pop + have_elec, data = data)
final_model_5 = lm(log_gni ~ labor_advEd + drink_water + have_elec + rural_pop + Region, data = data)

print("Summary for final model 3:")
summary(final_model_3)
print("Summary for final model 5:")
summary(final_model_5)

# Summary shows an increase from model 3->5 adj. R^2 of 0.6979 -> 0.7066. This is minimal.
# Additionally, water access and region are not close to being statistically significant (water is closest but p still is 0.114)

print("Anova for final model 3:")
anova(final_model_3)
print("Anova for final model 5:")
anova(final_model_5)

print("VIF for final model 3:")
vif(final_model_3)
print("VIF for final model 5:")
vif(final_model_5)

# VIF shows model 3 has good independence for selected variables, 5 is good too but still is worse.

# Based on everything, we select model 3 as our final model. Each predictor satisfies all necessary assumptions and the model provides good performance without risk of overfitting.
final_model = final_model_3

# Get coefficients
cf3 = coef(final_model_3)
cf5 = coef(final_model_5)

# final_model_3 = lm(log_gni ~ labor_advEd + urban_pop + have_elec, data = data)
# final_model_5 = lm(log_gni ~ labor_advEd + drink_water + have_elec + rural_pop + Region, data = data)

# Calculate means
mean_urbanPop = mean(data$urban_pop)
mean_advEd = mean(data$labor_advEd)
mean_elec = mean(data$have_elec)
mean_water = mean(data$drink_water)
mean_ruralPop = mean(data$rural_pop)

# Create x values
x_urbanPop = seq(min(data$urban_pop), max(data$urban_pop), length.out = 100)
x_advEd = seq(min(data$labor_advEd), max(data$labor_advEd), length.out = 100)
x_elec = seq(min(data$have_elec), max(data$have_elec), length.out = 100)
x_water = seq(min(data$drink_water), max(data$drink_water), length.out = 100)
x_ruralPop = seq(min(data$rural_pop), max(data$rural_pop), length.out = 100)

# Calculate predictions. First final model 3, then final model 5
gniPreds_urbanPop_3 = exp(cf3[1] + (cf3[2] * mean_adv) + (cf3[3] * x_urbanPop) + (cf3[4] * mean_elec))
gniPreds_advEd_3 = exp(cf3[1] + (cf3[2] * x_advEd) + (cf3[3] * mean_urbanPop) + (cf3[4] * mean_elec))
gniPreds_elec_3 = exp(cf3[1] + (cf3[2] * mean_adv) + (cf3[3] * mean_urbanPop) + (cf3[4] * x_elec))

gniPreds_advEd_5 = exp(cf[1] + (cf[2] * mean_adv) + (cf[3] * x_urbanPop) + (cf[4] * mean_elec))
gniPreds_elec_5 = exp(cf[1] + (cf[2] * mean_adv) + (cf[3] * x_urbanPop) + (cf[4] * mean_elec))
gniPreds_water_5 = exp(cf[1] + (cf[2] * mean_adv) + (cf[3] * x_urbanPop) + (cf[4] * mean_elec))
gniPreds_ruralPop_5 = exp(cf[1] + (cf[2] * mean_adv) + (cf[3] * x_urbanPop) + (cf[4] * mean_elec))

# Define the variable you want to "test" (x-axis)
x_seq <- seq(min(data$labor_advEd, na.rm=TRUE), 
             max(data$labor_advEd, na.rm=TRUE), length.out=100)

# Get all unique regions from your data
all_regions <- unique(data$Region)

# Create a grid: 100 rows for every region
pred_grid <- expand.grid(labor_advEd = x_seq, 
                         Region = all_regions)

# Add the "held constant" variables (means)
pred_grid$drink_water <- mean(data$drink_water, na.rm=TRUE)
pred_grid$have_elec   <- mean(data$have_elec, na.rm=TRUE)
pred_grid$rural_pop   <- mean(data$rural_pop, na.rm=TRUE)

# Generate predictions for all regions at once
# We use exp() because the model was trained on log_gni
pred_grid$gni_hat <- exp(predict(final_model_5, newdata = pred_grid))

ggplot(data, aes(x = labor_advEd, y = gni_pcap)) +
  # 1. Plot the raw data points in the background
  geom_point(aes(color = Region), alpha = 0.2) + 
  # 2. Plot the model lines for each region
  geom_line(data = pred_grid, aes(x = labor_advEd, y = gni_hat, color = Region), 
            size = 1.2) +
  labs(title = "Model 5: Regional Comparison of Advanced Education Effects",
       subtitle = "Other predictors held at their global mean",
       x = "Labor Force with Advanced Education (%)",
       y = "GNI per Capita (2015 USD)") +
  theme_minimal()

# Plot the raw data and the "Constant-Adjusted" line
ggplot(data, aes(x = urban_pop, y = gni_pcap)) +
  geom_point(alpha = 0.3) +
  geom_line(data = data.frame(x = x_seq, y = y_values), 
            aes(x = x, y = y), color = "red", size = 1) +
  labs(title = "Model 3: Urban Pop effect (others held at mean)")