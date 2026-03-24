# Load libraries
library(readr)
library(ggplot2)
library(ggfortify)
library(qqplotr)
library(rrr)
library(olsrr)
library(dplyr)
library(performance)

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

# Generate linear models for initial tests
m1 = lm(gni_pcap ~ drink_water, data = data)
m2 = lm(gni_pcap ~ rural_pop, data = data)
m3 = lm(gni_pcap ~ mfg_valAdd, data = data)
m4 = lm(gni_pcap ~ ag_exp_raw, data = data)
m5 = lm(gni_pcap ~ oil_rents, data = data)
m6 = lm(gni_pcap ~ labor_male, data = data)
m7 = lm(gni_pcap ~ labor_intEd, data = data)
m8 = lm(gni_pcap ~ urban_pop, data = data)
m9 = lm(gni_pcap ~ child_labor, data = data)
m10 = lm(gni_pcap ~ labor_female, data = data)
m11 = lm(gni_pcap ~ mineral_rents, data = data)
m12 = lm(gni_pcap ~ ore_met_exp, data = data)
m13 = lm(gni_pcap ~ arable_land, data = data)
m14 = lm(gni_pcap ~ trade, data = data)
m15 = lm(gni_pcap ~ have_elec, data = data)
m16 = lm(gni_pcap ~ labor_basicEd, data = data)
m17 = lm(gni_pcap ~ labor_advEd, data = data)
m18 = lm(gni_pcap ~ rich_money, data = data)
m19 = lm(gni_pcap ~ primary_cmplt, data = data)

for (i in 1:19){
  model = paste0("m", i)
  m = get(model)
  
  label = ggplot() + annotate("text", x = 0.5, y = 0.5, label = model, size = 20) + theme_void()
  ap = autoplot(m, which = 1:6, ncol = 2, label.size = 3)
  cm = check_model(m)
  
  print(label)
  print(ap)
  print(cm)
  
  ggsave(filename = paste0("Plots/", model, "_ap.png"), plot = ap, width = 12, height = 8)
  ggsave(filename = paste0("Plots/", model, "_cm.png"), plot = plot(cm), width = 12, height = 8)
}