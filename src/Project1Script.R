# Load libraries
library(readr)
library(ggplot2)
library(rrr)
library(olsrr)

# Load data
data = read_csv("Data/consolidated_data.csv")
descriptions = read_csv("Data/indicator_descriptions.csv")

ggplot(data, mapping = aes(x = SH.H2O.BASW.ZS, y = NY.GNP.PCAP.KD, color = Region))+
  geom_point() +
  labs(title = "People with Basic Drinking Water (% of population) vs. \nGNI per Capita (adj. 2015 USD)",
       x="People with Basic Drinking Water (% of population)",
       y="GNI per Capita (adj. 2015 USD)") +
  theme(plot.title = element_text(hjust = 0.5))

ggplot(data, mapping = aes(x = SP.RUR.TOTL.ZS, y = NY.GNP.PCAP.KD, color = Region))+
  geom_point() +
  labs(title = "Rural Population (% of population) vs. \nGNI per Capita (adj. 2015 USD)",
       x="Rural Population (% of population)",
       y="GNI per Capita (adj. 2015 USD)") +
  theme(plot.title = element_text(hjust = 0.5))

ggplot(data, mapping = aes(x = NV.IND.MANF.ZS, y = NY.GNP.PCAP.KD, color = Region))+
  geom_point() +
  labs(title = "Manufacturing (value added, % of GDP) vs. \nGNI per Capita (adj. 2015 USD)",
       x="Manufacturing (value added, % of GDP)",
       y="GNI per Capita (adj. 2015 USD)") +
  theme(plot.title = element_text(hjust = 0.5))

ggplot(data, mapping = aes(x = TX.VAL.AGRI.ZS.UN, y = NY.GNP.PCAP.KD, color = Region))+
  geom_point() +
  labs(title = "Agricultural raw materials \nexports (% of merchandise exports) vs. \nGNI per Capita (adj. 2015 USD)",
       x="Agricultural raw materials exports (% of merchandise exports)",
       y="GNI per Capita (adj. 2015 USD)") +
  theme(plot.title = element_text(hjust = 0.5))

ggplot(data, mapping = aes(x = NY.GDP.PETR.RT.ZS, y = NY.GNP.PCAP.KD, color = Region))+
  geom_point() +
  labs(title = "Oil rents (% of GDP) vs. \nGNI per Capita (adj. 2015 USD)",
       x="Oil rents (% of GDP)",
       y="GNI per Capita (adj. 2015 USD)") +
  theme(plot.title = element_text(hjust = 0.5))

ggplot(data, mapping = aes(x = SL.TLF.CACT.MA.ZS, y = NY.GNP.PCAP.KD, color = Region))+
  geom_point() +
  labs(title = "Labor force participation rate, male \n(% of male population ages 15+) vs. \nGNI per Capita (adj. 2015 USD)",
       x="Labor force participation rate, male (% of male population ages 15+)",
       y="GNI per Capita (adj. 2015 USD)") +
  theme(plot.title = element_text(hjust = 0.5))

ggplot(data, mapping = aes(x = SL.TLF.INTM.ZS, y = NY.GNP.PCAP.KD, color = Region))+
  geom_point() +
  labs(title = "Labor force with intermediate education (% of total working-age \npopulation with intermediate education) vs. \nGNI per Capita (adj. 2015 USD)",
       x="Labor force with intermediate education /n(% of total working-age population with intermediate education)",
       y="GNI per Capita (adj. 2015 USD)") +
  theme(plot.title = element_text(hjust = 0.5), plot.x = element_text(hjust = 0.5), plot.y = element_text(hjust = 0.5))