# R script to derive simple insights from full202052.dat
# Written by Luis Boscán

setwd("C:path/full202052.dat")

library(readr)
library(dplyr)
library(circlize)

full202052 <- read_csv("C:path/full202052.dat")
x <- as.data.frame(full202052)

# Focus of the simple analysis is on Denmark's trade ----
# I rely on customs data declared by Denmark to EUROSTAT
DK <- x %>%
  filter(DECLARANT == "008")

#  Part 1: DK's trade with the EU ---------------

#This df contains all goods imports and exports between DK and the EU
DK_EU <- DK %>%
  filter(TRADE_TYPE=="I")
  
#This df contains the value of goods imports and exports to the EU ----
DK_EU_trade <- DK_EU %>%
  filter(PRODUCT_SECTION == "TO") %>%
  group_by(FLOW) %>%
  summarise(value=sum(VALUE_IN_EUROS, na.rm = FALSE)/1000000000)

# Insight 1: The value of DK's imports FROM the EU amounts to 59 billion EUR
# Insight 2: The value of DK's exports TO the EU amounts to 49 billion EUR


# Identifying Denmark's import partners in the EU -----------
# I use the product section opening of up to 21 categories
DK_EU_IX <- DK_EU %>%
  group_by(DECLARANT_ISO,PARTNER_ISO, FLOW, PRODUCT_SECTION) %>%
  summarise(value=sum(VALUE_IN_EUROS, na.rm = FALSE))

# I focus on imports and arrange the data for visualization
DK_EU_IM <- DK_EU_IX %>%
  filter(FLOW=="1"& PRODUCT_SECTION == "TO") %>%
  summarise(value=sum(value, na.rm = FALSE)/1000000000) %>%
  arrange(desc(value)) %>%
  select(PARTNER_ISO, DECLARANT_ISO, value)
 
#Denmark's top 10 import partners --------
DK_EU_IM <- head(DK_EU_IM, 10)

#Visualizing Denmark's EU imports with a Chord Diagram -------
chordDiagram(DK_EU_IM)

# Insight 3: Identifying and visualizing Denmark's top import partners in the EU

# Denmark's top export partners in the EU ------------
DK_EU_EX <- DK_EU_IX %>%
  filter(FLOW=="2"& PRODUCT_SECTION == "TO") %>%
  summarise(value=sum(value, na.rm = FALSE)/1000000000) %>%
  select(DECLARANT_ISO, PARTNER_ISO, value) %>%
  arrange(desc(value))

#Denmark's top 10 export partners ------
DK_EU_EX <- head(DK_EU_EX, 10)

#Visualization with a Chord Diagram --------
chordDiagram(DK_EU_EX)

# Insight 4: Identifying and visualizing Denmark's top import partners in the EU

# DK's trade with non-EU countries -------------

DK_NON_EU <- DK %>%
  filter(TRADE_TYPE=="E")

#This df contains the value of Denmark's goods imports and exports outsider the EU ----

DK_NON_EU_trade <- DK_NON_EU %>%
  filter(PRODUCT_SECTION == "TO") %>%
  group_by(FLOW) %>%
  summarise(value=sum(VALUE_IN_EUROS, na.rm = FALSE)/1000000000)

# Insight 5: The value of Denmark's good imports FROM outside the EU was 23.64 billion EUR
# Insight 6: The value of Denmark's good exports TO non-EU countries was 41 billion EUR

DK_NON_EU_IX <- DK_NON_EU %>%
  group_by(DECLARANT_ISO,PARTNER_ISO, FLOW, PRODUCT_SECTION) %>%
  summarise(value=sum(VALUE_IN_EUROS, na.rm = FALSE))

#Denmark's top import partners outside the EU ----------
DK_NON_EU_IM <- DK_NON_EU_IX %>%
  filter(FLOW=="1"& PRODUCT_SECTION == "TO") %>%
  summarise(value=sum(value, na.rm = FALSE)/1000000000) %>%
  arrange(desc(value)) %>%
  select(PARTNER_ISO, DECLARANT_ISO, value)

# Insight 7: identifying and visualizing Denmark's import partners outside the EU

#Denmark's top 10 import partners outside the EU --------
DK_NON_EU_IM <- head(DK_NON_EU_IM, 10)

#Visualizing Denmark's  with a Chord Diagram -------
chordDiagram(DK_NON_EU_IM)

#Denmark's top export partners outside the EU ----------
DK_NON_EU_EX <- DK_NON_EU_IX %>%
  filter(FLOW=="2"& PRODUCT_SECTION == "TO") %>%
  summarise(value=sum(value, na.rm = FALSE)/1000000000) %>%
  select(DECLARANT_ISO, PARTNER_ISO, value) %>%
  arrange(desc(value))

#Denmark's top 10 export partners outside the EU --------
DK_NON_EU_EX <- head(DK_NON_EU_EX, 10)

#Visualizing Denmark's  with a Chord Diagram -------
chordDiagram(DK_NON_EU_EX)

# Insight 8: identifying and visualizing Denmark's import partners outside the EU
