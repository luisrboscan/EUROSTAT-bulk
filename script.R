setwd("C:/path")

library(car)
library(readr)
library(dplyr)
library(circlize)
full202052 <- read_csv("C:/path/full202052.dat")

x <- as.data.frame(full202052)
write.csv(x, "full202052.csv", row.names = FALSE )


# Focus on Denmark's trade --------------
DK<-x %>%
  filter(DECLARANT == "008")

# DK's trade with the EU ---------------
DK_EU <- DK %>%
  filter(TRADE_TYPE=="I")

DK_EU_IX <- DK_EU %>%
  group_by(DECLARANT_ISO,PARTNER_ISO, FLOW, PRODUCT_SECTION) %>%
  summarise(value=sum(VALUE_IN_EUROS, na.rm = FALSE))

#write.csv(DK_EU_IX, "DK_EU_IX.csv", row.names = FALSE )

# Denmark's top import partners in the EU -----------
DK_EU_IM <- DK_EU_IX %>%
  filter(FLOW=="1"& PRODUCT_SECTION == "TO") %>%
    summarise(value=sum(value, na.rm = FALSE)/1000000000) %>%
  select(PARTNER_ISO, DECLARANT_ISO, value) %>%
  arrange(desc(value))

#Denmark's top 10 import partners --------
DK_EU_IM <- head(DK_EU_IM, 10)

#Visualization with a Chord Diagram -------
chordDiagram(DK_EU_IM)

# Denmark's top export partners in the EU ------------
DK_EU_EX <- DK_EU_IX %>%
  filter(FLOW=="2"& PRODUCT_SECTION == "TO") %>%
  summarise(value=sum(value, na.rm = FALSE)/1000000000) %>%
  select(DECLARANT_ISO, PARTNER_ISO, value) %>%
  arrange(desc(value))

#Denmark's top 10 import partners ------
DK_EU_EX <- head(DK_EU_EX, 10)

#Visualization with a Chord Diagram --------
chordDiagram(DK_EU_EX)


# DK's trade with non-EU countries -------------
DK_NON_EU <- DK %>%
  filter(TRADE_TYPE=="E")

DK_NON_EU_IX <- DK_NON_EU %>%
  group_by(PARTNER,PARTNER_ISO, FLOW, PRODUCT_SECTION) %>%
  summarise(value=sum(VALUE_IN_EUROS, na.rm = FALSE))

#Denmark's top import partners outside the EU ----------
DK_NON_EU_IM <- DK_NON_EU_IX %>%
  filter(FLOW=="1"& PRODUCT_SECTION == "TO") %>%
  summarise(value=sum(value, na.rm = FALSE)) %>%
  arrange(desc(value))

#Denmark's top export partners outside the EU ----------
DK_NON_EU_EX <- DK_NON_EU_IX %>%
  filter(FLOW=="2"& PRODUCT_SECTION == "TO") %>%
  summarise(value=sum(value, na.rm = FALSE)) %>%
  arrange(desc(value))







write.csv(DK_EU_IM, "DK_EU_IM.csv", row.names = FALSE )
