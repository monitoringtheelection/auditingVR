## Setup libraries =============================================================
library(tidyverse)
if (!require("voterdiffR")) {
  devtools::install_github("sysilviakim/voterdiffR")
} 
if (!require("Kmisc")) {
  devtools::install_github("sysilviakim/Kmisc")
} 
if (!require("echarts4r.assets")) {
  remotes::install_github("JohnCoene/echarts4r.assets")
}
library(voterdiffR)
library(Kmisc)
library(lubridate)
library(echarts4r)
library(echarts4r.assets)
library(anomalize)
library(scales)
library(Rmisc)
library(broom)
options(digits = 5, scipen = 999)

## Variable list: sUnitNum is not always numeric e.g. unit 113b ================
vl <- list(
  name = c("szNameLast", "szNameMiddle", "szNameFirst", "sNameSuffix"),
  addr1 = c("sHouseNum", "szStreetName", "sUnitNum"),
  addr2 = c("szSitusAddress", "szSitusCity", "sSitusZip"),
  date = c("dtBirthDate", "dtRegDate", "dtOrigRegDate", "dtLastUpdate_dt"),
  num = c("sHouseNum", "sSitusZip"),
  id = c("lVoterUniqueID", "sAffNumber"),
  mat = c("szNameLast", "szNameFirst", "dtBirthDate", "sHouseNum", "sSitusZip")
)
vl$all <- Reduce(union, vl[1:4])

vl_dedup <- list(
  ln = c("szNameLast"),
  fn = c("szNameFirst"),
  dob = c("dtBirthDate"),
  addr = c("sHouseNum", "szStreetName", "sSitusZip"),
  phone = c("szPhone"),
  email = c("szEmailAddress")
)

## Custom functions ============================================================
change_graph <- function(df) {
  df %>%
    as.data.frame() %>%
    e_charts(date) %>%
    e_line(Changed, name = "Same Record, Details Changed") %>%
    e_line(Added, name = "Only in Previous (A)") %>%
    e_line(Dropped, name = "Only in Current (B)") %>%
    e_tooltip(trigger = "axis") %>%
    e_color(c("#f4a582", "#0571b0", "#ca0020")) %>%
    e_toolbox() %>%
    e_toolbox_feature("saveAsImage", title = "Save Image")
}

field_graph <- function(df) {
  df %>%
    as.data.frame() %>%
    e_charts(date) %>%
    e_line(lVoterUniqueID, bind = date_label) %>%
    e_line(szNameFirst) %>%
    e_line(szNameLast) %>%
    e_line(dtBirthDate) %>%
    e_line(szSitusAddress) %>%
    e_line(szPartyName) %>%
    e_tooltip(trigger = "axis") %>%
    e_color(c(
      "#000000", "#E69F00", "#56B4E9",
      "#009E73", "#D55E00", "#CC79A7"
    )) %>%
    e_toolbox() %>%
    e_toolbox_feature("saveAsImage", title = "Save Image")
}
