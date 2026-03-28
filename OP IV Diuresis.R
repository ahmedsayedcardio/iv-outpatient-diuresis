#Packages
library(httr2)
library(jsonlite)
library(data.table)
library(dplyr)
library(janitor)

#Inpatient APIs
ip_apis <- c(
  `2013` = "https://data.cms.gov/data-api/v1/dataset/c7019ecb-8f6f-4548-8eed-ee5bf35b51d5/data",
  `2014` = "https://data.cms.gov/data-api/v1/dataset/fe4fa35f-d27b-4bed-a14a-35528a870de0/data",
  `2015` = "https://data.cms.gov/data-api/v1/dataset/98069693-d269-46a1-8e7e-f360f37a2031/data",
  `2016` = "https://data.cms.gov/data-api/v1/dataset/81dca731-b739-4a0a-b2ec-555f6cf04ea6/data",
  `2017` = "https://data.cms.gov/data-api/v1/dataset/7c638c5f-7e8c-4346-8a25-9b37a0ed36dd/data",
  `2018` = "https://data.cms.gov/data-api/v1/dataset/25b5b6c9-977d-4f7a-aaa7-7dde72f3d5cc/data",
  `2019` = "https://data.cms.gov/data-api/v1/dataset/337617e5-c18b-4cb9-8152-34851747584b/data",
  `2020` = "https://data.cms.gov/data-api/v1/dataset/e583de38-33d7-4895-a115-0d8af74f0795/data",
  `2021` = "https://data.cms.gov/data-api/v1/dataset/68739613-1a4f-40ae-9664-49da07abd81f/data",
  `2022` = "https://data.cms.gov/data-api/v1/dataset/5c8b315f-a2a3-4b2f-bba6-7be19f8d7edc/data",
  `2023` = "https://data.cms.gov/data-api/v1/dataset/2941ab09-8cee-49d8-9703-f3c5b854e388/data"
)


#Import inpatient data
lapply(1:length(ip_apis),
       function(api)
         request(ip_apis[api]) %>%
         req_url_query(`filter[hcpcs-filter][condition][path]` = "DRG_Cd",
                       `filter[hcpcs-filter][condition][operator]` = "IN",
                       `filter[hcpcs-filter][condition][value][0]` = "291",
                       `filter[hcpcs-filter][condition][value][1]` = "292",
                       `filter[hcpcs-filter][condition][value][2]` = "293") %>%
         req_perform %>%
         resp_body_json(simplifyVector = TRUE) %>%
         mutate(year = 2012 + api) %>%
         data.table %>%
         clean_names
         
       ) %>%
  rbindlist -> ip_df
setnames(ip_df, "rndrng_prvdr_geo_desc", "state")

#IV diuresis APIs
op_apis <- c(
  `2013` = "https://data.cms.gov/data-api/v1/dataset/3c2a4756-0a8c-4e4d-845a-6ad169cb13d3/data",
  `2014` = "https://data.cms.gov/data-api/v1/dataset/28181bd2-b377-4003-b73a-4bd92d1db4a9/data",
  `2015` = "https://data.cms.gov/data-api/v1/dataset/dbee9609-2c90-43ca-b1b8-161bd9cfcdb2/data",
  `2016` = "https://data.cms.gov/data-api/v1/dataset/c7d3f18c-2f00-4553-8cd1-871b727d5cdd/data",
  `2017` = "https://data.cms.gov/data-api/v1/dataset/8e96a9f2-ce6e-46fd-b30d-8c695c756bfd/data",
  `2018` = "https://data.cms.gov/data-api/v1/dataset/05a85700-052f-4509-af43-7042b9b35868/data",
  `2019` = "https://data.cms.gov/data-api/v1/dataset/673030ae-ceed-4561-8fca-b1275395a86a/data",
  `2020` = "https://data.cms.gov/data-api/v1/dataset/31eb3018-43e0-4259-a0d8-e7a1112ffd08/data",
  `2021` = "https://data.cms.gov/data-api/v1/dataset/f00f462f-bb61-48b1-a321-1c50d78ce175/data",
  `2022` = "https://data.cms.gov/data-api/v1/dataset/87304f15-9ed0-41dc-a141-6141a0327453/data",
  `2023` = "https://data.cms.gov/data-api/v1/dataset/6fea9d79-0129-4e4c-b1b8-23cd86a4f435/data"
)

#Import outpatient data
lapply(1:length(op_apis),
       function(api)
         request(op_apis[api]) %>%
         req_url_query(`filter[opiv-filter][condition][path]` = "HCPCS_Cd",
                       `filter[opiv-filter][condition][operator]` = "HCPCS_Cd",
                       `filter[opiv-filter][condition][value][0]` = "J1940",
         ) %>%
         req_perform %>%
         resp_body_json(simplifyVector = TRUE) %>%
         mutate(year = 2012 + api) %>%
         data.table %>%
         clean_names
       
) %>%
  rbindlist -> op_df
setnames(op_df, "rndrng_prvdr_geo_desc", "state")

#Import CSV
medicare_df <- fread("Medicare_Monthly_Enrollment_Nov_2025.csv") %>% clean_names
medicare_df <- medicare_df[bene_geo_lvl != "County"]
setnames(medicare_df, "bene_state_desc", "state")
medicare_df[, a_tot_benes := as.numeric(a_orgnl_mdcr_benes)]
medicare_df[, b_tot_benes := as.numeric(b_orgnl_mdcr_benes)]

###Aggregate 
##Nationally (n = National)
#IP
ip_df[rndrng_prvdr_geo_lvl == "National" & drg_cd %in% c(291, 292, 293), 
      .(n = sum(tot_dschrgs %>% as.numeric)), by = year] -> opn_y_df
ipn_y_df[, type := "Inpatient"]
#OP
op_df[place_of_srvc == "O" & rndrng_prvdr_geo_lvl == "National",
      .(n = sum(tot_bene_day_srvcs %>% as.numeric)), by = year] -> opn_y_df
opn_y_df[, type := "Outpatient"]

##By state (s = state)
#IP
ip_df[rndrng_prvdr_geo_lvl == "State" & drg_cd %in% c(291, 292, 293), 
      .(n = sum(tot_dschrgs %>% as.numeric)), by = .(state, year)] -> ips_y_df
ips_y_df[, type := "Inpatient"]

#OP
op_df[place_of_srvc == "O"  & rndrng_prvdr_geo_lvl == "State", 
      .(n = sum(tot_bene_day_srvcs %>% as.numeric)), by = .(state, year)] -> ops_y_df
ops_y_df[, type := "Outpatient"]

#N makes sense per 10.1016/j.cardfail.2025.12.006

##Join to N of medicare
#State
merge(ops_y_df %>% rename(op = n), ips_y_df %>% rename(ip = n), by = c("state", "year")) -> s_y_df
merge(s_y_df, medicare_df[bene_state_abrvtn != "US" & month == "Year", .(year, state, a_tot_benes, b_tot_benes)],
      all.x = T, all.y = F, by = c("year", "state")) -> s_y_df
s_y_df[, ip_rate := 100000*ip/a_tot_benes]
s_y_df[, op_rate := 100000*op/b_tot_benes]
s_y_df[, ratio := op_rate/ip_rate]

#National
rbind(ipn_y_df, opn_y_df) -> n_y_df
merge(n_y_df, medicare_df[bene_state_abrvtn == "US" & month == "Year", .(year, a_tot_benes, b_tot_benes)],
      all.x = T, all.y = F, by = "year") -> n_y_df
n_y_df[type == "Inpatient", rate := 100000*n/a_tot_benes]
n_y_df[type == "Outpatient", rate := 100000*n/b_tot_benes]
n_y_df[, .(ratio = rate[type == "Outpatient"]/rate[type == "Inpatient"]), by = year] -> ratio_n_y_df



#Round to 2 decimals and include zero if needed
r2 <- function(x) {
  x %>% round(2) %>% format(nsmall = 2)
}
#Rename, exponentiate, and create a PE/CI column
rexp <- function(rms_contrast, contrast_label = "HR") {
  data.frame(
    lci = rms_contrast$Lower,
    pe = rms_contrast$Contrast,
    uci = rms_contrast$Upper
  ) %>%
    mutate(across(.cols = c("pe", "lci", "uci"), #Exponentiate to make output more interpretable
                  ~ . %>% exp),
           pe_ci = paste0("(", contrast_label, ": ", pe %>% r2,
                          "; 95% CI: ", lci %>% r2, " to ", 
                          uci %>% r2, ")"))
}

library(rms)
datadist(n_y_df) -> dd
options(datadist = "dd")
Glm(data = n_y_df[type == "Inpatient"], 
    family = poisson,
    offset = a_tot_benes,
    formula = n ~ year
    ) %>% contrast(a = data.frame(year = 1), data.frame(year = 0)) %>% rexp
Glm(data = n_y_df[type == "Outpatient"], 
    family = poisson,
    offset = a_tot_benes,
    formula = n ~ year
) %>% contrast(a = data.frame(year = 1), data.frame(year = 0)) %>% rexp
library(ggthemes)
library(ggpubr)
#Create Figure 1A
ggplot(data = n_y_df,
       aes(x = year,
           y = rate,
           fill = type)
       ) + 
  geom_col(position = "dodge", color = "black", lwd = 1.5, width = 0.8) +
  geom_text(inherit.aes = F,
            data = ratio_n_y_df,
            aes(label = ratio %>% round(2),
            x = year,
            y = -40),
            fontface = "bold", size = 8) +
  scale_fill_tableau(name = "") +
  #Add scales
  scale_x_continuous(
    name = "Year",
    expand = c(0.02, 0),
    breaks = seq(0, 3000, 1)) +
  scale_y_continuous(
    breaks = seq(0, 2000, 100),
    name = "Rate (Events per 100,000 beneficiaries)") +
  #Theme
  theme_pubclean() +
  theme(text = element_text(size = 23),
        plot.title=element_text(face = "bold",hjust = 0.0, size = 18),
        plot.subtitle = element_text(face = "bold", size = 10, hjust = 0.0, color = "grey45"),
        axis.text.x = element_text(size = 20, face = "bold"),
        axis.text.y = element_text(size = 20, face = "bold"),
        axis.title.x = element_text(size = 25, face = "bold"),
        axis.title.y = element_text(size = 21, face = "bold"),
        axis.line = element_line(colour = "black", 
                                 linewidth = 1.2),
        plot.margin = margin(0.5, 1, 0.5, 1, "cm"),
        legend.position = "bottom",
        legend.text = element_text(size = 16, face = "bold"),
        legend.key.width = unit(4, "cm"),
        legend.key.height = unit(0.75, "cm"),
        legend.background = element_rect(fill = "transparent"),
        legend.key = element_rect(fill = "transparent")
  ) -> fig_1a
#Save
ggsave(filename = "Figures/Figure 1A.pdf",
       dpi = 600,
       height = 9, width = 16)

#Create Figure 1B
library(usmap)
library(ggh4x)
plot_usmap(regions = "states",
           data = s_y_df[year == 2023 & state %in% (us_map()$full %>% unique)],
           values = "ratio") +
  scale_fill_viridis_b(name = "Ratio of outpatient IV diuresis to inpatient hospital discharges",
                       guide = guide_colorbar(direction = "horizontal",
                                              title.position = "top",
                                              barwidth = 25)) +
  ggtitle(waiver()) +
  #Customize theme
  theme(
    legend.text = element_text(size = 14, face = "bold"), 
    legend.title = element_text(size = 14, face = "bold"),
    legend.background = element_blank(), 
    legend.key = element_blank(),
    legend.position = c(0.55, 0.05),
    plot.title=element_text(face = "bold", hjust = 0.5, size = 25),
    panel.grid = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    panel.background = element_blank(),
    axis.title = element_blank()) +
  force_panelsizes(rows = unit(10, "in"),
                   cols = unit(16, "in")) -> fig_1b

#Save
ggsave(filename = "Figures/Figure 1B.pdf",
       dpi = 600,
       height = 9, width = 16)


#Results
library(scales)
#Ns
n_y_df[type == "Inpatient", sum(n) %>% comma]
n_y_df[type == "Outpatient", sum(n) %>% comma]

#Rate
n_y_df[type == "Inpatient" & year %in% c(2013, 2023), rate %>% round]
n_y_df[type == "Outpatient" & year %in% c(2013, 2023), rate %>% round]

#Highest in 2023
s_y_df[year == 2023][order(ratio), .(state, ratio = ratio %>% round(digits = 3))]

#Highest increase
s_y_df[, .(ratio_2023 = ratio[year == 2023] %>% round(2),
           ratio_2013 = ratio[year == 2013] %>% round(2),
           r_ratio = round(ratio[year == 2023]/ratio[year == 2013], 2)
           ), 
           by = state][order(r_ratio)]


