library(readr)
library(tidyverse)
# data from 2001-2004
county_data_2001 <-
  read_csv("original-data-agriculture/county_data_2001.csv")
county_data_2001$var84 <- as.numeric(county_data_2001$var84)
county_data_2002 <-
  read_csv("original-data-agriculture/county_data_2002.csv") %>% select(-countyname_2002)
county_data_2003 <-
  read_csv("original-data-agriculture/county_data_2003.csv")
county_data_2004 <-
  read_csv("original-data-agriculture/county_data_2004.csv") %>% select(-countyname_2004)

dt01_04 <-
  bind_rows(county_data_2001,
            county_data_2002,
            county_data_2003,
            county_data_2004) %>%
  arrange(var3, var1)

dt01_04_part2 <-
  dt01_04 %>% select(seq(5, ncol(dt01_04))) %>%
  apply(., 2, function(x)
    ifelse(x == 0 , NA, x))
new_dt01_04 <-
  cbind(dt01_04 %>% select(seq(1, 4)), dt01_04_part2) %>%
  filter(dt01_04_part2 %>%
           apply(., 1, function(x)
             (sum(is.na(
               x
             )) < ncol(dt01_04_part2)))) %>%
  mutate(var2 = gsub(" ", "", var2)) %>%
  mutate(var2 = gsub("　", "", var2)) %>%
  mutate(var3 = as.character(var3))

new_dt01_04 <-
  within(new_dt01_04, {
    var3[which(grepl("宝坻", var2))] <- "120224"
    var3[which(grepl("静海", var2))] <- "120223"
    var3[which(grepl("宁河", var2))] <- "120221"
    var3[which(grepl("海城市", var2))] <- "210381"
    var3[which(grepl("砀山县", var2))] <- "341321"
    var3[which(grepl("金城江区", var2))] <- "451202"
    var3[which(grepl("新都区", var2))] <- "510114"
    var3[which(grepl("瑶海区", var2))] <- "340102"
    var3[which(grepl("庐阳区", var2))] <- "340103"
    var3[which(grepl("蜀山区", var2))] <- "340104"
    var3[which(grepl("包河区", var2))] <- "340111"
    var3[which(grepl("毛集区", var2))] <- "340407"
    var3[which(grepl("叶集区", var2))] <- "341501"
    var3[which(grepl("双湖", var2))] <- "542431"
  })

# 检查是否有code出错的情况

new_dt01_04 %>% group_by(var3, var1) %>%
  summarise(count = n()) %>% filter(count > 1) %>% arrange(-count) %>% View()
new_dt01_04 %>% group_by(var3) %>%
  summarise(count = n()) %>% filter(count > 4) %>% arrange(-count) %>% View()

rm(
  county_data_2001,
  county_data_2002,
  county_data_2003,
  county_data_2004,
  dt_part1,
  dt01_04,
  dt01_04_part2
)




# data from 1996-2000
X1996_2000new <-
  read_csv("original-data-agriculture/1996-2000new.csv")

dt_part1 <-
  X1996_2000new %>%
  select(seq(8, ncol(X1996_2000new))) %>%
  apply(., 2, function(x)
    ifelse(x == 0, NA, x))

dt96_00 <-
  cbind(X1996_2000new %>% select(seq(1, 7)), dt_part1) %>%
  filter(dt_part1 %>% apply(., 1, function(x)
    sum(is.na(x))) != 108)
dt96_00 <-
  dt96_00 %>%
  filter(!duplicated(.))

dt96_00_1 <-
  dt96_00 %>%
  filter(is.na(code)) %>% select(name) %>% filter(!duplicated(.)) %>%
  inner_join(., dt96_00) %>%
  arrange(name) %>% View(.)

dt96_00 <-
  within(dt96_00, {
    code[which(name == "城关区")] <- "620102"
    code[which(name == "东区")] <- "510402"
    code[which(name == "西区")] <- "510403"
    code[which(name == "红海湾开发")] <- "440902"
    code[which(grepl("双湖", name))] <- "542431"
  }) %>%
  filter(!is.na(code))


library(readxl)
variable_name_1980_2000 <-
  read_excel("original-data-agriculture/variable_name.xls",
             sheet = "merge_list") %>%
  mutate(new_var = paste("new", seq(1, nrow(.)), sep = ""))



new_dt2 <-
  new_dt01_04 %>%
  rename(
    year = var1,
    name = var2,
    nameen = var2_EN,
    code = var3
  ) %>%
  select(-nameen) %>%
  reshape2::melt(.,
                 id.vars = c("name", "year", "code"),
                 na.rm = TRUE) %>%
  mutate(variable = as.character(variable)) %>%
  left_join(
    .,
    variable_name_1980_2000 %>%
      select(var00, new_var) %>%
      filter(!is.na(var00)) %>%
      rename(variable = var00)
  )  %>%
  select(-variable) #%>%
reshape2::dcast(., name  + code + year ~ new_var)

new_dt1 <-
  
  dt96_00 %>%
  select(-starts_with("relation")) %>%
  reshape2::melt(.,
                 id.vars = c("name", "year", "code"),
                 na.rm = TRUE) %>%
  mutate(variable = gsub("x", "X", as.character(variable))) %>%
  mutate(variable = gsub("a", "A", as.character(variable))) %>%
  mutate(variable = gsub(" ", "", as.character(variable))) %>%
  left_join(
    .,
    variable_name_1980_2000 %>%
      select(var90, new_var) %>%
      filter(!is.na(var90)) %>%
      rename(variable = var90)
  )  %>%
  select(-variable)  #


dt9604 <-
  bind_rows(new_dt1, new_dt2) %>%
  reshape2::dcast(., name  + code + year ~ new_var)  %>%
  arrange(code, year)

