---
title: "Funnel Daily PE"
output:
  html_document:
    toc: yes
    toc_depth: 2
    toc_float: yes
    theme: united
runtime: shiny
---

<style type="text/css">.main-container {max-width: 1500px; margin-left: auto; margin-right: auto;}</style>

```{r setup, include=FALSE}
knitr::opts_chunk$set(fig.height=10, fig.width=10)
```

```{r global, include=FALSE, echo=FALSE}
library(shiny)
library(DT)
library(tidyverse)
library(lubridate)
library(plotly)
options(shiny.maxRequestSize=25*1024^2)
#Read data
funnel <- readRDS('DailyFunnel.R') %>%
          #Date
          filter(date>='2020-11-23') %>%
          #Store name
          mutate(store_name=str_squish(stri_trans_totitle(store_name)))
#layout_(legend=list(orientation="h", x=0.3, y=-0.05))
```

```{r, include=FALSE, echo=FALSE}
#Choices
country_choices <- funnel %>% select(country_code) %>% distinct() %>% filter(nchar(country_code)==2) %>% arrange(country_code)
city_choices <- funnel %>% select(city_code) %>% distinct() %>% filter(nchar(city_code)>0) %>% arrange(city_code)
district_choices <- funnel %>% select(district) %>% distinct() %>% filter(nchar(district)>0) %>% arrange(district)
date_choices <- funnel %>% select(date) %>% distinct() %>% filter(nchar(date)>0) %>% arrange(desc(date))
category_choices <- funnel %>% select(category_name) %>% distinct() %>% filter(nchar(category_name)>0) %>% arrange(category_name)
tag_choices <- funnel %>% select(tag) %>% distinct(tag) %>% filter(nchar(tag)>0) %>% arrange(tag)
big_chain_choices <- funnel %>% select(big_chain) %>% distinct() %>% filter(nchar(big_chain)>0) %>% arrange(big_chain)
lh_choices <- funnel %>% select(local_hero) %>% distinct() %>% filter(nchar(local_hero)>0) %>% arrange(local_hero)
store_choices <- funnel %>% select(store_name) %>% distinct() %>% filter(nchar(store_name)>0) %>% arrange(store_name)
```

Información desde `r as.Date(min(funnel$date))` hasta `r as.Date(max(funnel$date))`

## Info

1. Filtrar por ciudad, distrito (pickup), fecha, categoría y nombre de tienda. Para categorías Mirror ver category_id.
2. Todas las tablas se pueden descargar o copiar (usar botones Copy y CSV).
3. Esta info es agregada por día, para la info por semana o mes consultar con Gerardo.

```{r, echo=FALSE}
inputPanel(
           selectInput("country", label="Country:", choices=country_choices$country_code),
           selectInput("city", label="City:", choices=city_choices$city_code, multiple=TRUE),
           selectInput("district", label="District:", choices=district_choices$district, multiple=TRUE),
           dateRangeInput(inputId="date", label="Date:", start="2020-09-28", end=max(date_choices$date), min=min(date_choices$date), max=max(date_choices$date)),
           selectInput("category", label="Category:", choices=category_choices$category_name, multiple=TRUE),
           selectInput("tag", label="Tag:", choices=tag_choices$tag, multiple=TRUE),
           selectInput("store", label="Store name:", choices=store_choices$store_name, multiple=TRUE),
           selectInput("big_chain", label="Big Chain:", choices=big_chain_choices$big_chain, multiple=FALSE),
           selectInput("local_hero", label="Local Hero:", choices=lh_choices$local_hero, multiple=FALSE)
           )
```

```{r, include=FALSE, echo=FALSE}
#Filters
funnelcoty <- reactive({
               funnel %>%
               filter(country_code == input$country) %>%
               filter(date >= min(input$date) & date <= max(input$date))
})
funnelcity <- reactive({
              if (is.null(input$city)){
               funnel %>%
               filter(country_code == input$country) %>%
               filter(date >= min(input$date) & date <= max(input$date))
              } else {
               funnel %>%
               filter(country_code == input$country) %>%
               filter(city_code == input$city) %>%
               filter(date >= min(input$date) & date <= max(input$date))
              }
})
funnelcate <- reactive({
              if (is.null(input$city) & is.null(input$district) & is.null(input$category)){
               funnel %>%
               filter(country_code == input$country) %>%
               filter(date >= min(input$date) & date <= max(input$date))
              } else 
              if (!is.null(input$city) & is.null(input$district) & is.null(input$category)){
               funnel %>%
               filter(country_code == input$country) %>%
               filter(city_code == input$city) %>%
               filter(date >= min(input$date) & date <= max(input$date))
              } else
              if (is.null(input$city) & !is.null(input$district) & is.null(input$category)){
               funnel %>%
               filter(country_code == input$country) %>%
               filter(district == input$district) %>%
               filter(date >= min(input$date) & date <= max(input$date))
              } else
              if (is.null(input$city) & is.null(input$district) & !is.null(input$category)){
               funnel %>%
               filter(country_code == input$country) %>%
               filter(category_name == input$category) %>%
               filter(date >= min(input$date) & date <= max(input$date))
              } else                
              if (!is.null(input$city) & !is.null(input$district) & is.null(input$category)){
               funnel %>%
               filter(country_code == input$country) %>%
               filter(city_code == input$city) %>%
               filter(district == input$district) %>%
               filter(date >= min(input$date) & date <= max(input$date))
              } else                
              if (!is.null(input$city) & is.null(input$district) & !is.null(input$category)){
               funnel %>%
               filter(country_code == input$country) %>%
               filter(city_code == input$city) %>%
               filter(category_name == input$category) %>%
               filter(date >= min(input$date) & date <= max(input$date))
              } else 
              if (is.null(input$city) & !is.null(input$district) & !is.null(input$category)){
               funnel %>%
               filter(country_code == input$country) %>%
               filter(district == input$district) %>%
               filter(category_name == input$category) %>%
               filter(date >= min(input$date) & date <= max(input$date))
              } else 
              if (!is.null(input$city) & !is.null(input$district) & !is.null(input$category)){
               funnel %>%
               filter(country_code == input$country) %>%
               filter(city_code == input$city) %>%
               filter(district == input$district) %>%
               filter(category_name == input$category) %>%
               filter(date >= min(input$date) & date <= max(input$date))
              }
})
funnelstor <- reactive({
              if (is.null(input$city) & is.null(input$category) & is.null(input$tag) & is.null(input$store)){
               funnel %>%
               filter(country_code == input$country) %>%
               filter(date >= min(input$date) & date <= max(input$date))
              } else 
              if (!is.null(input$city) & is.null(input$category) & is.null(input$tag) & is.null(input$store)){
               funnel %>%
               filter(country_code == input$country) %>%
               filter(city_code == input$city) %>%
               filter(date >= min(input$date) & date <= max(input$date))
              } else
              if (is.null(input$city) & is.null(input$category) & !is.null(input$tag) & is.null(input$store)){
               funnel %>%
               filter(country_code == input$country) %>%
               filter(tag == input$tag) %>%
               filter(date >= min(input$date) & date <= max(input$date))
              } else
              if (is.null(input$city) & !is.null(input$category) & is.null(input$tag) & is.null(input$store)){
               funnel %>%
               filter(country_code == input$country) %>%
               filter(category_name == input$category) %>%
               filter(date >= min(input$date) & date <= max(input$date))
              } else                
              if (is.null(input$city) & is.null(input$category) & is.null(input$tag)  & !is.null(input$store)){
               funnel %>%
               filter(country_code == input$country) %>%
               filter(store_name == input$store) %>%
               filter(date >= min(input$date) & date <= max(input$date))
              } else                
              if (!is.null(input$city) & is.null(input$category) & !is.null(input$tag)  & is.null(input$store)){
               funnel %>%
               filter(country_code == input$country) %>%
               filter(city_code == input$city) %>%
               filter(tag == input$tag) %>%
               filter(date >= min(input$date) & date <= max(input$date))
              } else 
              if (!is.null(input$city) & !is.null(input$category) & is.null(input$tag)  & is.null(input$store)){
               funnel %>%
               filter(country_code == input$country) %>%
               filter(city_code == input$city) %>%
               filter(category_name == input$category) %>%
               filter(date >= min(input$date) & date <= max(input$date))
              } else 
              if (!is.null(input$city) & is.null(input$category) & is.null(input$tag) & !is.null(input$store)){
               funnel %>%
               filter(country_code == input$country) %>%
               filter(city_code == input$city) %>%
               filter(store_name == input$store) %>%
               filter(date >= min(input$date) & date <= max(input$date))
              } else
              if (is.null(input$city) & !is.null(input$category) & !is.null(input$tag) & is.null(input$store)){
               funnel %>%
               filter(country_code == input$country) %>%
               filter(tag == input$tag) %>%
               filter(category_name == input$category) %>%
               filter(date >= min(input$date) & date <= max(input$date))
              } else
              if (is.null(input$city) & is.null(input$category) & !is.null(input$tag) & !is.null(input$store)){
               funnel %>%
               filter(country_code == input$country) %>%
               filter(tag == input$tag) %>%
               filter(store_name == input$store) %>%
               filter(date >= min(input$date) & date <= max(input$date))
              } else
              if (is.null(input$city) & !is.null(input$category) & is.null(input$tag) & !is.null(input$store)){
               funnel %>%
               filter(country_code == input$country) %>%
               filter(category_name == input$category) %>%
               filter(store_name == input$store) %>%
               filter(date >= min(input$date) & date <= max(input$date))
              } else
              if (is.null(input$city) & !is.null(input$category) & !is.null(input$tag) & !is.null(input$store)){
               funnel %>%
               filter(country_code == input$country) %>%
               filter(tag == input$tag) %>%
               filter(category_name == input$category) %>%
               filter(store_name == input$store) %>%
               filter(date >= min(input$date) & date <= max(input$date))
              } else
              if (!is.null(input$city) & !is.null(input$category) & is.null(input$tag) & !is.null(input$store)){
               funnel %>%
               filter(country_code == input$country) %>%
               filter(city_code == input$city) %>%
               filter(category_name == input$category) %>%
               filter(store_name == input$store) %>%
               filter(date >= min(input$date) & date <= max(input$date))
              } else
              if (!is.null(input$city) & is.null(input$category) & !is.null(input$tag) & !is.null(input$store)){
               funnel %>%
               filter(country_code == input$country) %>%
               filter(city_code == input$city) %>%
               filter(tag == input$tag) %>%
               filter(store_name == input$store) %>%
               filter(date >= min(input$date) & date <= max(input$date))
              } else
              if (!is.null(input$city) & !is.null(input$category) & !is.null(input$tag) & is.null(input$store)){
               funnel %>%
               filter(country_code == input$country) %>%
               filter(city_code == input$city) %>%
               filter(tag == input$tag) %>%
               filter(category_name == input$category) %>%
               filter(date >= min(input$date) & date <= max(input$date))
              } else
              if (!is.null(input$city) & !is.null(input$category) & !is.null(input$tag) & !is.null(input$store)){
               funnel %>%
               filter(country_code == input$country) %>%
               filter(city_code == input$city) %>%
               filter(tag == input$tag) %>%
               filter(category_name == input$category) %>%
               filter(store_name == input$store) %>%
               filter(date >= min(input$date) & date <= max(input$date))
              }
})
```

```{r, include=FALSE, echo=FALSE}
#Country
##Summary
coty_uniord <- reactive({
               funnelcoty() %>%
               group_by(country_code) %>%
               summarise(delivered=sum(delivered, na.rm=TRUE)) %>%
               filter(delivered > 0)
})
coty_datord <- reactive({
               funnelcoty() %>%
               group_by(country_code, date) %>%
               summarise(delivered=sum(delivered, na.rm=TRUE)) %>%
               filter(delivered > 0)
})
##Calculations
###Home
####Total
coty_hometl <- reactive({
               funnelcoty() %>%
               distinct(country_code, home_views, home_customers) %>%
               group_by(country_code) %>%
               summarise(across(home_views:home_customers,~sum(.x, na.rm=TRUE))) %>%
               filter(home_views > 0)  %>%
               left_join(coty_uniord() %>% select(country_code, delivered), by=c("country_code"="country_code")) %>%
               mutate(CR=delivered/home_views) %>%
               arrange(desc(home_customers))
})
####Date
coty_homeda <- reactive({
               funnelcoty() %>%
               distinct(country_code, date, home_views, home_customers) %>%
               group_by(country_code, date) %>%
               summarise(across(home_views:home_customers,~sum(.x, na.rm=TRUE))) %>%
               filter(home_views > 0) %>%
               left_join(coty_datord() %>% select(country_code, date, delivered), by=c("country_code"="country_code", "date"="date")) %>%
               mutate(CR=delivered/home_views) %>%
               arrange(desc(date))
})
###Category
####Total
coty_catetl <- reactive({
               funnelcoty() %>%
               distinct(country_code, category_views, category_customers)  %>%
               group_by(country_code) %>%
               summarise(across(category_views:category_customers,~sum(.x, na.rm=TRUE))) %>%
               filter(category_views > 0) %>%
               left_join(coty_uniord() %>% select(country_code, delivered), by=c("country_code"="country_code")) %>%
               mutate(CR=delivered/category_views) %>%
               arrange(desc(category_customers))
})
####Date
coty_cateda <- reactive({
               funnelcoty() %>%
               distinct(country_code, date, category_views, category_customers)  %>%
               group_by(country_code, date) %>%
               summarise(across(category_views:category_customers,~sum(.x, na.rm=TRUE))) %>%
               filter(category_views > 0)  %>%
               left_join(coty_datord() %>% select(country_code, date, delivered), by=c("country_code"="country_code", "date"="date")) %>%
               mutate(CR=delivered/category_views) %>%
               arrange(desc(date))
})
###Store
####Total
coty_stortl <- reactive({
               funnelcoty() %>%
               distinct(country_code, store_views, store_customers)  %>%
               group_by(country_code) %>%
               summarise(across(store_views:store_customers,~sum(.x, na.rm=TRUE))) %>%
               filter(store_views > 0) %>%
               left_join(coty_uniord() %>% select(country_code, delivered), by=c("country_code"="country_code")) %>%
               mutate(CR=delivered/store_views) %>%
               arrange(desc(store_customers))
})
coty_storda <- reactive({
               funnelcoty() %>% #funnel %>%
               distinct(country_code, date, store_views, store_customers)  %>%
               group_by(country_code, date) %>%
               summarise(across(store_views:store_customers,~sum(.x, na.rm=TRUE))) %>%
               filter(store_views > 0)  %>%
               left_join(coty_datord() %>% select(country_code, date, delivered), by=c("country_code"="country_code", "date"="date")) %>%
               mutate(CR=delivered/store_views) %>%
               arrange(desc(date))
})
###Add Cart - Order
####Total
coty_adortl <- reactive({
               funnelcoty() %>%
               group_by(country_code) %>%
               summarise(across(cart_adds:cancels_customers,~sum(as.numeric(.x), na.rm=TRUE))) %>%
               mutate(order_aov=order_value/delivered) %>%
               filter(cart_adds > 0)  %>%
               arrange(desc(cart_customers))
})
####Date
coty_adorda <- reactive({
               funnelcoty() %>%
               group_by(country_code, date) %>%
               summarise(across(cart_adds:cancels_customers,~sum(as.numeric(.x), na.rm=TRUE))) %>%
               mutate(order_aov=order_value/delivered) %>%
               filter(cart_adds > 0)  %>%
               arrange(desc(date))
})
```

```{r, include=FALSE, echo=FALSE}
#City
##Summary
city_uniord <- reactive({
               funnelcity() %>%
               group_by(city_code) %>%
               summarise(delivered=sum(delivered, na.rm=TRUE)) %>%
               filter(delivered > 0)
})
city_datord <- reactive({
               funnelcity() %>%
               group_by(city_code, date) %>%
               summarise(delivered=sum(delivered, na.rm=TRUE)) %>%
               filter(delivered > 0)
})
##Calculations
###Home
####Total
city_hometl <- reactive({
               funnelcity() %>%
               distinct(city_code, home_views, home_customers) %>%
               group_by(city_code) %>%
               summarise(across(home_views:home_customers,~sum(.x, na.rm=TRUE))) %>%
               filter(home_views > 0)  %>%
               left_join(city_uniord() %>% select(city_code, delivered), by=c("city_code"="city_code")) %>%
               mutate(CR=delivered/home_views) %>%
               arrange(desc(home_customers))
})
####Date
city_homeda <- reactive({
               funnelcity() %>%
               distinct(city_code, date, home_views, home_customers) %>%
               group_by(city_code, date) %>%
               summarise(across(home_views:home_customers,~sum(.x, na.rm=TRUE))) %>%
               filter(home_views > 0)  %>%
               left_join(city_datord() %>% select(city_code, date, delivered), by=c("city_code"="city_code", "date"="date")) %>%
               mutate(CR=delivered/home_views) %>%
               arrange(desc(date))
})
###Category
####Total
city_catetl <- reactive({
               funnelcity() %>%
               distinct(city_code, category_views, category_customers)  %>%
               group_by(city_code) %>%
               summarise(across(category_views:category_customers,~sum(.x, na.rm=TRUE))) %>%
               filter(category_views > 0)  %>%
               left_join(city_uniord() %>% select(city_code, delivered), by=c("city_code"="city_code")) %>%
               mutate(CR=delivered/category_views) %>%
               arrange(desc(category_customers))
})
####Date
city_cateda <- reactive({
               funnelcity() %>%
               distinct(city_code, date, category_views, category_customers)  %>%
               group_by(city_code, date) %>%
               summarise(across(category_views:category_customers,~sum(.x, na.rm=TRUE))) %>%
               filter(category_views > 0)  %>%
               left_join(city_datord() %>% select(city_code, date, delivered), by=c("city_code"="city_code", "date"="date")) %>%
               mutate(CR=delivered/category_views) %>%
               arrange(desc(date))
})
###Store
####Total
city_stortl <- reactive({
               funnelcity() %>%
               distinct(city_code, store_views, store_customers)  %>%
               group_by(city_code) %>%
               summarise(across(store_views:store_customers,~sum(.x, na.rm=TRUE))) %>%
               filter(store_views > 0)  %>%
               left_join(city_uniord() %>% select(city_code, delivered), by=c("city_code"="city_code")) %>%
               mutate(CR=delivered/store_views) %>%
               arrange(desc(store_customers))
})
city_storda <- reactive({
               funnelcity() %>%
               distinct(city_code, date, store_views, store_customers)  %>%
               group_by(city_code, date) %>%
               summarise(across(store_views:store_customers,~sum(.x, na.rm=TRUE))) %>%
               filter(store_views > 0)  %>%
               left_join(city_datord() %>% select(city_code, date, delivered), by=c("city_code"="city_code", "date"="date")) %>%
               mutate(CR=delivered/store_views) %>%
               arrange(desc(date))
})
###Add Cart - Order
####Total
city_adortl <- reactive({
               funnelcity() %>%
               group_by(city_code) %>%
               summarise(across(cart_adds:cancels_customers,~sum(as.numeric(.x), na.rm=TRUE))) %>%
                mutate(order_aov=order_value/delivered) %>%
                filter(cart_adds > 0)  %>%
                arrange(desc(cart_customers))
})
####Date
city_adorda <- reactive({
               funnelcity() %>%
               group_by(city_code, date) %>%
               summarise(across(cart_adds:cancels_customers,~sum(as.numeric(.x), na.rm=TRUE))) %>%
                mutate(order_aov=order_value/delivered) %>%
                filter(cart_adds > 0)  %>%
                arrange(desc(date))
})
```

```{r, include=FALSE, echo=FALSE}
#Category
##Summary
cate_uniord <- reactive({
               funnelcate() %>%
               group_by(category_name) %>%
               summarise(delivered=sum(delivered, na.rm=TRUE)
                         ) %>%
               filter(delivered > 0)
})
cate_datord <- reactive({
               funnelcate() %>%
               group_by(category_name, date) %>%
               summarise(delivered=sum(delivered, na.rm=TRUE)) %>%
               filter(delivered > 0)
})
##Calculations
###Category
####Total
cate_catetl <- reactive({
               funnelcate() %>%
               distinct(category_name, category_views, category_customers)  %>%
               group_by(category_name) %>%
               summarise(across(category_views:category_customers,~sum(.x, na.rm=TRUE))) %>%
               filter(category_views > 0) %>% 
               left_join(cate_uniord() %>% select(category_name, delivered), by=c("category_name"="category_name")) %>%
               mutate(CR=delivered/category_views) %>%
               arrange(desc(category_customers))
})
####Date
cate_cateda <- reactive({
               funnelcate() %>%
               distinct(category_name, date, category_views, category_customers)  %>%
               group_by(category_name, date) %>%
               summarise(across(category_views:category_customers,~sum(.x, na.rm=TRUE))) %>%
               filter(category_views > 0) %>%
               left_join(cate_datord() %>% select(category_name, date, delivered), by=c("category_name"="category_name", "date"="date")) %>%
               mutate(CR=delivered/category_views) %>%
               arrange(desc(date))
})
###Store
####Total
cate_stortl <- reactive({
               funnelcate() %>%
               distinct(category_name, store_views, store_customers)  %>%
               group_by(category_name) %>%
               summarise(across(store_views:store_customers,~sum(.x, na.rm=TRUE))) %>%
               filter(store_views > 0) %>%
               left_join(cate_uniord() %>% select(category_name, delivered), by=c("category_name"="category_name")) %>%
               mutate(CR=delivered/store_views) %>%
               arrange(desc(store_customers))
})
cate_storda <- reactive({
               funnelcate() %>%
               distinct(category_name, date, store_views, store_customers)  %>%
               group_by(category_name, date) %>%
               summarise(across(store_views:store_customers,~sum(.x, na.rm=TRUE))) %>%
               filter(store_views > 0) %>%
               left_join(cate_datord() %>% select(category_id, date, delivered), by=c("category_name"="category_name", "date"="date")) %>%
               mutate(CR=delivered/store_views) %>%
               arrange(desc(date))
})
###Add Cart - Order
####Total
cate_adortl <- reactive({
               funnelcate() %>%
               group_by(category_name) %>%
               summarise(across(cart_adds:cancels_customers,~sum(as.numeric(.x), na.rm=TRUE))) %>%
               mutate(order_aov=order_value/delivered) %>%
               filter(cart_adds > 0) %>%
               arrange(desc(cart_customers))
})
####Date
cate_adorda <- reactive({
               funnelcate() %>%
               group_by(category_name, date) %>%
               summarise(across(cart_adds:cancels_customers,~sum(as.numeric(.x), na.rm=TRUE))) %>%
               mutate(order_aov=order_value/delivered) %>%
               filter(cart_adds > 0) %>%
               arrange(desc(date))
})
```

```{r, include=FALSE, echo=FALSE}
#Store
##Summary
stor_uniord <- reactive({
               funnelstor() %>%
               group_by(store_name) %>%
               summarise(delivered=sum(delivered, na.rm=TRUE)) %>%
               filter(nchar(store_name) > 0)
})
stor_datord <- reactive({
               funnelstor() %>%
               group_by(store_name, date) %>%
               summarise(delivered=sum(delivered, na.rm=TRUE)) %>%
               filter(nchar(store_name) > 0)
})
##Calculations
###Store
####Total
stor_stortl <- reactive({
               funnelstor() %>%
               distinct(store_name, store_views, store_customers)  %>%
               group_by(store_name) %>%
               summarise(across(store_views:store_customers,~sum(.x, na.rm=TRUE))) %>%
               filter(store_views > 0) %>%
               left_join(stor_uniord() %>% select(store_name, delivered), by=c("store_name"="store_name")) %>%
               mutate(CR=delivered/store_views) %>%
               arrange(desc(store_customers))
})
stor_storda <- reactive({
               funnelstor() %>%
               distinct(store_name, date, store_views, store_customers)  %>%
               group_by(store_name, date) %>%
               summarise(across(store_views:store_customers,~sum(.x, na.rm=TRUE))) %>%
               filter(store_views > 0) %>%
               left_join(stor_datord() %>% select(store_name, date, delivered), by=c("store_name"="store_name", "date"="date")) %>%
               mutate(CR=delivered/store_views) %>%
               arrange(desc(date))
})
###Add Cart - Order
####Total
stor_adortl <- reactive({
               funnelstor() %>%
               group_by(store_name) %>%
               summarise(across(cart_adds:cancels_customers,~sum(as.numeric(.x), na.rm=TRUE))) %>%
               mutate(order_aov=order_value/delivered) %>%
               filter(cart_adds > 0)  %>%
               arrange(desc(cart_customers))
})
####Date
stor_adorda <- reactive({
               funnelstor() %>%
               group_by(store_name, date) %>%
               summarise(across(cart_adds:cancels_customers,~sum(as.numeric(.x), na.rm=TRUE))) %>%
               mutate(order_aov=order_value/delivered) %>%
               filter(cart_adds > 0)  %>%
               arrange(desc(date))
})
```

```{r, include=FALSE, echo=FALSE}
#Store Address
##Summary
stad_uniord <- reactive({
               funnelstor() %>%
               group_by(store_address_id) %>%
               summarise(delivered=sum(delivered, na.rm=TRUE)
                         ) %>%
               filter(delivered > 0)  %>%
               arrange(desc(delivered))
})
stad_datord <- reactive({
               funnelstor() %>%
               group_by(store_address_id, date) %>%
               summarise(delivered=sum(delivered, na.rm=TRUE)
                         ) %>%
               filter(delivered > 0)  %>%
               arrange(desc(date))
})
##Calculations
###Store
####Total
stad_stortl <- reactive({
               funnelstor() %>%
               distinct(store_address_id, store_name, store_views, store_customers)  %>%
               group_by(store_address_id, store_name) %>%
               summarise(across(store_views:store_customers,~sum(.x, na.rm=TRUE))) %>%
               filter(store_views > 0) %>%
               left_join(stad_uniord() %>% select(store_address_id, delivered), by=c("store_address_id"="store_address_id")) %>%
               mutate(CR=delivered/store_views) %>%
               arrange(desc(store_customers))
})
stad_storda <- reactive({
               funnelstor() %>%
               distinct(store_address_id, store_name, date, store_views, store_customers)  %>%
               group_by(store_address_id, store_name, date) %>%
               summarise(across(store_views:store_customers,~sum(.x, na.rm=TRUE))) %>%
               filter(store_views > 0) %>%
               left_join(stad_datord() %>% select(store_address_id, date, delivered), by=c("store_address_id"="store_address_id", "date"="date")) %>%
               mutate(CR=delivered/store_views) %>%
               arrange(desc(date))
})
###Add Cart - Order
####Total
stad_adortl <- reactive({
               funnelstor() %>%
               group_by(store_address_id, store_name) %>%
               summarise(across(cart_adds:cancels_customers,~sum(as.numeric(.x), na.rm=TRUE))) %>%
               mutate(order_aov=order_value/delivered) %>%
               filter(cart_adds > 0) %>%
               arrange(desc(cart_customers))
})
####Date
stad_adorda <- reactive({
               funnelstor() %>%
               group_by(store_address_id, store_name, date) %>%
               summarise(across(cart_adds:cancels_customers,~sum(as.numeric(.x), na.rm=TRUE))) %>%
               mutate(order_aov=order_value/delivered) %>%
               filter(cart_adds > 0) %>%
               arrange(desc(date))
})
```

```{r, include=FALSE, echo=FALSE}
##Theme 
themeplot <- theme(plot.title=element_text(color="black", size=12, face="bold", hjust=0.5),
                   plot.subtitle=element_text(color="black", size=10, face="bold", hjust=0.5),
                   panel.background=element_rect(fill="white", color="black"),
                   axis.title.x=element_blank(),
                   axis.title.y=element_blank(),
                   axis.text.x=element_text(color="black", size=8),
                   axis.text.y=element_text(color="black", size=8),
                   strip.text.x=element_text(color="black", size=8),
                   legend.title=element_blank(),
                   legend.position="bottom"
                   )
```

```{r, include=FALSE, echo=FALSE}
#Country
##Plots
###Home
coty_homegg <- reactive({
               coty_homeda() %>%
               pivot_longer(cols=c(home_views, home_customers), names_to="kpi") %>%
               ggplot(aes(x=date, y=value, col=kpi, group=kpi)) +
               geom_line(show.legend=TRUE) +
               facet_wrap(~country_code, scales="free") +
               themeplot
})
###Category
coty_categg <- reactive({
               coty_cateda() %>%
               pivot_longer(cols=c(category_views, category_customers), names_to="kpi") %>%
               ggplot(aes(x=date, y=value, col=kpi, group=kpi)) +
               geom_line(show.legend=TRUE) +
               facet_wrap(~country_code, scales="free") +
               themeplot
})
###Store
coty_storgg <- reactive({
               coty_storda() %>%
               pivot_longer(cols=c(store_views, store_customers), names_to="kpi") %>%
               ggplot(aes(x=date, y=value, col=kpi, group=kpi)) +
               geom_line(show.legend=TRUE) +
               facet_wrap(~country_code, scales="free") +
               themeplot
})
###Add Cart - Drop Carts
coty_addrgg <- reactive({
               coty_adorda() %>%
               pivot_longer(cols=c(cart_adds, cart_customers, cart_drops, cart_drops_customers), names_to="kpi") %>%
               ggplot(aes(x=date, y=value, col=kpi, group=kpi)) +
               geom_line(show.legend=TRUE) +
               facet_wrap(~country_code, scales="free") +
               themeplot
})
###Checkouts - Orders
coty_chorgg <- reactive({
               coty_adorda() %>%
               pivot_longer(cols=c(checkouts, checkouts_customers, orders, orders_customers), names_to="kpi") %>%
               ggplot(aes(x=date, y=value, col=kpi, group=kpi)) +
               geom_line(show.legend=TRUE) +
               facet_wrap(~country_code, scales="free") +
               themeplot
})
###Orders
coty_orcagg <- reactive({
               coty_adorda() %>%
               pivot_longer(cols=c(delivered, AU, cancels, cancels_customers), names_to="kpi") %>%
               ggplot(aes(x=date, y=value, col=kpi, group=kpi)) +
               geom_line(show.legend=TRUE) +
               facet_wrap(~country_code, scales="free") +
               themeplot
})
###Values
coty_valugg <- reactive({
               coty_adorda() %>%
               pivot_longer(cols=c(cart_value, cart_drops_value, order_value), names_to="kpi") %>%
               ggplot(aes(x=date, y=value, col=kpi, group=kpi)) +
               geom_line(show.legend=TRUE) +
               facet_wrap(~country_code, scales="free") +
               themeplot
})
###Products
coty_prodgg <- reactive({
               coty_adorda() %>%
               pivot_longer(cols=c(cart_products, cart_drops_products), names_to="kpi") %>%
               ggplot(aes(x=date, y=value, col=kpi, group=kpi)) +
               geom_line(show.legend=TRUE) +
               facet_wrap(~country_code, scales="free") +
               themeplot
})
```

```{r, include=FALSE, echo=FALSE}
#City
##Plots
###Home
city_homegg <- reactive({
               city_homeda() %>%
               pivot_longer(cols=c(home_views, home_customers), names_to="kpi") %>%
               filter(n()>2) %>%
               ggplot(aes(x=date, y=value, col=kpi, group=kpi)) +
               geom_line(show.legend=TRUE) +
               facet_wrap(~city_code, scales="free") +
               themeplot
})
###Category
city_categg <- reactive({
               city_cateda() %>%
               pivot_longer(cols=c(category_views, category_customers), names_to="kpi") %>%
               filter(n()>2) %>%
               ggplot(aes(x=date, y=value, col=kpi, group=kpi)) +
               geom_line(show.legend=TRUE) +
               facet_wrap(~city_code, scales="free") +
               themeplot
})
###Store
city_storgg <- reactive({
               city_storda() %>%
               pivot_longer(cols=c(store_views, store_customers), names_to="kpi") %>%
               filter(n()>2) %>%
               ggplot(aes(x=date, y=value, col=kpi, group=kpi)) +
               geom_line(show.legend=TRUE) +
               facet_wrap(~city_code, scales="free") +
               themeplot
})
###Add Cart - Drop Carts
city_addrgg <- reactive({
               city_adorda() %>%
               pivot_longer(cols=c(cart_adds, cart_customers, cart_drops, cart_drops_customers), names_to="kpi") %>%
               filter(n()>4) %>%
               ggplot(aes(x=date, y=value, col=kpi, group=kpi)) +
               geom_line(show.legend=TRUE) +
               facet_wrap(~city_code, scales="free") +
               themeplot
})
###Checkouts - Orders
city_chorgg <- reactive({
               city_adorda() %>%
               pivot_longer(cols=c(checkouts, checkouts_customers, orders, orders_customers), names_to="kpi") %>%
               filter(n()>4) %>%
               ggplot(aes(x=date, y=value, col=kpi, group=kpi)) +
               geom_line(show.legend=TRUE) +
               facet_wrap(~city_code, scales="free") +
               themeplot
})
###Orders
city_orcagg <- reactive({
               city_adorda() %>%
               pivot_longer(cols=c(delivered, AU, cancels, cancels_customers), names_to="kpi") %>%
               filter(n()>4) %>%
               ggplot(aes(x=date, y=value, col=kpi, group=kpi)) +
               geom_line(show.legend=TRUE) +
               facet_wrap(~city_code, scales="free") +
               themeplot
})
###Values
city_valugg <- reactive({
               city_adorda() %>%
               pivot_longer(cols=c(cart_value, cart_drops_value, order_value), names_to="kpi") %>%
               filter(n()>3) %>%
               ggplot(aes(x=date, y=value, col=kpi, group=kpi)) +
               geom_line(show.legend=TRUE) +
               facet_wrap(~city_code, scales="free") +
               themeplot
})
###Products
city_prodgg <- reactive({
               city_adorda() %>%
               pivot_longer(cols=c(cart_products, cart_drops_products), names_to="kpi") %>%
               filter(n()>2) %>%
               ggplot(aes(x=date, y=value, col=kpi, group=kpi)) +
               geom_line(show.legend=TRUE) +
               facet_wrap(~city_code, scales="free") +
               themeplot
})
```

```{r, include=FALSE, echo=FALSE}
#Category
##Plots
###Category
cate_categg <- reactive({
               cate_cateda() %>%
               pivot_longer(cols=c(category_views, category_customers), names_to="kpi") %>%
               filter(n()>2) %>%
               inner_join(cate_catetl() %>% ungroup() %>% top_n(12, delivered), by=c("category_name"="category_name")) %>%
               ggplot(aes(x=date, y=value, col=kpi, group=kpi)) +
               geom_line(show.legend=TRUE) +
               facet_wrap(~category_name, scales="free") +
               themeplot
})
###Store
cate_storgg <- reactive({
               cate_storda() %>%
               pivot_longer(cols=c(store_views, store_customers), names_to="kpi") %>%
               filter(n()>2) %>%
               inner_join(cate_catetl() %>% ungroup() %>% top_n(12, delivered), by=c("category_name"="category_name")) %>%
               ggplot(aes(x=date, y=value, col=kpi, group=kpi)) +
               geom_line(show.legend=TRUE) +
               facet_wrap(~category_name, scales="free") +
               themeplot
})
###Add Cart - Drop Carts
cate_addrgg <- reactive({
               cate_adorda() %>%
               pivot_longer(cols=c(cart_adds, cart_customers, cart_drops, cart_drops_customers), names_to="kpi") %>%
               filter(n()>4) %>%
               inner_join(cate_catetl() %>% ungroup() %>% top_n(12, delivered), by=c("category_name"="category_name")) %>%
               ggplot(aes(x=date, y=value, col=kpi, group=kpi)) +
               geom_line(show.legend=TRUE) +
               facet_wrap(~category_name, scales="free") +
               themeplot
})
###Checkouts - Orders
cate_chorgg <- reactive({
               cate_adorda() %>%
               pivot_longer(cols=c(checkouts, checkouts_customers, orders, orders_customers), names_to="kpi") %>%
               filter(n()>4) %>%
               inner_join(cate_catetl() %>% ungroup() %>% top_n(12, delivered), by=c("category_name"="category_name")) %>%
               ggplot(aes(x=date, y=value, col=kpi, group=kpi)) +
               geom_line(show.legend=TRUE) +
               facet_wrap(~category_name, scales="free") +
               themeplot
})
###Orders
cate_orcagg <- reactive({
               cate_adorda() %>%
               pivot_longer(cols=c(delivered, AU, cancels, cancels_customers), names_to="kpi") %>%
               filter(n()>4) %>%
               inner_join(cate_catetl() %>% ungroup() %>% top_n(12, delivered), by=c("category_name"="category_name")) %>%
               ggplot(aes(x=date, y=value, col=kpi, group=kpi)) +
               geom_line(show.legend=TRUE) +
               facet_wrap(~category_name, scales="free") +
               themeplot
})
###Values
cate_valugg <- reactive({
               cate_adorda() %>%
               pivot_longer(cols=c(cart_value, cart_drops_value, order_value), names_to="kpi") %>%
               filter(n()>3) %>%
               inner_join(cate_catetl() %>% ungroup() %>% top_n(12, delivered), by=c("category_name"="category_name")) %>%
               ggplot(aes(x=date, y=value, col=kpi, group=kpi)) +
               geom_line(show.legend=TRUE) +
               facet_wrap(~category_name, scales="free") +
               themeplot
})
###Products
cate_prodgg <- reactive({
               cate_adorda() %>%
               pivot_longer(cols=c(cart_products, cart_drops_products), names_to="kpi") %>%
               filter(n()>2) %>%
               inner_join(cate_catetl() %>% ungroup() %>% top_n(12, delivered), by=c("category_name"="category_name")) %>%
               ggplot(aes(x=date, y=value, col=kpi, group=kpi)) +
               geom_line(show.legend=TRUE) +
               facet_wrap(~category_name, scales="free") +
               themeplot
})
```

```{r, include=FALSE, echo=FALSE}
#Store
##Plots
stor_storgg <- reactive({
               stor_storda() %>%
               pivot_longer(cols=c(store_views, store_customers), names_to="kpi") %>%
               filter(n()>2) %>%
               inner_join(stor_stortl() %>% ungroup() %>% top_n(12, delivered), by=c("store_name"="store_name")) %>%
               ggplot(aes(x=date, y=value, col=kpi, group=kpi)) +
               geom_line(show.legend=TRUE) +
               facet_wrap(~store_name, scales="free") +
               themeplot
})
###Add Cart - Drop Carts
stor_addrgg <- reactive({
               stor_adorda() %>%
               pivot_longer(cols=c(cart_adds, cart_customers, cart_drops, cart_drops_customers), names_to="kpi") %>%
               filter(n()>4) %>%
               inner_join(stor_stortl() %>% ungroup() %>% top_n(12, delivered), by=c("store_name"="store_name")) %>%
               ggplot(aes(x=date, y=value, col=kpi, group=kpi)) +
               geom_line(show.legend=TRUE) +
               facet_wrap(~store_name, scales="free") +
               themeplot
})
###Checkouts - Orders
stor_chorgg <- reactive({
               stor_adorda() %>%
               pivot_longer(cols=c(checkouts, checkouts_customers, orders, orders_customers), names_to="kpi") %>%
               filter(n()>4) %>%
               inner_join(stor_stortl() %>% ungroup() %>% top_n(12, delivered), by=c("store_name"="store_name")) %>%
               ggplot(aes(x=date, y=value, col=kpi, group=kpi)) +
               geom_line(show.legend=TRUE) +
               facet_wrap(~store_name, scales="free") +
               themeplot
})
###Orders
stor_orcagg <- reactive({
               stor_adorda() %>%
               pivot_longer(cols=c(delivered, AU, cancels, cancels_customers), names_to="kpi") %>%
               filter(n()>4) %>%
               inner_join(stor_stortl() %>% ungroup() %>% top_n(12, delivered), by=c("store_name"="store_name")) %>%
               ggplot(aes(x=date, y=value, col=kpi, group=kpi)) +
               geom_line(show.legend=TRUE) +
               facet_wrap(~store_name, scales="free") +
               themeplot
})
###Values
stor_valugg <- reactive({
               stor_adorda() %>%
               pivot_longer(cols=c(cart_value, cart_drops_value, order_value), names_to="kpi") %>%
               filter(n()>3) %>%
               inner_join(stor_stortl() %>% ungroup() %>% top_n(12, delivered), by=c("store_name"="store_name")) %>%
               ggplot(aes(x=date, y=value, col=kpi, group=kpi)) +
               geom_line(show.legend=TRUE) +
               facet_wrap(~store_name, scales="free") +
               themeplot
})
###Products
stor_prodgg <- reactive({
               stor_adorda() %>%
               pivot_longer(cols=c(cart_products, cart_drops_products), names_to="kpi") %>%
               filter(n()>2) %>%
               inner_join(stor_stortl() %>% ungroup() %>% top_n(12, delivered), by=c("store_name"="store_name")) %>%
               ggplot(aes(x=date, y=value, col=kpi, group=kpi)) +
               geom_line(show.legend=TRUE) +
               facet_wrap(~store_name, scales="free") +
               themeplot
})
```

```{r, include=FALSE, echo=FALSE}
#Store Address
##Plots
###Store
stad_storgg <- reactive({
               stad_storda() %>%
               pivot_longer(cols=c(store_views, store_customers), names_to="kpi") %>%
               filter(n()>2) %>%
               inner_join(stad_stortl() %>% ungroup() %>% top_n(12, delivered), by=c("store_address_id"="store_address_id")) %>%
               ggplot(aes(x=date, y=value, col=kpi, group=kpi)) +
               geom_line(show.legend=TRUE) +
               facet_wrap(~store_address_id, scales="free")  +
               themeplot
})
###Add Cart - Drop Carts
stad_addrgg <- reactive({
               stad_adorda() %>%
               pivot_longer(cols=c(cart_adds, cart_customers, cart_drops, cart_drops_customers), names_to="kpi") %>%
               filter(n()>4) %>%
               inner_join(stad_stortl() %>% ungroup() %>% top_n(12, delivered), by=c("store_address_id"="store_address_id")) %>%
               ggplot(aes(x=date, y=value, col=kpi, group=kpi)) +
               geom_line(show.legend=TRUE) +
               facet_wrap(~store_address_id, scales="free")  +
               themeplot
})
###Checkouts - Orders
stad_chorgg <- reactive({
               stad_adorda() %>%
               pivot_longer(cols=c(checkouts, checkouts_customers, orders, orders_customers), names_to="kpi") %>%
               filter(n()>4) %>%
               inner_join(stad_stortl() %>% ungroup() %>% top_n(12, delivered), by=c("store_address_id"="store_address_id")) %>%
               ggplot(aes(x=date, y=value, col=kpi, group=kpi)) +
               geom_line(show.legend=TRUE) +
               facet_wrap(~store_address_id, scales="free")  +
               themeplot
})
###Orders
stad_orcagg <- reactive({
               stad_adorda() %>%
               pivot_longer(cols=c(delivered, AU, cancels, cancels_customers), names_to="kpi") %>%
               filter(n()>4) %>%
               inner_join(stad_stortl() %>% ungroup() %>% top_n(12, delivered), by=c("store_address_id"="store_address_id")) %>%
               ggplot(aes(x=date, y=value, col=kpi, group=kpi)) +
               geom_line(show.legend=TRUE) +
               facet_wrap(~store_address_id, scales="free") +
               themeplot
})
###Values
stad_valugg <- reactive({
               stad_adorda() %>%
               pivot_longer(cols=c(cart_value, cart_drops_value, order_value), names_to="kpi") %>%
               filter(n()>3) %>%
               inner_join(stad_stortl() %>% ungroup() %>% top_n(12, delivered), by=c("store_address_id"="store_address_id")) %>%
               ggplot(aes(x=date, y=value, col=kpi, group=kpi)) +
               geom_line(show.legend=TRUE) +
               facet_wrap(~store_address_id, scales="free")  +
               themeplot
})
###Products
stad_prodgg <- reactive({
               stad_adorda() %>%
               pivot_longer(cols=c(cart_products, cart_drops_products), names_to="kpi") %>%
               filter(n()>2) %>%
               inner_join(stad_stortl() %>% ungroup() %>% top_n(12, delivered), by=c("store_address_id"="store_address_id")) %>%
               ggplot(aes(x=date, y=value, col=kpi, group=kpi)) +
               geom_line(show.legend=TRUE) +
               facet_wrap(~store_address_id, scales="free")  +
               themeplot
})
```

## Country

### Info {.tabset}

#### Home

```{r, message=FALSE, echo=FALSE}
renderDataTable({datatable(coty_hometl(), rownames=FALSE, class="cell-border stripe", extensions='Buttons', options=list(dom='Bfrtip', buttons=c('copy', 'csv'), initComplete=JS("function(settings, json) {","$('body').css({'font-family': 'Calibri'});","}"))) %>% formatPercentage("CR",2)})
```

#### Category

```{r, message=FALSE, echo=FALSE}
renderDataTable({datatable(coty_catetl(), rownames=FALSE, class="cell-border stripe", extensions='Buttons', options=list(dom='Bfrtip', buttons=c('copy', 'csv'), initComplete=JS("function(settings, json) {","$('body').css({'font-family': 'Calibri'});","}"))) %>% formatPercentage("CR",2)})
```

#### Store

```{r, message=FALSE, echo=FALSE}
renderDataTable({datatable(coty_stortl(), rownames=FALSE, class="cell-border stripe", extensions='Buttons', options=list(dom='Bfrtip', buttons=c('copy', 'csv'), initComplete=JS("function(settings, json) {","$('body').css({'font-family': 'Calibri'});","}"))) %>% formatPercentage("CR",2)})
```

#### Add Cart - Drop Carts

```{r, message=FALSE, echo=FALSE}
renderDataTable({datatable(coty_adortl() %>% group_by(country_code) %>% select(cart_adds,cart_customers,cart_drops,cart_drops_customers), rownames=FALSE, class="cell-border stripe", extensions='Buttons', options=list(dom='Bfrtip', buttons=c('copy', 'csv'), initComplete=JS("function(settings, json) {","$('body').css({'font-family': 'Calibri'});","}")))})
```

#### Checkout - Orders

```{r, message=FALSE, echo=FALSE}
renderDataTable({datatable(coty_adortl() %>% group_by(country_code) %>% select(checkouts,checkouts_customers,orders,orders_customers), rownames=FALSE, class="cell-border stripe", extensions='Buttons', options=list(dom='Bfrtip', buttons=c('copy', 'csv'), initComplete=JS("function(settings, json) {","$('body').css({'font-family': 'Calibri'});","}")))})
```

#### Orders

```{r, message=FALSE, echo=FALSE}
renderDataTable({datatable(coty_adortl() %>% group_by(country_code) %>% select(delivered,AU,cancels,cancels_customers), rownames=FALSE, class="cell-border stripe", extensions='Buttons', options=list(dom='Bfrtip', buttons=c('copy', 'csv'), initComplete=JS("function(settings, json) {","$('body').css({'font-family': 'Calibri'});","}")))})
```

#### Values

```{r, message=FALSE, echo=FALSE}
renderDataTable({datatable(coty_adortl() %>% group_by(country_code) %>% select(cart_value,cart_drops_value,order_value), rownames=FALSE, class="cell-border stripe", extensions='Buttons', options=list(dom='Bfrtip', buttons=c('copy', 'csv'), initComplete=JS("function(settings, json) {","$('body').css({'font-family': 'Calibri'});","}")))})
```

#### Products

```{r, message=FALSE, echo=FALSE}
renderDataTable({datatable(coty_adortl() %>% group_by(country_code) %>% select(cart_products,cart_drops_products), rownames=FALSE, class="cell-border stripe", extensions='Buttons', options=list(dom='Bfrtip', buttons=c('copy', 'csv'), initComplete=JS("function(settings, json) {","$('body').css({'font-family': 'Calibri'});","}")))})
```

### Tables {.tabset}

#### Home

```{r, message=FALSE, echo=FALSE}
renderDataTable({datatable(coty_homeda(), rownames=FALSE, class="cell-border stripe", extensions='Buttons', options=list(dom='Bfrtip', buttons=c('copy', 'csv'), initComplete=JS("function(settings, json) {","$('body').css({'font-family': 'Calibri'});","}"))) %>% formatPercentage("CR",2)})
```

#### Category

```{r, message=FALSE, echo=FALSE}
renderDataTable({datatable(coty_cateda(), rownames=FALSE, class="cell-border stripe", extensions='Buttons', options=list(dom='Bfrtip', buttons=c('copy', 'csv'), initComplete=JS("function(settings, json) {","$('body').css({'font-family': 'Calibri'});","}"))) %>% formatPercentage("CR",2)})
```

#### Store

```{r, message=FALSE, echo=FALSE}
renderDataTable({datatable(coty_storda(), rownames=FALSE, class="cell-border stripe", extensions='Buttons', options=list(dom='Bfrtip', buttons=c('copy', 'csv'), initComplete=JS("function(settings, json) {","$('body').css({'font-family': 'Calibri'});","}"))) %>% formatPercentage("CR",2)})
```

#### Add Cart - Drop Carts

```{r, message=FALSE, echo=FALSE}
renderDataTable({datatable(coty_adorda() %>% group_by(country_code,date) %>% select(cart_adds,cart_customers,cart_drops,cart_drops_customers), rownames=FALSE, class="cell-border stripe", extensions='Buttons', options=list(dom='Bfrtip', buttons=c('copy', 'csv'), initComplete=JS("function(settings, json) {","$('body').css({'font-family': 'Calibri'});","}")))})
```

#### Checkout - Orders

```{r, message=FALSE, echo=FALSE}
renderDataTable({datatable(coty_adorda() %>% group_by(country_code,date) %>% select(checkouts,checkouts_customers,orders,orders_customers), rownames=FALSE, class="cell-border stripe", extensions='Buttons', options=list(dom='Bfrtip', buttons=c('copy', 'csv'), initComplete=JS("function(settings, json) {","$('body').css({'font-family': 'Calibri'});","}")))})
```

#### Orders

```{r, message=FALSE, echo=FALSE}
renderDataTable({datatable(coty_adorda() %>% group_by(country_code,date) %>% select(delivered,AU,cancels,cancels_customers), rownames=FALSE, class="cell-border stripe", extensions='Buttons', options=list(dom='Bfrtip', buttons=c('copy', 'csv'), initComplete=JS("function(settings, json) {","$('body').css({'font-family': 'Calibri'});","}")))})
```

#### Values

```{r, message=FALSE, echo=FALSE}
renderDataTable({datatable(coty_adorda() %>% group_by(country_code,date) %>% select(cart_value,cart_drops_value,order_value), rownames=FALSE, class="cell-border stripe", extensions='Buttons', options=list(dom='Bfrtip', buttons=c('copy', 'csv'), initComplete=JS("function(settings, json) {","$('body').css({'font-family': 'Calibri'});","}")))})
```

#### Products

```{r, message=FALSE, echo=FALSE}
renderDataTable({datatable(coty_adorda() %>% group_by(country_code,date) %>% select(cart_products,cart_drops_products), rownames=FALSE, class="cell-border stripe", extensions='Buttons', options=list(dom='Bfrtip', buttons=c('copy', 'csv'), initComplete=JS("function(settings, json) {","$('body').css({'font-family': 'Calibri'});","}")))})
```

### Plots {.tabset}

#### Home

```{r, message=FALSE, echo=FALSE}
renderPlotly({ggplotly(coty_homegg()) %>% layout(legend=list(orientation="h", x=0.3, y=-0.05))})
```

#### Category

```{r, message=FALSE, echo=FALSE}
renderPlotly({ggplotly(coty_categg()) %>% layout(legend=list(orientation="h", x=0.3, y=-0.05))})
```

#### Store

```{r, message=FALSE, echo=FALSE}
renderPlotly({ggplotly(coty_storgg()) %>% layout(legend=list(orientation="h", x=0.3, y=-0.05))})
```

#### Add Cart - Drop Carts

```{r, message=FALSE, echo=FALSE}
renderPlotly({ggplotly(coty_addrgg()) %>% layout(legend=list(orientation="h", x=0.3, y=-0.05))})
```

#### Checkouts - Orders

```{r, message=FALSE, echo=FALSE}
renderPlotly({ggplotly(coty_chorgg()) %>% layout(legend=list(orientation="h", x=0.3, y=-0.05))})
```

#### Orders

```{r, message=FALSE, echo=FALSE}
renderPlotly({ggplotly(coty_orcagg()) %>% layout(legend=list(orientation="h", x=0.3, y=-0.05))})
```

#### Values

```{r, message=FALSE, echo=FALSE}
renderPlotly({ggplotly(coty_valugg()) %>% layout(legend=list(orientation="h", x=0.3, y=-0.05))})
```

#### Products

```{r, message=FALSE, echo=FALSE}
renderPlotly({ggplotly(coty_prodgg()) %>% layout(legend=list(orientation="h", x=0.3, y=-0.05))})
```

---------------

## City

### Info {.tabset}

#### Home

```{r, message=FALSE, echo=FALSE}
renderDataTable({datatable(city_hometl(), rownames=FALSE, class="cell-border stripe", extensions='Buttons', options=list(dom='Bfrtip', buttons=c('copy', 'csv'), initComplete=JS("function(settings, json) {","$('body').css({'font-family': 'Calibri'});","}"))) %>% formatPercentage("CR",2)})
```

#### Category

```{r, message=FALSE, echo=FALSE}
renderDataTable({datatable(city_catetl(), rownames=FALSE, class="cell-border stripe", extensions='Buttons', options=list(dom='Bfrtip', buttons=c('copy', 'csv'), initComplete=JS("function(settings, json) {","$('body').css({'font-family': 'Calibri'});","}"))) %>% formatPercentage("CR",2)})
```

#### Store

```{r, message=FALSE, echo=FALSE}
renderDataTable({datatable(city_stortl(), rownames=FALSE, class="cell-border stripe", extensions='Buttons', options=list(dom='Bfrtip', buttons=c('copy', 'csv'), initComplete=JS("function(settings, json) {","$('body').css({'font-family': 'Calibri'});","}"))) %>% formatPercentage("CR",2)})
```

#### Add Cart - Drop Carts

```{r, message=FALSE, echo=FALSE}
renderDataTable({datatable(city_adortl() %>% group_by(city_code) %>% select(cart_adds,cart_customers,cart_drops,cart_drops_customers), rownames=FALSE, class="cell-border stripe", extensions='Buttons', options=list(dom='Bfrtip', buttons=c('copy', 'csv'), initComplete=JS("function(settings, json) {","$('body').css({'font-family': 'Calibri'});","}")))})
```

#### Checkout - Orders

```{r, message=FALSE, echo=FALSE}
renderDataTable({datatable(city_adortl() %>% group_by(city_code) %>% select(checkouts,checkouts_customers,orders,orders_customers), rownames=FALSE, class="cell-border stripe", extensions='Buttons', options=list(dom='Bfrtip', buttons=c('copy', 'csv'), initComplete=JS("function(settings, json) {","$('body').css({'font-family': 'Calibri'});","}")))})
```

#### Values

```{r, message=FALSE, echo=FALSE}
renderDataTable({datatable(city_adortl() %>% group_by(city_code) %>% select(cart_value,cart_drops_value,order_value), rownames=FALSE, class="cell-border stripe", extensions='Buttons', options=list(dom='Bfrtip', buttons=c('copy', 'csv'), initComplete=JS("function(settings, json) {","$('body').css({'font-family': 'Calibri'});","}")))})
```

#### Products

```{r, message=FALSE, echo=FALSE}
renderDataTable({datatable(city_adortl() %>% group_by(city_code) %>% select(cart_products,cart_drops_products), rownames=FALSE, class="cell-border stripe", extensions='Buttons', options=list(dom='Bfrtip', buttons=c('copy', 'csv'), initComplete=JS("function(settings, json) {","$('body').css({'font-family': 'Calibri'});","}")))})
```

### Tables {.tabset}

#### Home

```{r, message=FALSE, echo=FALSE}
renderDataTable({datatable(city_homeda(), rownames=FALSE, class="cell-border stripe", extensions='Buttons', options=list(dom='Bfrtip', buttons=c('copy', 'csv'), initComplete=JS("function(settings, json) {","$('body').css({'font-family': 'Calibri'});","}"))) %>% formatPercentage("CR",2)})
```

#### Category

```{r, message=FALSE, echo=FALSE}
renderDataTable({datatable(city_cateda(), rownames=FALSE, class="cell-border stripe", extensions='Buttons', options=list(dom='Bfrtip', buttons=c('copy', 'csv'), initComplete=JS("function(settings, json) {","$('body').css({'font-family': 'Calibri'});","}"))) %>% formatPercentage("CR",2)})
```

#### Store

```{r, message=FALSE, echo=FALSE}
renderDataTable({datatable(city_storda(), rownames=FALSE, class="cell-border stripe", extensions='Buttons', options=list(dom='Bfrtip', buttons=c('copy', 'csv'), initComplete=JS("function(settings, json) {","$('body').css({'font-family': 'Calibri'});","}"))) %>% formatPercentage("CR",2)})
```

#### Add Cart - Drop Carts

```{r, message=FALSE, echo=FALSE}
renderDataTable({datatable(city_adorda() %>% group_by(city_code,date) %>% select(cart_adds,cart_customers,cart_drops,cart_drops_customers), rownames=FALSE, class="cell-border stripe", extensions='Buttons', options=list(dom='Bfrtip', buttons=c('copy', 'csv'), initComplete=JS("function(settings, json) {","$('body').css({'font-family': 'Calibri'});","}")))})
```

#### Checkout - Orders

```{r, message=FALSE, echo=FALSE}
renderDataTable({datatable(city_adorda() %>% group_by(city_code,date) %>% select(checkouts,checkouts_customers,orders,orders_customers), rownames=FALSE, class="cell-border stripe", extensions='Buttons', options=list(dom='Bfrtip', buttons=c('copy', 'csv'), initComplete=JS("function(settings, json) {","$('body').css({'font-family': 'Calibri'});","}")))})
```

#### Values

```{r, message=FALSE, echo=FALSE}
renderDataTable({datatable(city_adorda() %>% group_by(city_code,date) %>% select(cart_value,cart_drops_value,order_value), rownames=FALSE, class="cell-border stripe", extensions='Buttons', options=list(dom='Bfrtip', buttons=c('copy', 'csv'), initComplete=JS("function(settings, json) {","$('body').css({'font-family': 'Calibri'});","}")))})
```

#### Products

```{r, message=FALSE, echo=FALSE}
renderDataTable({datatable(city_adorda() %>% group_by(city_code,date) %>% select(cart_products,cart_drops_products), rownames=FALSE, class="cell-border stripe", extensions='Buttons', options=list(dom='Bfrtip', buttons=c('copy', 'csv'), initComplete=JS("function(settings, json) {","$('body').css({'font-family': 'Calibri'});","}")))})
```

### Plots {.tabset}

#### Home

```{r, message=FALSE, echo=FALSE}
renderPlotly({ggplotly(city_homegg()) %>% layout(legend=list(orientation="h", x=0.3, y=-0.05))})
```

#### Category

```{r, message=FALSE, echo=FALSE}
renderPlotly({ggplotly(city_categg()) %>% layout(legend=list(orientation="h", x=0.3, y=-0.05))})
```

#### Store

```{r, message=FALSE, echo=FALSE}
renderPlotly({ggplotly(city_storgg()) %>% layout(legend=list(orientation="h", x=0.3, y=-0.05))})
```

#### Add Cart - Drop Carts

```{r, message=FALSE, echo=FALSE}
renderPlotly({ggplotly(city_addrgg()) %>% layout(legend=list(orientation="h", x=0.3, y=-0.05))})
```

#### Checkouts - Orders

```{r, message=FALSE, echo=FALSE}
renderPlotly({ggplotly(city_chorgg()) %>% layout(legend=list(orientation="h", x=0.3, y=-0.05))})
```

#### Values

```{r, message=FALSE, echo=FALSE}
renderPlotly({ggplotly(city_valugg()) %>% layout(legend=list(orientation="h", x=0.3, y=-0.05))})
```

#### Products

```{r, message=FALSE, echo=FALSE}
renderPlotly({ggplotly(city_prodgg()) %>% layout(legend=list(orientation="h", x=0.3, y=-0.05))})
```

## Category

### Info {.tabset}

#### Category

```{r, message=FALSE, echo=FALSE}
renderDataTable({datatable(cate_catetl(), rownames=FALSE, class="cell-border stripe", extensions='Buttons', options=list(dom='Bfrtip', buttons=c('copy', 'csv'), initComplete=JS("function(settings, json) {","$('body').css({'font-family': 'Calibri'});","}"))) %>% formatPercentage("CR",2)})
```

#### Store

```{r, message=FALSE, echo=FALSE}
renderDataTable({datatable(cate_stortl(), rownames=FALSE, class="cell-border stripe", extensions='Buttons', options=list(dom='Bfrtip', buttons=c('copy', 'csv'), initComplete=JS("function(settings, json) {","$('body').css({'font-family': 'Calibri'});","}"))) %>% formatPercentage("CR",2)})
```

#### Add Cart - Drop Carts

```{r, message=FALSE, echo=FALSE}
renderDataTable({datatable(cate_adortl() %>% group_by(category_name) %>% select(cart_adds,cart_customers,cart_drops,cart_drops_customers), rownames=FALSE, class="cell-border stripe", extensions='Buttons', options=list(dom='Bfrtip', buttons=c('copy', 'csv'), initComplete=JS("function(settings, json) {","$('body').css({'font-family': 'Calibri'});","}")))})
```

#### Checkout - Orders

```{r, message=FALSE, echo=FALSE}
renderDataTable({datatable(cate_adortl() %>% group_by(category_name) %>% select(checkouts,checkouts_customers,orders,orders_customers), rownames=FALSE, class="cell-border stripe", extensions='Buttons', options=list(dom='Bfrtip', buttons=c('copy', 'csv'), initComplete=JS("function(settings, json) {","$('body').css({'font-family': 'Calibri'});","}")))})
```

#### Orders

```{r, message=FALSE, echo=FALSE}
renderDataTable({datatable(cate_adortl() %>% group_by(category_name) %>% select(delivered,AU,cancels,cancels_customers), rownames=FALSE, class="cell-border stripe", extensions='Buttons', options=list(dom='Bfrtip', buttons=c('copy', 'csv'), initComplete=JS("function(settings, json) {","$('body').css({'font-family': 'Calibri'});","}")))})
```

#### Values

```{r, message=FALSE, echo=FALSE}
renderDataTable({datatable(cate_adortl() %>% group_by(category_name) %>% select(cart_value,cart_drops_value,order_value), rownames=FALSE, class="cell-border stripe", extensions='Buttons', options=list(dom='Bfrtip', buttons=c('copy', 'csv'), initComplete=JS("function(settings, json) {","$('body').css({'font-family': 'Calibri'});","}")))})
```

#### Products

```{r, message=FALSE, echo=FALSE}
renderDataTable({datatable(cate_adortl() %>% group_by(category_name) %>% select(cart_products,cart_drops_products), rownames=FALSE, class="cell-border stripe", extensions='Buttons', options=list(dom='Bfrtip', buttons=c('copy', 'csv'), initComplete=JS("function(settings, json) {","$('body').css({'font-family': 'Calibri'});","}")))})
```

### Tables {.tabset}

#### Category

```{r, message=FALSE, echo=FALSE}
renderDataTable({datatable(cate_cateda(), rownames=FALSE, class="cell-border stripe", extensions='Buttons', options=list(dom='Bfrtip', buttons=c('copy', 'csv'), initComplete=JS("function(settings, json) {","$('body').css({'font-family': 'Calibri'});","}"))) %>% formatPercentage("CR",2)})
```

#### Store

```{r, message=FALSE, echo=FALSE}
renderDataTable({datatable(cate_storda(), rownames=FALSE, class="cell-border stripe", extensions='Buttons', options=list(dom='Bfrtip', buttons=c('copy', 'csv'), initComplete=JS("function(settings, json) {","$('body').css({'font-family': 'Calibri'});","}"))) %>% formatPercentage("CR",2)})
```

#### Add Cart - Drop Carts

```{r, message=FALSE, echo=FALSE}
renderDataTable({datatable(cate_adorda() %>% group_by(category_name,date) %>% select(cart_products,cart_customers,cart_drops,cart_drops_customers), rownames=FALSE, class="cell-border stripe", extensions='Buttons', options=list(dom='Bfrtip', buttons=c('copy', 'csv'), initComplete=JS("function(settings, json) {","$('body').css({'font-family': 'Calibri'});","}")))})
```

#### Checkout - Orders

```{r, message=FALSE, echo=FALSE}
renderDataTable({datatable(cate_adorda() %>% group_by(category_name,date) %>% select(checkouts,checkouts_customers,orders,orders_customers), rownames=FALSE, class="cell-border stripe", extensions='Buttons', options=list(dom='Bfrtip', buttons=c('copy', 'csv'), initComplete=JS("function(settings, json) {","$('body').css({'font-family': 'Calibri'});","}")))})
```

#### Orders

```{r, message=FALSE, echo=FALSE}
renderDataTable({datatable(cate_adorda() %>% group_by(category_name,date) %>% select(delivered,AU,cancels,cancels_customers), rownames=FALSE, class="cell-border stripe", extensions='Buttons', options=list(dom='Bfrtip', buttons=c('copy', 'csv'), initComplete=JS("function(settings, json) {","$('body').css({'font-family': 'Calibri'});","}")))})
```

#### Values

```{r, message=FALSE, echo=FALSE}
renderDataTable({datatable(cate_adorda() %>% group_by(category_name,date) %>% select(cart_value,cart_drops_value,order_value), rownames=FALSE, class="cell-border stripe", extensions='Buttons', options=list(dom='Bfrtip', buttons=c('copy', 'csv'), initComplete=JS("function(settings, json) {","$('body').css({'font-family': 'Calibri'});","}")))})
```

#### Products

```{r, message=FALSE, echo=FALSE}
renderDataTable({datatable(cate_adorda() %>% group_by(category_name,date) %>% select(cart_products,cart_drops_products), rownames=FALSE, class="cell-border stripe", extensions='Buttons', options=list(dom='Bfrtip', buttons=c('copy', 'csv'), initComplete=JS("function(settings, json) {","$('body').css({'font-family': 'Calibri'});","}")))})
```

### Plots {.tabset}

#### Category

```{r, message=FALSE, echo=FALSE}
renderPlotly({ggplotly(cate_categg()) %>% layout(legend=list(orientation="h", x=0.3, y=-0.05))})
```

#### Store

```{r, message=FALSE, echo=FALSE}
renderPlotly({ggplotly(cate_storgg()) %>% layout(legend=list(orientation="h", x=0.3, y=-0.05))})
```

#### Add Cart - Drop Carts

```{r, message=FALSE, echo=FALSE}
renderPlotly({ggplotly(cate_addrgg()) %>% layout(legend=list(orientation="h", x=0.3, y=-0.05))})
```

#### Checkouts - Orders

```{r, message=FALSE, echo=FALSE}
renderPlotly({ggplotly(cate_chorgg()) %>% layout(legend=list(orientation="h", x=0.3, y=-0.05))})
```

#### Orders

```{r, message=FALSE, echo=FALSE}
renderPlotly({ggplotly(cate_orcagg()) %>% layout(legend=list(orientation="h", x=0.3, y=-0.05))})
```

#### Values

```{r, message=FALSE, echo=FALSE}
renderPlotly({ggplotly(cate_valugg()) %>% layout(legend=list(orientation="h", x=0.3, y=-0.05))})
```

#### Products

```{r, message=FALSE, echo=FALSE}
renderPlotly({ggplotly(cate_prodgg()) %>% layout(legend=list(orientation="h", x=0.3, y=-0.05))})
```

-----------

## Store 

### Info {.tabset}

#### Store

```{r, message=FALSE, echo=FALSE}
renderDataTable({datatable(stor_stortl(), rownames=FALSE, class="cell-border stripe", extensions='Buttons', options=list(dom='Bfrtip', buttons=c('copy', 'csv'), initComplete=JS("function(settings, json) {","$('body').css({'font-family': 'Calibri'});","}"))) %>% formatPercentage("CR",2)})
```

#### Add Cart - Drop Carts

```{r, message=FALSE, echo=FALSE}
renderDataTable({datatable(stor_adortl() %>% group_by(store_name) %>% select(cart_adds,cart_customers,cart_drops,cart_drops_customers), rownames=FALSE, class="cell-border stripe", extensions='Buttons', options=list(dom='Bfrtip', buttons=c('copy', 'csv'), initComplete=JS("function(settings, json) {","$('body').css({'font-family': 'Calibri'});","}")))})
```

#### Checkout - Orders

```{r, message=FALSE, echo=FALSE}
renderDataTable({datatable(stor_adortl() %>% group_by(store_name) %>% select(checkouts,checkouts_customers,orders,orders_customers), rownames=FALSE, class="cell-border stripe", extensions='Buttons', options=list(dom='Bfrtip', buttons=c('copy', 'csv'), initComplete=JS("function(settings, json) {","$('body').css({'font-family': 'Calibri'});","}")))})
```

#### Orders

```{r, message=FALSE, echo=FALSE}
renderDataTable({datatable(stor_adortl() %>% group_by(store_name) %>% select(delivered,AU,cancels,cancels_customers), rownames=FALSE, class="cell-border stripe", extensions='Buttons', options=list(dom='Bfrtip', buttons=c('copy', 'csv'), initComplete=JS("function(settings, json) {","$('body').css({'font-family': 'Calibri'});","}")))})
```

#### Values

```{r, message=FALSE, echo=FALSE}
renderDataTable({datatable(stor_adortl() %>% group_by(store_name) %>% select(cart_value,cart_drops_value,order_value), rownames=FALSE, class="cell-border stripe", extensions='Buttons', options=list(dom='Bfrtip', buttons=c('copy', 'csv'), initComplete=JS("function(settings, json) {","$('body').css({'font-family': 'Calibri'});","}")))})
```

#### Products

```{r, message=FALSE, echo=FALSE}
renderDataTable({datatable(stor_adortl() %>% group_by(store_name) %>% select(cart_products,cart_drops_products), rownames=FALSE, class="cell-border stripe", extensions='Buttons', options=list(dom='Bfrtip', buttons=c('copy', 'csv'), initComplete=JS("function(settings, json) {","$('body').css({'font-family': 'Calibri'});","}")))})
```

### Tables {.tabset}

#### Store

```{r, message=FALSE, echo=FALSE}
renderDataTable({datatable(stor_storda(), rownames=FALSE, class="cell-border stripe", extensions='Buttons', options=list(dom='Bfrtip', buttons=c('copy', 'csv'), initComplete=JS("function(settings, json) {","$('body').css({'font-family': 'Calibri'});","}"))) %>% formatPercentage("CR",2)})
```

#### Add Cart - Drop Carts

```{r, message=FALSE, echo=FALSE}
renderDataTable({datatable(stor_adorda() %>% group_by(store_name,date) %>% select(cart_adds,cart_customers,cart_drops,cart_drops_customers), rownames=FALSE, class="cell-border stripe", extensions='Buttons', options=list(dom='Bfrtip', buttons=c('copy', 'csv'), initComplete=JS("function(settings, json) {","$('body').css({'font-family': 'Calibri'});","}")))})
```

#### Checkout - Orders

```{r, message=FALSE, echo=FALSE}
renderDataTable({datatable(stor_adorda() %>% group_by(store_name,date) %>% select(checkouts,checkouts_customers,orders,orders_customers), rownames=FALSE, class="cell-border stripe", extensions='Buttons', options=list(dom='Bfrtip', buttons=c('copy', 'csv'), initComplete=JS("function(settings, json) {","$('body').css({'font-family': 'Calibri'});","}")))})
```

#### Orders

```{r, message=FALSE, echo=FALSE}
renderDataTable({datatable(stor_adorda() %>% group_by(store_name,date) %>% select(delivered,AU,cancels,cancels_customers), rownames=FALSE, class="cell-border stripe", extensions='Buttons', options=list(dom='Bfrtip', buttons=c('copy', 'csv'), initComplete=JS("function(settings, json) {","$('body').css({'font-family': 'Calibri'});","}")))})
```

#### Values

```{r, message=FALSE, echo=FALSE}
renderDataTable({datatable(stor_adorda() %>% group_by(store_name,date) %>% select(cart_value,cart_drops_value,order_value), rownames=FALSE, class="cell-border stripe", extensions='Buttons', options=list(dom='Bfrtip', buttons=c('copy', 'csv'), initComplete=JS("function(settings, json) {","$('body').css({'font-family': 'Calibri'});","}")))})
```

#### Products

```{r, message=FALSE, echo=FALSE}
renderDataTable({datatable(stor_adorda() %>% group_by(store_name,date) %>% select(cart_products,cart_drops_products), rownames=FALSE, class="cell-border stripe", extensions='Buttons', options=list(dom='Bfrtip', buttons=c('copy', 'csv'), initComplete=JS("function(settings, json) {","$('body').css({'font-family': 'Calibri'});","}")))})
```

### Plots {.tabset}

#### Store

```{r, message=FALSE, echo=FALSE}
renderPlotly({ggplotly(stor_storgg()) %>% layout(legend=list(orientation="h", x=0.3, y=-0.05))})
```

#### Add Cart - Drop Carts

```{r, message=FALSE, echo=FALSE}
renderPlotly({ggplotly(stor_addrgg()) %>% layout(legend=list(orientation="h", x=0.3, y=-0.05))})
```

#### Checkouts - Orders

```{r, message=FALSE, echo=FALSE}
renderPlotly({ggplotly(stor_chorgg()) %>% layout(legend=list(orientation="h", x=0.3, y=-0.05))})
```

#### Orders

```{r, message=FALSE, echo=FALSE}
renderPlotly({ggplotly(stor_orcagg()) %>% layout(legend=list(orientation="h", x=0.3, y=-0.05))})
```

#### Values

```{r, message=FALSE, echo=FALSE}
renderPlotly({ggplotly(stor_valugg()) %>% layout(legend=list(orientation="h", x=0.3, y=-0.05))})
```

#### Products

```{r, message=FALSE, echo=FALSE}
renderPlotly({ggplotly(stor_prodgg()) %>% layout(legend=list(orientation="h", x=0.3, y=-0.05))})
```

---

## Store Address

### Info {.tabset}

#### Add Cart - Drop Carts

```{r, message=FALSE, echo=FALSE}
renderDataTable({datatable(stad_adortl() %>% group_by(store_address_id,store_name) %>% select(cart_adds,cart_customers,cart_drops,cart_drops_customers), rownames=FALSE, class="cell-border stripe", extensions='Buttons', options=list(dom='Bfrtip', buttons=c('copy', 'csv'), initComplete=JS("function(settings, json) {","$('body').css({'font-family': 'Calibri'});","}")))})
```

#### Checkout - Orders

```{r, message=FALSE, echo=FALSE}
renderDataTable({datatable(stad_adortl() %>% group_by(store_address_id,store_name) %>% select(checkouts,checkouts_customers,orders,orders_customers), rownames=FALSE, class="cell-border stripe", extensions='Buttons', options=list(dom='Bfrtip', buttons=c('copy', 'csv'), initComplete=JS("function(settings, json) {","$('body').css({'font-family': 'Calibri'});","}")))})
```

#### Orders

```{r, message=FALSE, echo=FALSE}
renderDataTable({datatable(stad_adortl() %>% group_by(store_address_id,store_name) %>% select(delivered,AU,cancels,cancels_customers), rownames=FALSE, class="cell-border stripe", extensions='Buttons', options=list(dom='Bfrtip', buttons=c('copy', 'csv'), initComplete=JS("function(settings, json) {","$('body').css({'font-family': 'Calibri'});","}")))})
```

#### Values

```{r, message=FALSE, echo=FALSE}
renderDataTable({datatable(stad_adortl() %>% group_by(store_address_id,store_name) %>% select(cart_value,cart_drops_value,order_value), rownames=FALSE, class="cell-border stripe", extensions='Buttons', options=list(dom='Bfrtip', buttons=c('copy', 'csv'), initComplete=JS("function(settings, json) {","$('body').css({'font-family': 'Calibri'});","}")))})
```

#### Products

```{r, message=FALSE, echo=FALSE}
renderDataTable({datatable(stad_adortl() %>% group_by(store_address_id,store_name) %>% select(cart_products,cart_drops_products), rownames=FALSE, class="cell-border stripe", extensions='Buttons', options=list(dom='Bfrtip', buttons=c('copy', 'csv'), initComplete=JS("function(settings, json) {","$('body').css({'font-family': 'Calibri'});","}")))})
```

### Tables {.tabset}

#### Add Cart - Drop Carts

```{r, message=FALSE, echo=FALSE}
renderDataTable({datatable(stad_adorda() %>% group_by(store_address_id, store_name, date) %>% select(cart_adds,cart_customers,cart_drops,cart_drops_customers), rownames=FALSE, class="cell-border stripe", extensions='Buttons', options=list(dom='Bfrtip', buttons=c('copy', 'csv'), initComplete=JS("function(settings, json) {","$('body').css({'font-family': 'Calibri'});","}")))})
```

#### Checkout - Orders

```{r, message=FALSE, echo=FALSE}
renderDataTable({datatable(stad_adorda() %>% group_by(store_address_id, store_name, date) %>% select(checkouts,checkouts_customers,orders,orders_customers), rownames=FALSE, class="cell-border stripe", extensions='Buttons', options=list(dom='Bfrtip', buttons=c('copy', 'csv'), initComplete=JS("function(settings, json) {","$('body').css({'font-family': 'Calibri'});","}")))})
```

#### Orders

```{r, message=FALSE, echo=FALSE}
renderDataTable({datatable(stad_adorda() %>% group_by(store_address_id, store_name, date) %>% select(delivered,AU,cancels,cancels_customers), rownames=FALSE, class="cell-border stripe", extensions='Buttons', options=list(dom='Bfrtip', buttons=c('copy', 'csv'), initComplete=JS("function(settings, json) {","$('body').css({'font-family': 'Calibri'});","}")))})
```

#### Values

```{r, message=FALSE, echo=FALSE}
renderDataTable({datatable(stad_adorda() %>% group_by(store_address_id, store_name, date) %>% select(cart_value,cart_drops_value,order_value), rownames=FALSE, class="cell-border stripe", extensions='Buttons', options=list(dom='Bfrtip', buttons=c('copy', 'csv'), initComplete=JS("function(settings, json) {","$('body').css({'font-family': 'Calibri'});","}")))})
```

#### Products

```{r, message=FALSE, echo=FALSE}
renderDataTable({datatable(stad_adorda() %>% group_by(store_address_id, store_name, date) %>% select(cart_products,cart_drops_products), rownames=FALSE, class="cell-border stripe", extensions='Buttons', options=list(dom='Bfrtip', buttons=c('copy', 'csv'), initComplete=JS("function(settings, json) {","$('body').css({'font-family': 'Calibri'});","}")))})
```

### Plots {.tabset}

#### Add Cart - Drop Carts

```{r, message=FALSE, echo=FALSE}
renderPlotly({ggplotly(stad_addrgg()) %>% layout(legend=list(orientation="h", x=0.3, y=-0.05))})
```

#### Checkouts - Orders

```{r, message=FALSE, echo=FALSE}
renderPlotly({ggplotly(stad_chorgg()) %>% layout(legend=list(orientation="h", x=0.3, y=-0.05))})
```

#### Orders

```{r, message=FALSE, echo=FALSE}
renderPlotly({ggplotly(stad_orcagg()) %>% layout(legend=list(orientation="h", x=0.3, y=-0.05))})
```

#### Values

```{r, message=FALSE, echo=FALSE}
renderPlotly({ggplotly(stad_valugg()) %>% layout(legend=list(orientation="h", x=0.3, y=-0.05))})
```

#### Products

```{r, message=FALSE, echo=FALSE}
renderPlotly({ggplotly(stad_prodgg()) %>% layout(legend=list(orientation="h", x=0.3, y=-0.05))})
```

## Downloads

Selecciona filtros (fecha, ciudad, categoría, tag y nombre de tienda) y puedes descargar la información en formato csv con este botón:

```{r, message=FALSE, echo=FALSE}
downloadHandler(filename=function(){paste(paste("funnel", Sys.Date(), sep="-"), ".csv", sep="")},
                content=function(file){write_csv(funnelstor(), file)}
                )
```

## Notes

Se pueden apagar los kpis de los gráficos haciendo click a sus nombres en la leyenda.

Los kpis que se visualizan en este reporte son los siguientes:

- Date: Fecha de creación de evento (vista)
- Country code: País
- City code: Ciudad

- Home views: Vistas de la pantalla Home
- Home customers: Vistas de la pantalla Home (usuarios únicos)

- Category ID: ID de la categoría (necesario para tiendas Mirror)
- Category views: Vistas de la pantalla Category
- Category customers: Vistas de la pantalla Category (usuarios únicos)

- Store ID: ID de la marca
- Store Address ID: ID del local de la marca (tienda)
- Store Name: Nombre de la marca (tienda)
- Tag: Etiqueta de la tienda (única)
- Big Chain: Indica si la tienda es parte de un big chain.
- Local Hero: Indica si la tienda es parte de un local hero.
- Store Address: Dirección de la tienda
- District: Distrito donde se encuentra la tienda
- Store Views: Vistas de la pantalla Store
- Store Customers: Vistas de la pantalla Store (usuarios únicos)

- Cart Adds: Total de eventos para agregar productos al carrito de compras
- Cart Customer: Usuarios únicos que agregaron productos al carrito de compras
- Cart Products: Total de productos agregados al carrito de compras
- Cart Value: Valor total de productos agregados al carrito de compras

- Cart Drops: Total de eventos para quitar productos al carrito de compras
- Cart Drops Customers: Usuarios únicos que quitaron productos del carrito de compras
- Cart Drops Products: Total de productos quitados del carrito de compras
- Cart Drops Value: Valor total de productos quitados del carrito de compras

- Checkouts: Total de eventos en la pantalla Checkout antes de realizar la compra
- Checkouts Customers: Usuarios únicos en la pantalla Checkout

- Orders: Ordenes creadas
- Orders Customers: Usuarios únicos que crearon ordenes

- AOV: Average Order Value
- Final Value: Valor final de las ordenes creadas

- Delivered: Ordenes entregadas
- Active Users: Usuarios únicos de las ordenes entregadas

- Cancels: Ordenes canceladas
- Canceled Orders Customer: Usuarios únicos de las ordenes canceladas
