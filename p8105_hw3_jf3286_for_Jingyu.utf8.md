---
title: "p8105_hw3_jf3286.Rmd"
author: "Jingyu Fu"
date: "2019/10/13"
output: html_document
---






## Prob 1

load data from website

```r
library(p8105.datasets)
data("instacart")
```

Description: 

This data has 15 columns and1384617rows. It contains numeric columns and character columns. Overall, this data contains inforation about order ID, product ID, customer ID, aisle name, the number of products, the number of order, etc. The key variables are those including ID information, beucase these variables are unique to each order or user. From this data we can tell what has a certain customer purchased. For example, we can tell thet user ID 112108 purchased Bulgarian Yogurt, Organic 4% Milk Fat Whole Milk Cottage Cheese, Organic Celery Hearts, and other 5 types of product. We can also tell how many times a certain customer has ordered a product. For example, user ID 112108 ordered Bulgarian Yogurt twice, becuase it is shown in caolumn of reordered that it has been reordered once. 


Number of aisle

```r
  instacart%>% 
  group_by (aisle) %>%
  summarize (
    n_aisle = n()
    )
```

```
## # A tibble: 134 x 2
##    aisle                  n_aisle
##    <chr>                    <int>
##  1 air fresheners candles    1067
##  2 asian foods               7007
##  3 baby accessories           306
##  4 baby bath body care        328
##  5 baby food formula        13198
##  6 bakery desserts           1501
##  7 baking ingredients       13088
##  8 baking supplies decor     1094
##  9 beauty                     287
## 10 beers coolers             1839
## # ... with 124 more rows
```

Aisle that are most items ordered from

```r
 instacart_n_items = instacart %>% 
  group_by(aisle) %>%
  summarize (
    n_items = sum(add_to_cart_order)
  ) 

  instacart_n_items %>%  mutate(
    rank_items = rank(n_items)
  ) %>% 
  filter (
    rank_items == 1
  )
```

```
## # A tibble: 1 x 3
##   aisle                      n_items rank_items
##   <chr>                        <int>      <dbl>
## 1 specialty wines champagnes    2491          1
```

Comments

Most items are ordered from aisle specialty wines champagnes, it can be assumed that among those investigated aisles in this data, specialty wines champagnes is the most popular aisle. 



Making plots,limiting to aisles with more than 10000 items, arranging aisles, and organizing plots

```r
instacart_n_items %>% 
  filter(
    n_items >= 10000
    ) %>% 
   mutate(
    aisle = factor(aisle),
    aisle = fct_reorder(aisle, n_items)
  ) %>% 
  ggplot(aes (x = aisle, y = n_items, color ="blue")) + 
  geom_point(alpha = .5) + 
  labs(
    title = "Aisle and number of items ordered",
    x = "Aisle",
    y = "Nuber of items ordered",
    caption = "Data from instcart"
  ) +
  theme_bw()+
  theme(legend.position = "bottom")+
  viridis::scale_color_viridis(
    name = "Aisle", 
    discrete = TRUE
  )
```

<img src="p8105_hw3_jf3286_for_Jingyu_files/figure-html/unnamed-chunk-4-1.png" width="90%" />

Comments

The plots look like a function graph. The number of items ordered from each ailse increased steadily at first but then there's a sudden  severe increase. For most of the aisles, each has  10110 ~ (5e^5)/2 items ordered, but for several others, there are a lot more.  


Making tables showing the three most popular items in each of the aisles

```r
instacart %>% 
  group_by(aisle) %>% 
  mutate(
    add_to_cart_order = desc(add_to_cart_order),
    rank_product = min_rank(add_to_cart_order)
  ) %>% 
  filter(
    rank_product == 1,2,3
  ) %>% 
  select(aisle, rank_product, product_name, add_to_cart_order) %>% 
   knitr::kable()
```



aisle                            rank_product  product_name                                                              add_to_cart_order
------------------------------  -------------  -----------------------------------------------------------------------  ------------------
feminine care                               1  Ultimate Flora Vaginal Support Probiotic Supplement Vegetable Capsules                  -65
dog food care                               1  Sausage Cuts Real Beef Treats                                                           -66
energy granola bars                         1  Mighty Bar Beef with Uncured Bacon & Apple Protein Bar                                  -69
bread                                       1  Sourdough Bread                                                                         -70
soft drinks                                 1  Diet 12 Oz Ginger Ale                                                                   -60
specialty cheeses                           1  Fresh Mozzarella Snacking Cheese                                                        -64
frozen breads doughs                        1  Gluten Free Whole Grain Bread                                                           -52
body lotions soap                           1  Hydro Body Sponge With Hand Strap                                                       -57
facial care                                 1  Deep Action Cream Cleanser                                                              -52
tofu meat alternatives                      1  Meatless Vegan Hot Dogs                                                                 -55
other                                       1  Premium Lubricant Condoms ENZ                                                           -67
hair care                                   1  Color Me Happy Colorsafe Shampoo                                                        -68
laundry                                     1  Regular Concentrated Bleach                                                             -69
dish detergents                             1  Dishwasher Detergent                                                                    -70
food storage                                1  Aluminum Foil                                                                           -71
spirits                                     1  Single Malt Scotch Whisky                                                               -67
beers coolers                               1  Dark Belgian Beer                                                                       -75
missing                                     1  YoKids Organic Low Fat Strawberry Banana Yogurt                                         -57
soap                                        1  Safe & Natural Tropical Coconut Twist Liquid Soap For Kids                              -67
bakery desserts                             1  Chocolate Cupcake                                                                       -69
grains rice dried goods                     1  All Natural Organic Green Lentils                                                       -70
candy chocolate                             1  Pretzel Chocolate Organic                                                               -72
ice cream ice                               1  Chocolate Ice Cream                                                                     -73
hot dogs bacon sausage                      1  Organic Sunday Bacon                                                                    -74
oils vinegars                               1  Extra Virgin Olive Oil                                                                  -75
canned meat seafood                         1  Wild Albacore Tuna No Salt Added                                                        -76
muscles joints pain relief                  1  Premium Epsom Salt                                                                      -47
protein meal replacements                   1  Alkalize Detox Green Super Food Single Packet                                           -64
soup broth bouillon                         1  Organic Beef Broth                                                                      -58
oral hygiene                                1  Pronamel Gentle Whitening Toothpaste                                                    -59
packaged poultry                            1  Lean Ground Turky                                                                       -61
packaged cheese                             1  Shredded Sharp Cheddar Cheese                                                           -64
canned jarred vegetables                    1  Diced Fire Roasted W/Garlic Tomatoes                                                    -66
prepared soups salads                       1  Tuna Salad                                                                              -50
juice nectars                               1  Cranberry Juice Cocktail                                                                -63
asian foods                                 1  Organic Seaweed Snack Sea Salt                                                          -64
granola                                     1  Healthy Grains Oats & Honey Clusters with Toasted Coconut Gluten Free                   -70
cookies cakes                               1  Fig Newmans Fruit Filled Cookies                                                        -71
hot cereal pancake mixes                    1  Instant Oatmeal Variety Pack                                                            -72
frozen dessert                              1  Gluten Free Chocolate Raspberry Whoopie Pies,                                           -53
juice nectars                               1  Natural Premium Coconut Water                                                           -63
asian foods                                 1  Organic Tamari Gluten-Free Soy Sauce                                                    -64
prepared meals                              1  Sweet Corn Tamales                                                                      -50
baby accessories                            1  Totz Toothbrush Extra Soft 18+ Months                                                   -42
vitamins supplements                        1  Standardized Elderberry Syrup Berry Flavor                                              -51
breakfast bars pastries                     1  BelVita Blueberry Breakfast Biscuit Packs                                               -59
frozen produce                              1  Health Berry Blend                                                                      -68
packaged produce                            1  Hass Avocados                                                                           -69
soy lactosefree                             1  Organic Unsweetened Vanilla Almond Milk                                                 -73
yogurt                                      1  Strawberry Rhubarb Yoghurt                                                              -75
milk                                        1  Organic Whole Milk                                                                      -76
cream                                       1  Half & Half                                                                             -77
beauty                                      1  Triple Size Cotton Balls                                                                -42
red wines                                   1  Merlot                                                                                  -47
butter                                      1  Original Spread                                                                         -57
tortillas flat bread                        1  6\" Yellow Corn Tortillas                                                               -62
packaged meat                               1  Organic Chicken Thighs                                                                  -50
packaged seafood                            1  Traditional Scottish Style Smoked Salmon                                                -52
plates bowls cups flatware                  1  9 Inch Plates                                                                           -64
specialty wines champagnes                  1  Rose                                                                                    -42
white wines                                 1  Sauvignon Blanc                                                                         -43
more household                              1  Soft White 60 Watts Halogen Bulb                                                        -58
canned meals beans                          1  Black Beans                                                                             -64
frozen breakfast                            1  Gluten Free Apple Cinnamon Waffles                                                      -66
fresh pasta                                 1  Mushroom Ravioli                                                                        -70
pickled goods olives                        1  Baby Dill Pickles                                                                       -72
pasta sauce                                 1  Tomato Paste                                                                            -64
air fresheners candles                      1  Air Effects With Gain Original Scent Air Freshener Spray                                -69
dish detergents                             1  Complete™ ActionPacs™ Fresh Scent Dishwasher Detergent                                  -70
frozen meals                                1  Cheddar Cheese Burrito                                                                  -71
crackers                                    1  Toasted Chips Garden Valley Veggie Wheat Thins                                          -73
chips pretzels                              1  Lightly Salted Baked Snap Pea Crisps                                                    -74
paper goods                                 1  Bathroom Tissue, Unscented, Double Rolls, 3-Ply                                         -75
diapers wipes                               1  Done Baby Wipes Cumber & Green Tea                                                      -77
fresh fruits                                1  Organic Avocado                                                                         -79
nuts seeds dried fruit                      1  Mixed Nuts Lightly Salted                                                               -80
lunch meat                                  1  Uncured Diced Pancetta                                                                  -63
instant foods                               1  Spanish Rice Pilaf Mix                                                                  -69
other creams cheeses                        1  Lowfat Small Curd Cottage Cheese                                                        -70
cocoa drink mixes                           1  Hot Cocoa Mix, Milk Chocolate Flavor                                                    -59
baking ingredients                          1  All-Purpose Flour                                                                       -68
frozen dessert                              1  Dark Chocolate Covered Banana                                                           -53
energy sports drinks                        1  Ice Sports Drink                                                                        -56
frozen vegan vegetarian                     1  Green Chili & Cumin Adzuki Bean Burger                                                  -58
spices seasonings                           1  Pure Ground Black Pepper                                                                -60
baking supplies decor                       1  Non-Stick Parchment Paper                                                               -69
cleaning products                           1  Disinfecting Toilet Wand Refills                                                        -70
ice cream toppings                          1  Organic Ice Cream Cones                                                                 -53
poultry counter                             1  Boneless Skinless Chicken Breast                                                        -52
doughs gelatins bake mixes                  1  Chocolate Chip Cookie Dough                                                             -60
poultry counter                             1  Air Chilled Organic Boneless Skinless Chicken Breasts                                   -52
bulk grains rice dried goods                1  Organic Short Brown Sprouted Rice                                                       -45
cat food care                               1  24/7 Performance Clumping Litter                                                        -55
cold flu allergy                            1  Sore Throat Plus Coating Protection Wild Berry Flavor Spray                             -45
mint gum                                    1  Wrigley's Orbit Wintermint Sugar Free Gum- 3 PK                                         -39
preserved dips spreads                      1  Organic Medium Salsa                                                                    -60
fruit vegetable snacks                      1  Mixed Fruit Fruit Snacks                                                                -65
frozen meat seafood                         1  Chipotle Black Bean Burger                                                              -67
hair care                                   1  Juicy Grape Conditioner                                                                 -68
water seltzer sparkling water               1  Original Sparkling Mountain Spring Water                                                -70
fresh herbs                                 1  Marjoram                                                                                -63
coffee                                      1  White Paper 8-12 Cups Basket Coffee Filters                                             -62
trail mix snack mix                         1  Organic Cheddar Snack Mix                                                               -55
spreads                                     1  Creamy Almond Butter                                                                    -61
bulk dried fruits vegetables                1  Organic Black Mission Figs                                                              -62
canned meals beans                          1  Organic Lentil Beans                                                                    -64
breakfast bakery                            1  Authentic French Chocolate Swirl Brioche                                                -68
frozen juice                                1  All Natural Smoothies Orange Dream Machine                                              -44
meat counter                                1  85% Lean Ground Beef                                                                    -60
kitchen supplies                            1  Wax Paper                                                                               -52
eggs                                        1  Hard-Boiled Eggs                                                                        -62
frozen pizza                                1  French Bread Pizza Pepperoni                                                            -59
salad dressing toppings                     1  Family Recipe Italian Dressing                                                          -70
condiments                                  1  Dijon Mustard                                                                           -72
marinades meat preparation                  1  Panko Italian Style Crispy Bread Crumbs                                                 -73
tea                                         1  Sweet Tea W/Real Sugar Iced Tea                                                         -74
dry pasta                                   1  Elbow Macaroni Pasta                                                                    -75
frozen appetizers sides                     1  Olive Oil Rosemary and Garlic Oven Fries                                                -76
fresh vegetables                            1  Asparation/Broccolini/Baby Broccoli                                                     -77
cereal                                      1  Raisin Bran Cereal                                                                      -78
popcorn jerky                               1  Aged White Cheddar Puffs Cheese Snacks                                                  -79
packaged vegetables fruits                  1  Fresh European Style Baby Spinach                                                       -80
baby bath body care                         1  Baby Naturals Head-to-Toe Baby Wash                                                     -52
soft drinks                                 1  Fridge Pack Cola                                                                        -60
indian foods                                1  Mild Major Grey Chutney                                                                 -52
first aid                                   1  Pain Relieving Patch - 5 CT                                                             -49
honeys syrups nectars                       1  Raw Local Wildflower Honey                                                              -55
baby food formula                           1  Organic Stage 3 Pear Carrot Apricot Baby Food                                           -64
trash bags liners                           1  Small Compostable Waste Bag                                                             -70
skin care                                   1  Indian Healing Clay                                                                     -71
fresh dips tapenades                        1  Pico De Gallo Chunky Salsa                                                              -72
canned fruit applesauce                     1  Bada Bing Pitted Cherries                                                               -73
yogurt                                      1  Yoghurt Blueberry                                                                       -75
refrigerated                                1  California Lemonade                                                                     -76
refrigerated pudding desserts               1  Organic Chocolate Almondmilk Pudding                                                    -49
latino foods                                1  Mild Traditional Enchilada Sauce                                                        -55
kosher foods                                1  Gluten Free Israeli Couscous                                                            -45
frozen vegan vegetarian                     1  California Veggie Burgers - 4 CT                                                        -58
deodorants                                  1  Clinical Protection Shower Clean Anti-Perspirant & Deodorant                            -46
seafood counter                             1  Coho Salmon                                                                             -52
digestion                                   1  Natural Raspberry Flavor Yum Yum Dophilus Probiotic                                     -52
eye ear care                                1  Purse Pack Cotton Swabs                                                                 -53
buns rolls                                  1  White Hamburger Buns                                                                    -57
shave needs                                 1  Organic Smooth & Moisturizing Shaving Gel, Lemongrass Lime                              -63
Comments

Each aisle has its own most popular items. For example, the most popular item in aisle dog food care is sausage cuts real beef treats. The order number of the most popular items in each aisle is around 60 to 80. 



Making table showing the mean hour of the day at which Pink Lady Apples and Coffee Ice Cream are ordered on each day of the week.

```r
instacart_2products = instacart %>%
  group_by(product_name,order_dow) %>% 
  filter(
    product_name %in%  c("Pink Lady Apples","Coffee Ice Cream")  
    )  %>% 
  summarize(
    mean_hour = mean(order_hour_of_day)
     
  ) %>% 
  pivot_wider (
    names_from = order_dow,
    values_from = mean_hour
  )
 
instacart_2products    
```

```
## # A tibble: 2 x 8
## # Groups:   product_name [2]
##   product_name       `0`   `1`   `2`   `3`   `4`   `5`   `6`
##   <chr>            <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>
## 1 Coffee Ice Cream  13.8  14.3  15.4  15.3  15.2  12.3  13.8
## 2 Pink Lady Apples  13.4  11.4  11.7  14.2  11.6  12.8  11.9
```

Comments
Coffee Ice Cream has its highest mean order hour of the day on order_dow 2(which is Monday), while Pink Lady Apples has its highest mean order hour of the day on order_dow 3 (which is Tuesday). 


## Problem 2

Load data from a package, and  do some data cleaning:


```r
library(p8105.datasets)
data("brfss_smart2010")

brfss_smart2010 = brfss_smart2010 %>% 
janitor::clean_names() %>% 
  rename (
    state = locationabbr ,
    county = locationdesc  
  ) %>% 
  filter(
    topic == "Overall Health", 
   response %in% c("Poor", "Fair", "Good", "Very good", "Excellent")
           ) %>% 
  mutate (
    response = factor(response),
    response = fct_relevel (response, "Poor", "Fair", "Good", "Very good", "Excellent")
  )
brfss_smart2010
```

```
## # A tibble: 10,625 x 23
##     year state county class topic question response sample_size data_value
##    <int> <chr> <chr>  <chr> <chr> <chr>    <fct>          <int>      <dbl>
##  1  2010 AL    AL - ~ Heal~ Over~ How is ~ Excelle~          94       18.9
##  2  2010 AL    AL - ~ Heal~ Over~ How is ~ Very go~         148       30  
##  3  2010 AL    AL - ~ Heal~ Over~ How is ~ Good             208       33.1
##  4  2010 AL    AL - ~ Heal~ Over~ How is ~ Fair             107       12.5
##  5  2010 AL    AL - ~ Heal~ Over~ How is ~ Poor              45        5.5
##  6  2010 AL    AL - ~ Heal~ Over~ How is ~ Excelle~          91       15.6
##  7  2010 AL    AL - ~ Heal~ Over~ How is ~ Very go~         177       31.3
##  8  2010 AL    AL - ~ Heal~ Over~ How is ~ Good             224       31.2
##  9  2010 AL    AL - ~ Heal~ Over~ How is ~ Fair             120       15.5
## 10  2010 AL    AL - ~ Heal~ Over~ How is ~ Poor              66        6.4
## # ... with 10,615 more rows, and 14 more variables:
## #   confidence_limit_low <dbl>, confidence_limit_high <dbl>,
## #   display_order <int>, data_value_unit <chr>, data_value_type <chr>,
## #   data_value_footnote_symbol <chr>, data_value_footnote <chr>,
## #   data_source <chr>, class_id <chr>, topic_id <chr>, location_id <chr>,
## #   question_id <chr>, respid <chr>, geo_location <chr>
```

In 2002 and 2010, finding the states that were observed at 7 or more locations.

```r
brfss_smart2010 %>% 
  group_by(state) %>% 
  filter(year == 2002) %>% 
  summarize (
    n_location = n_distinct(county)
  ) %>% 
    filter(n_location >= 7)
```

```
## # A tibble: 6 x 2
##   state n_location
##   <chr>      <int>
## 1 CT             7
## 2 FL             7
## 3 MA             8
## 4 NC             7
## 5 NJ             8
## 6 PA            10
```

```r
brfss_smart2010 %>% 
  group_by(state) %>% 
  filter(year == 2010) %>% 
  summarize (
    n_location = n_distinct(county)
  ) %>% 
    filter(n_location >= 7)
```

```
## # A tibble: 14 x 2
##    state n_location
##    <chr>      <int>
##  1 CA            12
##  2 CO             7
##  3 FL            41
##  4 MA             9
##  5 MD            12
##  6 NC            12
##  7 NE            10
##  8 NJ            19
##  9 NY             9
## 10 OH             8
## 11 PA             7
## 12 SC             7
## 13 TX            16
## 14 WA            10
```
Comments
In 2002, there are 6 states that were observed at 7 or more locations, while in 2010, there were 14. States including FL, MA, NC, NJ, PA were both observed in 2002 and 2010. 


Construct a dataset that is limited to Excellent responses, and contains, year, state, and a Make a “spaghetti” plot of this average value over time within a state. 


```r
dataset_prob2 = brfss_smart2010 %>% 
  group_by(state,year) %>% 
  filter (response == "Excellent") %>% 
  mutate (ave_data_value = mean (data_value)) %>% 
  select (year, state, ave_data_value) 

 dataset_prob2 %>%
  ggplot()+
  geom_line( aes (x = year, y = ave_data_value, color = state),alpha = .5)+
   labs (
     title = "State and Year",
     x = "Year",
     y = "Average value",
     caption = "Data from BRFSS"
   )+
   theme_bw()+
   theme(legend.position = "bottom") +
   viridis::scale_color_viridis(
    name = "state", 
    discrete = TRUE
  )
```

<img src="p8105_hw3_jf3286_for_Jingyu_files/figure-html/unnamed-chunk-9-1.png" width="90%" />
Comments

For most those with a "Excellent" response, the average data value changes severely over time. For very few of them, the average value stays somewhere between 20 and 25, and most of them are between 15 and 30. 


Make a two-panel plot showing, for the years 2006, and 2010, distribution of data_value for responses (“Poor” to “Excellent”) among locations in NY State.

```r
plot_of_distribution = brfss_smart2010 %>% 
  group_by(response,year) %>% 
  filter(state == "NY" & year %in% c("2006","2010")  & response %in% c("Poor","Excellent")) %>% 
  ggplot(aes(x = county, y = data_value, color = response))+
  geom_col(aes(fill = response))+
  labs (
     title = "Distribution of data value among locations in 2006",
     x = "Locations",
     y = "Data value",
     caption = "Data from BRFSS"
   )+
   theme_bw()+
   theme(legend.position = "bottom")+
  facet_grid (~ year)+
  viridis::scale_color_viridis(
    name = "response", 
    discrete = TRUE)

plot_of_distribution
```

<img src="p8105_hw3_jf3286_for_Jingyu_files/figure-html/unnamed-chunk-10-1.png" width="90%" />
Comments

Those with a "Poor" response have a much higher data value compared to those with "Excellent" response. 





## Problem 3
Load, tidy, and otherwise wrangle the data.


```r
accel_data = read_csv(file = "./accel_data.csv") %>% 
  janitor::clean_names() %>% 
  mutate(
    day_of_week = case_when( 
      day == "Monday" ~ "Weekday",
      day == "Tuesday" ~ "Weekday",
      day == "Wednesday" ~ "Weekday",
      day == "Thursday" ~ "Weekday",
      day == "Friday" ~ "Weekday",
      day == "Saturday" ~ "Weekend",
      day == "Sunday" ~ "Weekend",
      TRUE ~ ""
      )
           ) %>% 
  select(week, day_id, day, day_of_week,everything())
```

```
## Parsed with column specification:
## cols(
##   .default = col_double(),
##   day = col_character()
## )
```

```
## See spec(...) for full column specifications.
```
Desicription
This data includes variables such as week, day_id, day, day_of_week and activity. There are 'r count(accel_data, week)'obervations for week. 



Aggregate accross minutes to create a total activity variable for each day, and create a table showing these totals. 

```r
accel_data_tra = accel_data %>% 
  group_by (day_id) %>% 
  pivot_longer(
    activity_1:activity_64,
    names_to = "activity_number",
    names_prefix = "activity_",
    values_to = "activity_counts"
  ) %>% 
  mutate(
    activity_total = sum(activity_counts) 
  ) %>% 
  select (week, day_id, day, day_of_week, activity_total)  
  
 accel_data_tra %>% 
  knitr::kable()
```



 week   day_id  day         day_of_week    activity_total
-----  -------  ----------  ------------  ---------------
    1        1  Friday      Weekday              2593.311
    1        1  Friday      Weekday              2593.311
    1        1  Friday      Weekday              2593.311
    1        1  Friday      Weekday              2593.311
    1        1  Friday      Weekday              2593.311
    1        1  Friday      Weekday              2593.311
    1        1  Friday      Weekday              2593.311
    1        1  Friday      Weekday              2593.311
    1        1  Friday      Weekday              2593.311
    1        1  Friday      Weekday              2593.311
    1        1  Friday      Weekday              2593.311
    1        1  Friday      Weekday              2593.311
    1        1  Friday      Weekday              2593.311
    1        1  Friday      Weekday              2593.311
    1        1  Friday      Weekday              2593.311
    1        1  Friday      Weekday              2593.311
    1        1  Friday      Weekday              2593.311
    1        1  Friday      Weekday              2593.311
    1        1  Friday      Weekday              2593.311
    1        1  Friday      Weekday              2593.311
    1        1  Friday      Weekday              2593.311
    1        1  Friday      Weekday              2593.311
    1        1  Friday      Weekday              2593.311
    1        1  Friday      Weekday              2593.311
    1        1  Friday      Weekday              2593.311
    1        1  Friday      Weekday              2593.311
    1        1  Friday      Weekday              2593.311
    1        1  Friday      Weekday              2593.311
    1        1  Friday      Weekday              2593.311
    1        1  Friday      Weekday              2593.311
    1        1  Friday      Weekday              2593.311
    1        1  Friday      Weekday              2593.311
    1        1  Friday      Weekday              2593.311
    1        1  Friday      Weekday              2593.311
    1        1  Friday      Weekday              2593.311
    1        1  Friday      Weekday              2593.311
    1        1  Friday      Weekday              2593.311
    1        1  Friday      Weekday              2593.311
    1        1  Friday      Weekday              2593.311
    1        1  Friday      Weekday              2593.311
    1        1  Friday      Weekday              2593.311
    1        1  Friday      Weekday              2593.311
    1        1  Friday      Weekday              2593.311
    1        1  Friday      Weekday              2593.311
    1        1  Friday      Weekday              2593.311
    1        1  Friday      Weekday              2593.311
    1        1  Friday      Weekday              2593.311
    1        1  Friday      Weekday              2593.311
    1        1  Friday      Weekday              2593.311
    1        1  Friday      Weekday              2593.311
    1        1  Friday      Weekday              2593.311
    1        1  Friday      Weekday              2593.311
    1        1  Friday      Weekday              2593.311
    1        1  Friday      Weekday              2593.311
    1        1  Friday      Weekday              2593.311
    1        1  Friday      Weekday              2593.311
    1        1  Friday      Weekday              2593.311
    1        1  Friday      Weekday              2593.311
    1        1  Friday      Weekday              2593.311
    1        1  Friday      Weekday              2593.311
    1        1  Friday      Weekday              2593.311
    1        1  Friday      Weekday              2593.311
    1        1  Friday      Weekday              2593.311
    1        1  Friday      Weekday              2593.311
    1        2  Monday      Weekday                64.000
    1        2  Monday      Weekday                64.000
    1        2  Monday      Weekday                64.000
    1        2  Monday      Weekday                64.000
    1        2  Monday      Weekday                64.000
    1        2  Monday      Weekday                64.000
    1        2  Monday      Weekday                64.000
    1        2  Monday      Weekday                64.000
    1        2  Monday      Weekday                64.000
    1        2  Monday      Weekday                64.000
    1        2  Monday      Weekday                64.000
    1        2  Monday      Weekday                64.000
    1        2  Monday      Weekday                64.000
    1        2  Monday      Weekday                64.000
    1        2  Monday      Weekday                64.000
    1        2  Monday      Weekday                64.000
    1        2  Monday      Weekday                64.000
    1        2  Monday      Weekday                64.000
    1        2  Monday      Weekday                64.000
    1        2  Monday      Weekday                64.000
    1        2  Monday      Weekday                64.000
    1        2  Monday      Weekday                64.000
    1        2  Monday      Weekday                64.000
    1        2  Monday      Weekday                64.000
    1        2  Monday      Weekday                64.000
    1        2  Monday      Weekday                64.000
    1        2  Monday      Weekday                64.000
    1        2  Monday      Weekday                64.000
    1        2  Monday      Weekday                64.000
    1        2  Monday      Weekday                64.000
    1        2  Monday      Weekday                64.000
    1        2  Monday      Weekday                64.000
    1        2  Monday      Weekday                64.000
    1        2  Monday      Weekday                64.000
    1        2  Monday      Weekday                64.000
    1        2  Monday      Weekday                64.000
    1        2  Monday      Weekday                64.000
    1        2  Monday      Weekday                64.000
    1        2  Monday      Weekday                64.000
    1        2  Monday      Weekday                64.000
    1        2  Monday      Weekday                64.000
    1        2  Monday      Weekday                64.000
    1        2  Monday      Weekday                64.000
    1        2  Monday      Weekday                64.000
    1        2  Monday      Weekday                64.000
    1        2  Monday      Weekday                64.000
    1        2  Monday      Weekday                64.000
    1        2  Monday      Weekday                64.000
    1        2  Monday      Weekday                64.000
    1        2  Monday      Weekday                64.000
    1        2  Monday      Weekday                64.000
    1        2  Monday      Weekday                64.000
    1        2  Monday      Weekday                64.000
    1        2  Monday      Weekday                64.000
    1        2  Monday      Weekday                64.000
    1        2  Monday      Weekday                64.000
    1        2  Monday      Weekday                64.000
    1        2  Monday      Weekday                64.000
    1        2  Monday      Weekday                64.000
    1        2  Monday      Weekday                64.000
    1        2  Monday      Weekday                64.000
    1        2  Monday      Weekday                64.000
    1        2  Monday      Weekday                64.000
    1        2  Monday      Weekday                64.000
    1        3  Saturday    Weekend              4543.000
    1        3  Saturday    Weekend              4543.000
    1        3  Saturday    Weekend              4543.000
    1        3  Saturday    Weekend              4543.000
    1        3  Saturday    Weekend              4543.000
    1        3  Saturday    Weekend              4543.000
    1        3  Saturday    Weekend              4543.000
    1        3  Saturday    Weekend              4543.000
    1        3  Saturday    Weekend              4543.000
    1        3  Saturday    Weekend              4543.000
    1        3  Saturday    Weekend              4543.000
    1        3  Saturday    Weekend              4543.000
    1        3  Saturday    Weekend              4543.000
    1        3  Saturday    Weekend              4543.000
    1        3  Saturday    Weekend              4543.000
    1        3  Saturday    Weekend              4543.000
    1        3  Saturday    Weekend              4543.000
    1        3  Saturday    Weekend              4543.000
    1        3  Saturday    Weekend              4543.000
    1        3  Saturday    Weekend              4543.000
    1        3  Saturday    Weekend              4543.000
    1        3  Saturday    Weekend              4543.000
    1        3  Saturday    Weekend              4543.000
    1        3  Saturday    Weekend              4543.000
    1        3  Saturday    Weekend              4543.000
    1        3  Saturday    Weekend              4543.000
    1        3  Saturday    Weekend              4543.000
    1        3  Saturday    Weekend              4543.000
    1        3  Saturday    Weekend              4543.000
    1        3  Saturday    Weekend              4543.000
    1        3  Saturday    Weekend              4543.000
    1        3  Saturday    Weekend              4543.000
    1        3  Saturday    Weekend              4543.000
    1        3  Saturday    Weekend              4543.000
    1        3  Saturday    Weekend              4543.000
    1        3  Saturday    Weekend              4543.000
    1        3  Saturday    Weekend              4543.000
    1        3  Saturday    Weekend              4543.000
    1        3  Saturday    Weekend              4543.000
    1        3  Saturday    Weekend              4543.000
    1        3  Saturday    Weekend              4543.000
    1        3  Saturday    Weekend              4543.000
    1        3  Saturday    Weekend              4543.000
    1        3  Saturday    Weekend              4543.000
    1        3  Saturday    Weekend              4543.000
    1        3  Saturday    Weekend              4543.000
    1        3  Saturday    Weekend              4543.000
    1        3  Saturday    Weekend              4543.000
    1        3  Saturday    Weekend              4543.000
    1        3  Saturday    Weekend              4543.000
    1        3  Saturday    Weekend              4543.000
    1        3  Saturday    Weekend              4543.000
    1        3  Saturday    Weekend              4543.000
    1        3  Saturday    Weekend              4543.000
    1        3  Saturday    Weekend              4543.000
    1        3  Saturday    Weekend              4543.000
    1        3  Saturday    Weekend              4543.000
    1        3  Saturday    Weekend              4543.000
    1        3  Saturday    Weekend              4543.000
    1        3  Saturday    Weekend              4543.000
    1        3  Saturday    Weekend              4543.000
    1        3  Saturday    Weekend              4543.000
    1        3  Saturday    Weekend              4543.000
    1        3  Saturday    Weekend              4543.000
    1        4  Sunday      Weekend              1088.000
    1        4  Sunday      Weekend              1088.000
    1        4  Sunday      Weekend              1088.000
    1        4  Sunday      Weekend              1088.000
    1        4  Sunday      Weekend              1088.000
    1        4  Sunday      Weekend              1088.000
    1        4  Sunday      Weekend              1088.000
    1        4  Sunday      Weekend              1088.000
    1        4  Sunday      Weekend              1088.000
    1        4  Sunday      Weekend              1088.000
    1        4  Sunday      Weekend              1088.000
    1        4  Sunday      Weekend              1088.000
    1        4  Sunday      Weekend              1088.000
    1        4  Sunday      Weekend              1088.000
    1        4  Sunday      Weekend              1088.000
    1        4  Sunday      Weekend              1088.000
    1        4  Sunday      Weekend              1088.000
    1        4  Sunday      Weekend              1088.000
    1        4  Sunday      Weekend              1088.000
    1        4  Sunday      Weekend              1088.000
    1        4  Sunday      Weekend              1088.000
    1        4  Sunday      Weekend              1088.000
    1        4  Sunday      Weekend              1088.000
    1        4  Sunday      Weekend              1088.000
    1        4  Sunday      Weekend              1088.000
    1        4  Sunday      Weekend              1088.000
    1        4  Sunday      Weekend              1088.000
    1        4  Sunday      Weekend              1088.000
    1        4  Sunday      Weekend              1088.000
    1        4  Sunday      Weekend              1088.000
    1        4  Sunday      Weekend              1088.000
    1        4  Sunday      Weekend              1088.000
    1        4  Sunday      Weekend              1088.000
    1        4  Sunday      Weekend              1088.000
    1        4  Sunday      Weekend              1088.000
    1        4  Sunday      Weekend              1088.000
    1        4  Sunday      Weekend              1088.000
    1        4  Sunday      Weekend              1088.000
    1        4  Sunday      Weekend              1088.000
    1        4  Sunday      Weekend              1088.000
    1        4  Sunday      Weekend              1088.000
    1        4  Sunday      Weekend              1088.000
    1        4  Sunday      Weekend              1088.000
    1        4  Sunday      Weekend              1088.000
    1        4  Sunday      Weekend              1088.000
    1        4  Sunday      Weekend              1088.000
    1        4  Sunday      Weekend              1088.000
    1        4  Sunday      Weekend              1088.000
    1        4  Sunday      Weekend              1088.000
    1        4  Sunday      Weekend              1088.000
    1        4  Sunday      Weekend              1088.000
    1        4  Sunday      Weekend              1088.000
    1        4  Sunday      Weekend              1088.000
    1        4  Sunday      Weekend              1088.000
    1        4  Sunday      Weekend              1088.000
    1        4  Sunday      Weekend              1088.000
    1        4  Sunday      Weekend              1088.000
    1        4  Sunday      Weekend              1088.000
    1        4  Sunday      Weekend              1088.000
    1        4  Sunday      Weekend              1088.000
    1        4  Sunday      Weekend              1088.000
    1        4  Sunday      Weekend              1088.000
    1        4  Sunday      Weekend              1088.000
    1        4  Sunday      Weekend              1088.000
    1        5  Thursday    Weekday              2460.400
    1        5  Thursday    Weekday              2460.400
    1        5  Thursday    Weekday              2460.400
    1        5  Thursday    Weekday              2460.400
    1        5  Thursday    Weekday              2460.400
    1        5  Thursday    Weekday              2460.400
    1        5  Thursday    Weekday              2460.400
    1        5  Thursday    Weekday              2460.400
    1        5  Thursday    Weekday              2460.400
    1        5  Thursday    Weekday              2460.400
    1        5  Thursday    Weekday              2460.400
    1        5  Thursday    Weekday              2460.400
    1        5  Thursday    Weekday              2460.400
    1        5  Thursday    Weekday              2460.400
    1        5  Thursday    Weekday              2460.400
    1        5  Thursday    Weekday              2460.400
    1        5  Thursday    Weekday              2460.400
    1        5  Thursday    Weekday              2460.400
    1        5  Thursday    Weekday              2460.400
    1        5  Thursday    Weekday              2460.400
    1        5  Thursday    Weekday              2460.400
    1        5  Thursday    Weekday              2460.400
    1        5  Thursday    Weekday              2460.400
    1        5  Thursday    Weekday              2460.400
    1        5  Thursday    Weekday              2460.400
    1        5  Thursday    Weekday              2460.400
    1        5  Thursday    Weekday              2460.400
    1        5  Thursday    Weekday              2460.400
    1        5  Thursday    Weekday              2460.400
    1        5  Thursday    Weekday              2460.400
    1        5  Thursday    Weekday              2460.400
    1        5  Thursday    Weekday              2460.400
    1        5  Thursday    Weekday              2460.400
    1        5  Thursday    Weekday              2460.400
    1        5  Thursday    Weekday              2460.400
    1        5  Thursday    Weekday              2460.400
    1        5  Thursday    Weekday              2460.400
    1        5  Thursday    Weekday              2460.400
    1        5  Thursday    Weekday              2460.400
    1        5  Thursday    Weekday              2460.400
    1        5  Thursday    Weekday              2460.400
    1        5  Thursday    Weekday              2460.400
    1        5  Thursday    Weekday              2460.400
    1        5  Thursday    Weekday              2460.400
    1        5  Thursday    Weekday              2460.400
    1        5  Thursday    Weekday              2460.400
    1        5  Thursday    Weekday              2460.400
    1        5  Thursday    Weekday              2460.400
    1        5  Thursday    Weekday              2460.400
    1        5  Thursday    Weekday              2460.400
    1        5  Thursday    Weekday              2460.400
    1        5  Thursday    Weekday              2460.400
    1        5  Thursday    Weekday              2460.400
    1        5  Thursday    Weekday              2460.400
    1        5  Thursday    Weekday              2460.400
    1        5  Thursday    Weekday              2460.400
    1        5  Thursday    Weekday              2460.400
    1        5  Thursday    Weekday              2460.400
    1        5  Thursday    Weekday              2460.400
    1        5  Thursday    Weekday              2460.400
    1        5  Thursday    Weekday              2460.400
    1        5  Thursday    Weekday              2460.400
    1        5  Thursday    Weekday              2460.400
    1        5  Thursday    Weekday              2460.400
    1        6  Tuesday     Weekday              2880.000
    1        6  Tuesday     Weekday              2880.000
    1        6  Tuesday     Weekday              2880.000
    1        6  Tuesday     Weekday              2880.000
    1        6  Tuesday     Weekday              2880.000
    1        6  Tuesday     Weekday              2880.000
    1        6  Tuesday     Weekday              2880.000
    1        6  Tuesday     Weekday              2880.000
    1        6  Tuesday     Weekday              2880.000
    1        6  Tuesday     Weekday              2880.000
    1        6  Tuesday     Weekday              2880.000
    1        6  Tuesday     Weekday              2880.000
    1        6  Tuesday     Weekday              2880.000
    1        6  Tuesday     Weekday              2880.000
    1        6  Tuesday     Weekday              2880.000
    1        6  Tuesday     Weekday              2880.000
    1        6  Tuesday     Weekday              2880.000
    1        6  Tuesday     Weekday              2880.000
    1        6  Tuesday     Weekday              2880.000
    1        6  Tuesday     Weekday              2880.000
    1        6  Tuesday     Weekday              2880.000
    1        6  Tuesday     Weekday              2880.000
    1        6  Tuesday     Weekday              2880.000
    1        6  Tuesday     Weekday              2880.000
    1        6  Tuesday     Weekday              2880.000
    1        6  Tuesday     Weekday              2880.000
    1        6  Tuesday     Weekday              2880.000
    1        6  Tuesday     Weekday              2880.000
    1        6  Tuesday     Weekday              2880.000
    1        6  Tuesday     Weekday              2880.000
    1        6  Tuesday     Weekday              2880.000
    1        6  Tuesday     Weekday              2880.000
    1        6  Tuesday     Weekday              2880.000
    1        6  Tuesday     Weekday              2880.000
    1        6  Tuesday     Weekday              2880.000
    1        6  Tuesday     Weekday              2880.000
    1        6  Tuesday     Weekday              2880.000
    1        6  Tuesday     Weekday              2880.000
    1        6  Tuesday     Weekday              2880.000
    1        6  Tuesday     Weekday              2880.000
    1        6  Tuesday     Weekday              2880.000
    1        6  Tuesday     Weekday              2880.000
    1        6  Tuesday     Weekday              2880.000
    1        6  Tuesday     Weekday              2880.000
    1        6  Tuesday     Weekday              2880.000
    1        6  Tuesday     Weekday              2880.000
    1        6  Tuesday     Weekday              2880.000
    1        6  Tuesday     Weekday              2880.000
    1        6  Tuesday     Weekday              2880.000
    1        6  Tuesday     Weekday              2880.000
    1        6  Tuesday     Weekday              2880.000
    1        6  Tuesday     Weekday              2880.000
    1        6  Tuesday     Weekday              2880.000
    1        6  Tuesday     Weekday              2880.000
    1        6  Tuesday     Weekday              2880.000
    1        6  Tuesday     Weekday              2880.000
    1        6  Tuesday     Weekday              2880.000
    1        6  Tuesday     Weekday              2880.000
    1        6  Tuesday     Weekday              2880.000
    1        6  Tuesday     Weekday              2880.000
    1        6  Tuesday     Weekday              2880.000
    1        6  Tuesday     Weekday              2880.000
    1        6  Tuesday     Weekday              2880.000
    1        6  Tuesday     Weekday              2880.000
    1        7  Wednesday   Weekday              2222.267
    1        7  Wednesday   Weekday              2222.267
    1        7  Wednesday   Weekday              2222.267
    1        7  Wednesday   Weekday              2222.267
    1        7  Wednesday   Weekday              2222.267
    1        7  Wednesday   Weekday              2222.267
    1        7  Wednesday   Weekday              2222.267
    1        7  Wednesday   Weekday              2222.267
    1        7  Wednesday   Weekday              2222.267
    1        7  Wednesday   Weekday              2222.267
    1        7  Wednesday   Weekday              2222.267
    1        7  Wednesday   Weekday              2222.267
    1        7  Wednesday   Weekday              2222.267
    1        7  Wednesday   Weekday              2222.267
    1        7  Wednesday   Weekday              2222.267
    1        7  Wednesday   Weekday              2222.267
    1        7  Wednesday   Weekday              2222.267
    1        7  Wednesday   Weekday              2222.267
    1        7  Wednesday   Weekday              2222.267
    1        7  Wednesday   Weekday              2222.267
    1        7  Wednesday   Weekday              2222.267
    1        7  Wednesday   Weekday              2222.267
    1        7  Wednesday   Weekday              2222.267
    1        7  Wednesday   Weekday              2222.267
    1        7  Wednesday   Weekday              2222.267
    1        7  Wednesday   Weekday              2222.267
    1        7  Wednesday   Weekday              2222.267
    1        7  Wednesday   Weekday              2222.267
    1        7  Wednesday   Weekday              2222.267
    1        7  Wednesday   Weekday              2222.267
    1        7  Wednesday   Weekday              2222.267
    1        7  Wednesday   Weekday              2222.267
    1        7  Wednesday   Weekday              2222.267
    1        7  Wednesday   Weekday              2222.267
    1        7  Wednesday   Weekday              2222.267
    1        7  Wednesday   Weekday              2222.267
    1        7  Wednesday   Weekday              2222.267
    1        7  Wednesday   Weekday              2222.267
    1        7  Wednesday   Weekday              2222.267
    1        7  Wednesday   Weekday              2222.267
    1        7  Wednesday   Weekday              2222.267
    1        7  Wednesday   Weekday              2222.267
    1        7  Wednesday   Weekday              2222.267
    1        7  Wednesday   Weekday              2222.267
    1        7  Wednesday   Weekday              2222.267
    1        7  Wednesday   Weekday              2222.267
    1        7  Wednesday   Weekday              2222.267
    1        7  Wednesday   Weekday              2222.267
    1        7  Wednesday   Weekday              2222.267
    1        7  Wednesday   Weekday              2222.267
    1        7  Wednesday   Weekday              2222.267
    1        7  Wednesday   Weekday              2222.267
    1        7  Wednesday   Weekday              2222.267
    1        7  Wednesday   Weekday              2222.267
    1        7  Wednesday   Weekday              2222.267
    1        7  Wednesday   Weekday              2222.267
    1        7  Wednesday   Weekday              2222.267
    1        7  Wednesday   Weekday              2222.267
    1        7  Wednesday   Weekday              2222.267
    1        7  Wednesday   Weekday              2222.267
    1        7  Wednesday   Weekday              2222.267
    1        7  Wednesday   Weekday              2222.267
    1        7  Wednesday   Weekday              2222.267
    1        7  Wednesday   Weekday              2222.267
    2        8  Friday      Weekday              6953.000
    2        8  Friday      Weekday              6953.000
    2        8  Friday      Weekday              6953.000
    2        8  Friday      Weekday              6953.000
    2        8  Friday      Weekday              6953.000
    2        8  Friday      Weekday              6953.000
    2        8  Friday      Weekday              6953.000
    2        8  Friday      Weekday              6953.000
    2        8  Friday      Weekday              6953.000
    2        8  Friday      Weekday              6953.000
    2        8  Friday      Weekday              6953.000
    2        8  Friday      Weekday              6953.000
    2        8  Friday      Weekday              6953.000
    2        8  Friday      Weekday              6953.000
    2        8  Friday      Weekday              6953.000
    2        8  Friday      Weekday              6953.000
    2        8  Friday      Weekday              6953.000
    2        8  Friday      Weekday              6953.000
    2        8  Friday      Weekday              6953.000
    2        8  Friday      Weekday              6953.000
    2        8  Friday      Weekday              6953.000
    2        8  Friday      Weekday              6953.000
    2        8  Friday      Weekday              6953.000
    2        8  Friday      Weekday              6953.000
    2        8  Friday      Weekday              6953.000
    2        8  Friday      Weekday              6953.000
    2        8  Friday      Weekday              6953.000
    2        8  Friday      Weekday              6953.000
    2        8  Friday      Weekday              6953.000
    2        8  Friday      Weekday              6953.000
    2        8  Friday      Weekday              6953.000
    2        8  Friday      Weekday              6953.000
    2        8  Friday      Weekday              6953.000
    2        8  Friday      Weekday              6953.000
    2        8  Friday      Weekday              6953.000
    2        8  Friday      Weekday              6953.000
    2        8  Friday      Weekday              6953.000
    2        8  Friday      Weekday              6953.000
    2        8  Friday      Weekday              6953.000
    2        8  Friday      Weekday              6953.000
    2        8  Friday      Weekday              6953.000
    2        8  Friday      Weekday              6953.000
    2        8  Friday      Weekday              6953.000
    2        8  Friday      Weekday              6953.000
    2        8  Friday      Weekday              6953.000
    2        8  Friday      Weekday              6953.000
    2        8  Friday      Weekday              6953.000
    2        8  Friday      Weekday              6953.000
    2        8  Friday      Weekday              6953.000
    2        8  Friday      Weekday              6953.000
    2        8  Friday      Weekday              6953.000
    2        8  Friday      Weekday              6953.000
    2        8  Friday      Weekday              6953.000
    2        8  Friday      Weekday              6953.000
    2        8  Friday      Weekday              6953.000
    2        8  Friday      Weekday              6953.000
    2        8  Friday      Weekday              6953.000
    2        8  Friday      Weekday              6953.000
    2        8  Friday      Weekday              6953.000
    2        8  Friday      Weekday              6953.000
    2        8  Friday      Weekday              6953.000
    2        8  Friday      Weekday              6953.000
    2        8  Friday      Weekday              6953.000
    2        8  Friday      Weekday              6953.000
    2        9  Monday      Weekday              7535.000
    2        9  Monday      Weekday              7535.000
    2        9  Monday      Weekday              7535.000
    2        9  Monday      Weekday              7535.000
    2        9  Monday      Weekday              7535.000
    2        9  Monday      Weekday              7535.000
    2        9  Monday      Weekday              7535.000
    2        9  Monday      Weekday              7535.000
    2        9  Monday      Weekday              7535.000
    2        9  Monday      Weekday              7535.000
    2        9  Monday      Weekday              7535.000
    2        9  Monday      Weekday              7535.000
    2        9  Monday      Weekday              7535.000
    2        9  Monday      Weekday              7535.000
    2        9  Monday      Weekday              7535.000
    2        9  Monday      Weekday              7535.000
    2        9  Monday      Weekday              7535.000
    2        9  Monday      Weekday              7535.000
    2        9  Monday      Weekday              7535.000
    2        9  Monday      Weekday              7535.000
    2        9  Monday      Weekday              7535.000
    2        9  Monday      Weekday              7535.000
    2        9  Monday      Weekday              7535.000
    2        9  Monday      Weekday              7535.000
    2        9  Monday      Weekday              7535.000
    2        9  Monday      Weekday              7535.000
    2        9  Monday      Weekday              7535.000
    2        9  Monday      Weekday              7535.000
    2        9  Monday      Weekday              7535.000
    2        9  Monday      Weekday              7535.000
    2        9  Monday      Weekday              7535.000
    2        9  Monday      Weekday              7535.000
    2        9  Monday      Weekday              7535.000
    2        9  Monday      Weekday              7535.000
    2        9  Monday      Weekday              7535.000
    2        9  Monday      Weekday              7535.000
    2        9  Monday      Weekday              7535.000
    2        9  Monday      Weekday              7535.000
    2        9  Monday      Weekday              7535.000
    2        9  Monday      Weekday              7535.000
    2        9  Monday      Weekday              7535.000
    2        9  Monday      Weekday              7535.000
    2        9  Monday      Weekday              7535.000
    2        9  Monday      Weekday              7535.000
    2        9  Monday      Weekday              7535.000
    2        9  Monday      Weekday              7535.000
    2        9  Monday      Weekday              7535.000
    2        9  Monday      Weekday              7535.000
    2        9  Monday      Weekday              7535.000
    2        9  Monday      Weekday              7535.000
    2        9  Monday      Weekday              7535.000
    2        9  Monday      Weekday              7535.000
    2        9  Monday      Weekday              7535.000
    2        9  Monday      Weekday              7535.000
    2        9  Monday      Weekday              7535.000
    2        9  Monday      Weekday              7535.000
    2        9  Monday      Weekday              7535.000
    2        9  Monday      Weekday              7535.000
    2        9  Monday      Weekday              7535.000
    2        9  Monday      Weekday              7535.000
    2        9  Monday      Weekday              7535.000
    2        9  Monday      Weekday              7535.000
    2        9  Monday      Weekday              7535.000
    2        9  Monday      Weekday              7535.000
    2       10  Saturday    Weekend               562.000
    2       10  Saturday    Weekend               562.000
    2       10  Saturday    Weekend               562.000
    2       10  Saturday    Weekend               562.000
    2       10  Saturday    Weekend               562.000
    2       10  Saturday    Weekend               562.000
    2       10  Saturday    Weekend               562.000
    2       10  Saturday    Weekend               562.000
    2       10  Saturday    Weekend               562.000
    2       10  Saturday    Weekend               562.000
    2       10  Saturday    Weekend               562.000
    2       10  Saturday    Weekend               562.000
    2       10  Saturday    Weekend               562.000
    2       10  Saturday    Weekend               562.000
    2       10  Saturday    Weekend               562.000
    2       10  Saturday    Weekend               562.000
    2       10  Saturday    Weekend               562.000
    2       10  Saturday    Weekend               562.000
    2       10  Saturday    Weekend               562.000
    2       10  Saturday    Weekend               562.000
    2       10  Saturday    Weekend               562.000
    2       10  Saturday    Weekend               562.000
    2       10  Saturday    Weekend               562.000
    2       10  Saturday    Weekend               562.000
    2       10  Saturday    Weekend               562.000
    2       10  Saturday    Weekend               562.000
    2       10  Saturday    Weekend               562.000
    2       10  Saturday    Weekend               562.000
    2       10  Saturday    Weekend               562.000
    2       10  Saturday    Weekend               562.000
    2       10  Saturday    Weekend               562.000
    2       10  Saturday    Weekend               562.000
    2       10  Saturday    Weekend               562.000
    2       10  Saturday    Weekend               562.000
    2       10  Saturday    Weekend               562.000
    2       10  Saturday    Weekend               562.000
    2       10  Saturday    Weekend               562.000
    2       10  Saturday    Weekend               562.000
    2       10  Saturday    Weekend               562.000
    2       10  Saturday    Weekend               562.000
    2       10  Saturday    Weekend               562.000
    2       10  Saturday    Weekend               562.000
    2       10  Saturday    Weekend               562.000
    2       10  Saturday    Weekend               562.000
    2       10  Saturday    Weekend               562.000
    2       10  Saturday    Weekend               562.000
    2       10  Saturday    Weekend               562.000
    2       10  Saturday    Weekend               562.000
    2       10  Saturday    Weekend               562.000
    2       10  Saturday    Weekend               562.000
    2       10  Saturday    Weekend               562.000
    2       10  Saturday    Weekend               562.000
    2       10  Saturday    Weekend               562.000
    2       10  Saturday    Weekend               562.000
    2       10  Saturday    Weekend               562.000
    2       10  Saturday    Weekend               562.000
    2       10  Saturday    Weekend               562.000
    2       10  Saturday    Weekend               562.000
    2       10  Saturday    Weekend               562.000
    2       10  Saturday    Weekend               562.000
    2       10  Saturday    Weekend               562.000
    2       10  Saturday    Weekend               562.000
    2       10  Saturday    Weekend               562.000
    2       10  Saturday    Weekend               562.000
    2       11  Sunday      Weekend               728.000
    2       11  Sunday      Weekend               728.000
    2       11  Sunday      Weekend               728.000
    2       11  Sunday      Weekend               728.000
    2       11  Sunday      Weekend               728.000
    2       11  Sunday      Weekend               728.000
    2       11  Sunday      Weekend               728.000
    2       11  Sunday      Weekend               728.000
    2       11  Sunday      Weekend               728.000
    2       11  Sunday      Weekend               728.000
    2       11  Sunday      Weekend               728.000
    2       11  Sunday      Weekend               728.000
    2       11  Sunday      Weekend               728.000
    2       11  Sunday      Weekend               728.000
    2       11  Sunday      Weekend               728.000
    2       11  Sunday      Weekend               728.000
    2       11  Sunday      Weekend               728.000
    2       11  Sunday      Weekend               728.000
    2       11  Sunday      Weekend               728.000
    2       11  Sunday      Weekend               728.000
    2       11  Sunday      Weekend               728.000
    2       11  Sunday      Weekend               728.000
    2       11  Sunday      Weekend               728.000
    2       11  Sunday      Weekend               728.000
    2       11  Sunday      Weekend               728.000
    2       11  Sunday      Weekend               728.000
    2       11  Sunday      Weekend               728.000
    2       11  Sunday      Weekend               728.000
    2       11  Sunday      Weekend               728.000
    2       11  Sunday      Weekend               728.000
    2       11  Sunday      Weekend               728.000
    2       11  Sunday      Weekend               728.000
    2       11  Sunday      Weekend               728.000
    2       11  Sunday      Weekend               728.000
    2       11  Sunday      Weekend               728.000
    2       11  Sunday      Weekend               728.000
    2       11  Sunday      Weekend               728.000
    2       11  Sunday      Weekend               728.000
    2       11  Sunday      Weekend               728.000
    2       11  Sunday      Weekend               728.000
    2       11  Sunday      Weekend               728.000
    2       11  Sunday      Weekend               728.000
    2       11  Sunday      Weekend               728.000
    2       11  Sunday      Weekend               728.000
    2       11  Sunday      Weekend               728.000
    2       11  Sunday      Weekend               728.000
    2       11  Sunday      Weekend               728.000
    2       11  Sunday      Weekend               728.000
    2       11  Sunday      Weekend               728.000
    2       11  Sunday      Weekend               728.000
    2       11  Sunday      Weekend               728.000
    2       11  Sunday      Weekend               728.000
    2       11  Sunday      Weekend               728.000
    2       11  Sunday      Weekend               728.000
    2       11  Sunday      Weekend               728.000
    2       11  Sunday      Weekend               728.000
    2       11  Sunday      Weekend               728.000
    2       11  Sunday      Weekend               728.000
    2       11  Sunday      Weekend               728.000
    2       11  Sunday      Weekend               728.000
    2       11  Sunday      Weekend               728.000
    2       11  Sunday      Weekend               728.000
    2       11  Sunday      Weekend               728.000
    2       11  Sunday      Weekend               728.000
    2       12  Thursday    Weekday             11329.000
    2       12  Thursday    Weekday             11329.000
    2       12  Thursday    Weekday             11329.000
    2       12  Thursday    Weekday             11329.000
    2       12  Thursday    Weekday             11329.000
    2       12  Thursday    Weekday             11329.000
    2       12  Thursday    Weekday             11329.000
    2       12  Thursday    Weekday             11329.000
    2       12  Thursday    Weekday             11329.000
    2       12  Thursday    Weekday             11329.000
    2       12  Thursday    Weekday             11329.000
    2       12  Thursday    Weekday             11329.000
    2       12  Thursday    Weekday             11329.000
    2       12  Thursday    Weekday             11329.000
    2       12  Thursday    Weekday             11329.000
    2       12  Thursday    Weekday             11329.000
    2       12  Thursday    Weekday             11329.000
    2       12  Thursday    Weekday             11329.000
    2       12  Thursday    Weekday             11329.000
    2       12  Thursday    Weekday             11329.000
    2       12  Thursday    Weekday             11329.000
    2       12  Thursday    Weekday             11329.000
    2       12  Thursday    Weekday             11329.000
    2       12  Thursday    Weekday             11329.000
    2       12  Thursday    Weekday             11329.000
    2       12  Thursday    Weekday             11329.000
    2       12  Thursday    Weekday             11329.000
    2       12  Thursday    Weekday             11329.000
    2       12  Thursday    Weekday             11329.000
    2       12  Thursday    Weekday             11329.000
    2       12  Thursday    Weekday             11329.000
    2       12  Thursday    Weekday             11329.000
    2       12  Thursday    Weekday             11329.000
    2       12  Thursday    Weekday             11329.000
    2       12  Thursday    Weekday             11329.000
    2       12  Thursday    Weekday             11329.000
    2       12  Thursday    Weekday             11329.000
    2       12  Thursday    Weekday             11329.000
    2       12  Thursday    Weekday             11329.000
    2       12  Thursday    Weekday             11329.000
    2       12  Thursday    Weekday             11329.000
    2       12  Thursday    Weekday             11329.000
    2       12  Thursday    Weekday             11329.000
    2       12  Thursday    Weekday             11329.000
    2       12  Thursday    Weekday             11329.000
    2       12  Thursday    Weekday             11329.000
    2       12  Thursday    Weekday             11329.000
    2       12  Thursday    Weekday             11329.000
    2       12  Thursday    Weekday             11329.000
    2       12  Thursday    Weekday             11329.000
    2       12  Thursday    Weekday             11329.000
    2       12  Thursday    Weekday             11329.000
    2       12  Thursday    Weekday             11329.000
    2       12  Thursday    Weekday             11329.000
    2       12  Thursday    Weekday             11329.000
    2       12  Thursday    Weekday             11329.000
    2       12  Thursday    Weekday             11329.000
    2       12  Thursday    Weekday             11329.000
    2       12  Thursday    Weekday             11329.000
    2       12  Thursday    Weekday             11329.000
    2       12  Thursday    Weekday             11329.000
    2       12  Thursday    Weekday             11329.000
    2       12  Thursday    Weekday             11329.000
    2       12  Thursday    Weekday             11329.000
    2       13  Tuesday     Weekday               262.000
    2       13  Tuesday     Weekday               262.000
    2       13  Tuesday     Weekday               262.000
    2       13  Tuesday     Weekday               262.000
    2       13  Tuesday     Weekday               262.000
    2       13  Tuesday     Weekday               262.000
    2       13  Tuesday     Weekday               262.000
    2       13  Tuesday     Weekday               262.000
    2       13  Tuesday     Weekday               262.000
    2       13  Tuesday     Weekday               262.000
    2       13  Tuesday     Weekday               262.000
    2       13  Tuesday     Weekday               262.000
    2       13  Tuesday     Weekday               262.000
    2       13  Tuesday     Weekday               262.000
    2       13  Tuesday     Weekday               262.000
    2       13  Tuesday     Weekday               262.000
    2       13  Tuesday     Weekday               262.000
    2       13  Tuesday     Weekday               262.000
    2       13  Tuesday     Weekday               262.000
    2       13  Tuesday     Weekday               262.000
    2       13  Tuesday     Weekday               262.000
    2       13  Tuesday     Weekday               262.000
    2       13  Tuesday     Weekday               262.000
    2       13  Tuesday     Weekday               262.000
    2       13  Tuesday     Weekday               262.000
    2       13  Tuesday     Weekday               262.000
    2       13  Tuesday     Weekday               262.000
    2       13  Tuesday     Weekday               262.000
    2       13  Tuesday     Weekday               262.000
    2       13  Tuesday     Weekday               262.000
    2       13  Tuesday     Weekday               262.000
    2       13  Tuesday     Weekday               262.000
    2       13  Tuesday     Weekday               262.000
    2       13  Tuesday     Weekday               262.000
    2       13  Tuesday     Weekday               262.000
    2       13  Tuesday     Weekday               262.000
    2       13  Tuesday     Weekday               262.000
    2       13  Tuesday     Weekday               262.000
    2       13  Tuesday     Weekday               262.000
    2       13  Tuesday     Weekday               262.000
    2       13  Tuesday     Weekday               262.000
    2       13  Tuesday     Weekday               262.000
    2       13  Tuesday     Weekday               262.000
    2       13  Tuesday     Weekday               262.000
    2       13  Tuesday     Weekday               262.000
    2       13  Tuesday     Weekday               262.000
    2       13  Tuesday     Weekday               262.000
    2       13  Tuesday     Weekday               262.000
    2       13  Tuesday     Weekday               262.000
    2       13  Tuesday     Weekday               262.000
    2       13  Tuesday     Weekday               262.000
    2       13  Tuesday     Weekday               262.000
    2       13  Tuesday     Weekday               262.000
    2       13  Tuesday     Weekday               262.000
    2       13  Tuesday     Weekday               262.000
    2       13  Tuesday     Weekday               262.000
    2       13  Tuesday     Weekday               262.000
    2       13  Tuesday     Weekday               262.000
    2       13  Tuesday     Weekday               262.000
    2       13  Tuesday     Weekday               262.000
    2       13  Tuesday     Weekday               262.000
    2       13  Tuesday     Weekday               262.000
    2       13  Tuesday     Weekday               262.000
    2       13  Tuesday     Weekday               262.000
    2       14  Wednesday   Weekday               855.000
    2       14  Wednesday   Weekday               855.000
    2       14  Wednesday   Weekday               855.000
    2       14  Wednesday   Weekday               855.000
    2       14  Wednesday   Weekday               855.000
    2       14  Wednesday   Weekday               855.000
    2       14  Wednesday   Weekday               855.000
    2       14  Wednesday   Weekday               855.000
    2       14  Wednesday   Weekday               855.000
    2       14  Wednesday   Weekday               855.000
    2       14  Wednesday   Weekday               855.000
    2       14  Wednesday   Weekday               855.000
    2       14  Wednesday   Weekday               855.000
    2       14  Wednesday   Weekday               855.000
    2       14  Wednesday   Weekday               855.000
    2       14  Wednesday   Weekday               855.000
    2       14  Wednesday   Weekday               855.000
    2       14  Wednesday   Weekday               855.000
    2       14  Wednesday   Weekday               855.000
    2       14  Wednesday   Weekday               855.000
    2       14  Wednesday   Weekday               855.000
    2       14  Wednesday   Weekday               855.000
    2       14  Wednesday   Weekday               855.000
    2       14  Wednesday   Weekday               855.000
    2       14  Wednesday   Weekday               855.000
    2       14  Wednesday   Weekday               855.000
    2       14  Wednesday   Weekday               855.000
    2       14  Wednesday   Weekday               855.000
    2       14  Wednesday   Weekday               855.000
    2       14  Wednesday   Weekday               855.000
    2       14  Wednesday   Weekday               855.000
    2       14  Wednesday   Weekday               855.000
    2       14  Wednesday   Weekday               855.000
    2       14  Wednesday   Weekday               855.000
    2       14  Wednesday   Weekday               855.000
    2       14  Wednesday   Weekday               855.000
    2       14  Wednesday   Weekday               855.000
    2       14  Wednesday   Weekday               855.000
    2       14  Wednesday   Weekday               855.000
    2       14  Wednesday   Weekday               855.000
    2       14  Wednesday   Weekday               855.000
    2       14  Wednesday   Weekday               855.000
    2       14  Wednesday   Weekday               855.000
    2       14  Wednesday   Weekday               855.000
    2       14  Wednesday   Weekday               855.000
    2       14  Wednesday   Weekday               855.000
    2       14  Wednesday   Weekday               855.000
    2       14  Wednesday   Weekday               855.000
    2       14  Wednesday   Weekday               855.000
    2       14  Wednesday   Weekday               855.000
    2       14  Wednesday   Weekday               855.000
    2       14  Wednesday   Weekday               855.000
    2       14  Wednesday   Weekday               855.000
    2       14  Wednesday   Weekday               855.000
    2       14  Wednesday   Weekday               855.000
    2       14  Wednesday   Weekday               855.000
    2       14  Wednesday   Weekday               855.000
    2       14  Wednesday   Weekday               855.000
    2       14  Wednesday   Weekday               855.000
    2       14  Wednesday   Weekday               855.000
    2       14  Wednesday   Weekday               855.000
    2       14  Wednesday   Weekday               855.000
    2       14  Wednesday   Weekday               855.000
    2       14  Wednesday   Weekday               855.000
    3       15  Friday      Weekday               734.000
    3       15  Friday      Weekday               734.000
    3       15  Friday      Weekday               734.000
    3       15  Friday      Weekday               734.000
    3       15  Friday      Weekday               734.000
    3       15  Friday      Weekday               734.000
    3       15  Friday      Weekday               734.000
    3       15  Friday      Weekday               734.000
    3       15  Friday      Weekday               734.000
    3       15  Friday      Weekday               734.000
    3       15  Friday      Weekday               734.000
    3       15  Friday      Weekday               734.000
    3       15  Friday      Weekday               734.000
    3       15  Friday      Weekday               734.000
    3       15  Friday      Weekday               734.000
    3       15  Friday      Weekday               734.000
    3       15  Friday      Weekday               734.000
    3       15  Friday      Weekday               734.000
    3       15  Friday      Weekday               734.000
    3       15  Friday      Weekday               734.000
    3       15  Friday      Weekday               734.000
    3       15  Friday      Weekday               734.000
    3       15  Friday      Weekday               734.000
    3       15  Friday      Weekday               734.000
    3       15  Friday      Weekday               734.000
    3       15  Friday      Weekday               734.000
    3       15  Friday      Weekday               734.000
    3       15  Friday      Weekday               734.000
    3       15  Friday      Weekday               734.000
    3       15  Friday      Weekday               734.000
    3       15  Friday      Weekday               734.000
    3       15  Friday      Weekday               734.000
    3       15  Friday      Weekday               734.000
    3       15  Friday      Weekday               734.000
    3       15  Friday      Weekday               734.000
    3       15  Friday      Weekday               734.000
    3       15  Friday      Weekday               734.000
    3       15  Friday      Weekday               734.000
    3       15  Friday      Weekday               734.000
    3       15  Friday      Weekday               734.000
    3       15  Friday      Weekday               734.000
    3       15  Friday      Weekday               734.000
    3       15  Friday      Weekday               734.000
    3       15  Friday      Weekday               734.000
    3       15  Friday      Weekday               734.000
    3       15  Friday      Weekday               734.000
    3       15  Friday      Weekday               734.000
    3       15  Friday      Weekday               734.000
    3       15  Friday      Weekday               734.000
    3       15  Friday      Weekday               734.000
    3       15  Friday      Weekday               734.000
    3       15  Friday      Weekday               734.000
    3       15  Friday      Weekday               734.000
    3       15  Friday      Weekday               734.000
    3       15  Friday      Weekday               734.000
    3       15  Friday      Weekday               734.000
    3       15  Friday      Weekday               734.000
    3       15  Friday      Weekday               734.000
    3       15  Friday      Weekday               734.000
    3       15  Friday      Weekday               734.000
    3       15  Friday      Weekday               734.000
    3       15  Friday      Weekday               734.000
    3       15  Friday      Weekday               734.000
    3       15  Friday      Weekday               734.000
    3       16  Monday      Weekday              7851.000
    3       16  Monday      Weekday              7851.000
    3       16  Monday      Weekday              7851.000
    3       16  Monday      Weekday              7851.000
    3       16  Monday      Weekday              7851.000
    3       16  Monday      Weekday              7851.000
    3       16  Monday      Weekday              7851.000
    3       16  Monday      Weekday              7851.000
    3       16  Monday      Weekday              7851.000
    3       16  Monday      Weekday              7851.000
    3       16  Monday      Weekday              7851.000
    3       16  Monday      Weekday              7851.000
    3       16  Monday      Weekday              7851.000
    3       16  Monday      Weekday              7851.000
    3       16  Monday      Weekday              7851.000
    3       16  Monday      Weekday              7851.000
    3       16  Monday      Weekday              7851.000
    3       16  Monday      Weekday              7851.000
    3       16  Monday      Weekday              7851.000
    3       16  Monday      Weekday              7851.000
    3       16  Monday      Weekday              7851.000
    3       16  Monday      Weekday              7851.000
    3       16  Monday      Weekday              7851.000
    3       16  Monday      Weekday              7851.000
    3       16  Monday      Weekday              7851.000
    3       16  Monday      Weekday              7851.000
    3       16  Monday      Weekday              7851.000
    3       16  Monday      Weekday              7851.000
    3       16  Monday      Weekday              7851.000
    3       16  Monday      Weekday              7851.000
    3       16  Monday      Weekday              7851.000
    3       16  Monday      Weekday              7851.000
    3       16  Monday      Weekday              7851.000
    3       16  Monday      Weekday              7851.000
    3       16  Monday      Weekday              7851.000
    3       16  Monday      Weekday              7851.000
    3       16  Monday      Weekday              7851.000
    3       16  Monday      Weekday              7851.000
    3       16  Monday      Weekday              7851.000
    3       16  Monday      Weekday              7851.000
    3       16  Monday      Weekday              7851.000
    3       16  Monday      Weekday              7851.000
    3       16  Monday      Weekday              7851.000
    3       16  Monday      Weekday              7851.000
    3       16  Monday      Weekday              7851.000
    3       16  Monday      Weekday              7851.000
    3       16  Monday      Weekday              7851.000
    3       16  Monday      Weekday              7851.000
    3       16  Monday      Weekday              7851.000
    3       16  Monday      Weekday              7851.000
    3       16  Monday      Weekday              7851.000
    3       16  Monday      Weekday              7851.000
    3       16  Monday      Weekday              7851.000
    3       16  Monday      Weekday              7851.000
    3       16  Monday      Weekday              7851.000
    3       16  Monday      Weekday              7851.000
    3       16  Monday      Weekday              7851.000
    3       16  Monday      Weekday              7851.000
    3       16  Monday      Weekday              7851.000
    3       16  Monday      Weekday              7851.000
    3       16  Monday      Weekday              7851.000
    3       16  Monday      Weekday              7851.000
    3       16  Monday      Weekday              7851.000
    3       16  Monday      Weekday              7851.000
    3       17  Saturday    Weekend              1759.000
    3       17  Saturday    Weekend              1759.000
    3       17  Saturday    Weekend              1759.000
    3       17  Saturday    Weekend              1759.000
    3       17  Saturday    Weekend              1759.000
    3       17  Saturday    Weekend              1759.000
    3       17  Saturday    Weekend              1759.000
    3       17  Saturday    Weekend              1759.000
    3       17  Saturday    Weekend              1759.000
    3       17  Saturday    Weekend              1759.000
    3       17  Saturday    Weekend              1759.000
    3       17  Saturday    Weekend              1759.000
    3       17  Saturday    Weekend              1759.000
    3       17  Saturday    Weekend              1759.000
    3       17  Saturday    Weekend              1759.000
    3       17  Saturday    Weekend              1759.000
    3       17  Saturday    Weekend              1759.000
    3       17  Saturday    Weekend              1759.000
    3       17  Saturday    Weekend              1759.000
    3       17  Saturday    Weekend              1759.000
    3       17  Saturday    Weekend              1759.000
    3       17  Saturday    Weekend              1759.000
    3       17  Saturday    Weekend              1759.000
    3       17  Saturday    Weekend              1759.000
    3       17  Saturday    Weekend              1759.000
    3       17  Saturday    Weekend              1759.000
    3       17  Saturday    Weekend              1759.000
    3       17  Saturday    Weekend              1759.000
    3       17  Saturday    Weekend              1759.000
    3       17  Saturday    Weekend              1759.000
    3       17  Saturday    Weekend              1759.000
    3       17  Saturday    Weekend              1759.000
    3       17  Saturday    Weekend              1759.000
    3       17  Saturday    Weekend              1759.000
    3       17  Saturday    Weekend              1759.000
    3       17  Saturday    Weekend              1759.000
    3       17  Saturday    Weekend              1759.000
    3       17  Saturday    Weekend              1759.000
    3       17  Saturday    Weekend              1759.000
    3       17  Saturday    Weekend              1759.000
    3       17  Saturday    Weekend              1759.000
    3       17  Saturday    Weekend              1759.000
    3       17  Saturday    Weekend              1759.000
    3       17  Saturday    Weekend              1759.000
    3       17  Saturday    Weekend              1759.000
    3       17  Saturday    Weekend              1759.000
    3       17  Saturday    Weekend              1759.000
    3       17  Saturday    Weekend              1759.000
    3       17  Saturday    Weekend              1759.000
    3       17  Saturday    Weekend              1759.000
    3       17  Saturday    Weekend              1759.000
    3       17  Saturday    Weekend              1759.000
    3       17  Saturday    Weekend              1759.000
    3       17  Saturday    Weekend              1759.000
    3       17  Saturday    Weekend              1759.000
    3       17  Saturday    Weekend              1759.000
    3       17  Saturday    Weekend              1759.000
    3       17  Saturday    Weekend              1759.000
    3       17  Saturday    Weekend              1759.000
    3       17  Saturday    Weekend              1759.000
    3       17  Saturday    Weekend              1759.000
    3       17  Saturday    Weekend              1759.000
    3       17  Saturday    Weekend              1759.000
    3       17  Saturday    Weekend              1759.000
    3       18  Sunday      Weekend              5889.000
    3       18  Sunday      Weekend              5889.000
    3       18  Sunday      Weekend              5889.000
    3       18  Sunday      Weekend              5889.000
    3       18  Sunday      Weekend              5889.000
    3       18  Sunday      Weekend              5889.000
    3       18  Sunday      Weekend              5889.000
    3       18  Sunday      Weekend              5889.000
    3       18  Sunday      Weekend              5889.000
    3       18  Sunday      Weekend              5889.000
    3       18  Sunday      Weekend              5889.000
    3       18  Sunday      Weekend              5889.000
    3       18  Sunday      Weekend              5889.000
    3       18  Sunday      Weekend              5889.000
    3       18  Sunday      Weekend              5889.000
    3       18  Sunday      Weekend              5889.000
    3       18  Sunday      Weekend              5889.000
    3       18  Sunday      Weekend              5889.000
    3       18  Sunday      Weekend              5889.000
    3       18  Sunday      Weekend              5889.000
    3       18  Sunday      Weekend              5889.000
    3       18  Sunday      Weekend              5889.000
    3       18  Sunday      Weekend              5889.000
    3       18  Sunday      Weekend              5889.000
    3       18  Sunday      Weekend              5889.000
    3       18  Sunday      Weekend              5889.000
    3       18  Sunday      Weekend              5889.000
    3       18  Sunday      Weekend              5889.000
    3       18  Sunday      Weekend              5889.000
    3       18  Sunday      Weekend              5889.000
    3       18  Sunday      Weekend              5889.000
    3       18  Sunday      Weekend              5889.000
    3       18  Sunday      Weekend              5889.000
    3       18  Sunday      Weekend              5889.000
    3       18  Sunday      Weekend              5889.000
    3       18  Sunday      Weekend              5889.000
    3       18  Sunday      Weekend              5889.000
    3       18  Sunday      Weekend              5889.000
    3       18  Sunday      Weekend              5889.000
    3       18  Sunday      Weekend              5889.000
    3       18  Sunday      Weekend              5889.000
    3       18  Sunday      Weekend              5889.000
    3       18  Sunday      Weekend              5889.000
    3       18  Sunday      Weekend              5889.000
    3       18  Sunday      Weekend              5889.000
    3       18  Sunday      Weekend              5889.000
    3       18  Sunday      Weekend              5889.000
    3       18  Sunday      Weekend              5889.000
    3       18  Sunday      Weekend              5889.000
    3       18  Sunday      Weekend              5889.000
    3       18  Sunday      Weekend              5889.000
    3       18  Sunday      Weekend              5889.000
    3       18  Sunday      Weekend              5889.000
    3       18  Sunday      Weekend              5889.000
    3       18  Sunday      Weekend              5889.000
    3       18  Sunday      Weekend              5889.000
    3       18  Sunday      Weekend              5889.000
    3       18  Sunday      Weekend              5889.000
    3       18  Sunday      Weekend              5889.000
    3       18  Sunday      Weekend              5889.000
    3       18  Sunday      Weekend              5889.000
    3       18  Sunday      Weekend              5889.000
    3       18  Sunday      Weekend              5889.000
    3       18  Sunday      Weekend              5889.000
    3       19  Thursday    Weekday              5925.000
    3       19  Thursday    Weekday              5925.000
    3       19  Thursday    Weekday              5925.000
    3       19  Thursday    Weekday              5925.000
    3       19  Thursday    Weekday              5925.000
    3       19  Thursday    Weekday              5925.000
    3       19  Thursday    Weekday              5925.000
    3       19  Thursday    Weekday              5925.000
    3       19  Thursday    Weekday              5925.000
    3       19  Thursday    Weekday              5925.000
    3       19  Thursday    Weekday              5925.000
    3       19  Thursday    Weekday              5925.000
    3       19  Thursday    Weekday              5925.000
    3       19  Thursday    Weekday              5925.000
    3       19  Thursday    Weekday              5925.000
    3       19  Thursday    Weekday              5925.000
    3       19  Thursday    Weekday              5925.000
    3       19  Thursday    Weekday              5925.000
    3       19  Thursday    Weekday              5925.000
    3       19  Thursday    Weekday              5925.000
    3       19  Thursday    Weekday              5925.000
    3       19  Thursday    Weekday              5925.000
    3       19  Thursday    Weekday              5925.000
    3       19  Thursday    Weekday              5925.000
    3       19  Thursday    Weekday              5925.000
    3       19  Thursday    Weekday              5925.000
    3       19  Thursday    Weekday              5925.000
    3       19  Thursday    Weekday              5925.000
    3       19  Thursday    Weekday              5925.000
    3       19  Thursday    Weekday              5925.000
    3       19  Thursday    Weekday              5925.000
    3       19  Thursday    Weekday              5925.000
    3       19  Thursday    Weekday              5925.000
    3       19  Thursday    Weekday              5925.000
    3       19  Thursday    Weekday              5925.000
    3       19  Thursday    Weekday              5925.000
    3       19  Thursday    Weekday              5925.000
    3       19  Thursday    Weekday              5925.000
    3       19  Thursday    Weekday              5925.000
    3       19  Thursday    Weekday              5925.000
    3       19  Thursday    Weekday              5925.000
    3       19  Thursday    Weekday              5925.000
    3       19  Thursday    Weekday              5925.000
    3       19  Thursday    Weekday              5925.000
    3       19  Thursday    Weekday              5925.000
    3       19  Thursday    Weekday              5925.000
    3       19  Thursday    Weekday              5925.000
    3       19  Thursday    Weekday              5925.000
    3       19  Thursday    Weekday              5925.000
    3       19  Thursday    Weekday              5925.000
    3       19  Thursday    Weekday              5925.000
    3       19  Thursday    Weekday              5925.000
    3       19  Thursday    Weekday              5925.000
    3       19  Thursday    Weekday              5925.000
    3       19  Thursday    Weekday              5925.000
    3       19  Thursday    Weekday              5925.000
    3       19  Thursday    Weekday              5925.000
    3       19  Thursday    Weekday              5925.000
    3       19  Thursday    Weekday              5925.000
    3       19  Thursday    Weekday              5925.000
    3       19  Thursday    Weekday              5925.000
    3       19  Thursday    Weekday              5925.000
    3       19  Thursday    Weekday              5925.000
    3       19  Thursday    Weekday              5925.000
    3       20  Tuesday     Weekday               473.000
    3       20  Tuesday     Weekday               473.000
    3       20  Tuesday     Weekday               473.000
    3       20  Tuesday     Weekday               473.000
    3       20  Tuesday     Weekday               473.000
    3       20  Tuesday     Weekday               473.000
    3       20  Tuesday     Weekday               473.000
    3       20  Tuesday     Weekday               473.000
    3       20  Tuesday     Weekday               473.000
    3       20  Tuesday     Weekday               473.000
    3       20  Tuesday     Weekday               473.000
    3       20  Tuesday     Weekday               473.000
    3       20  Tuesday     Weekday               473.000
    3       20  Tuesday     Weekday               473.000
    3       20  Tuesday     Weekday               473.000
    3       20  Tuesday     Weekday               473.000
    3       20  Tuesday     Weekday               473.000
    3       20  Tuesday     Weekday               473.000
    3       20  Tuesday     Weekday               473.000
    3       20  Tuesday     Weekday               473.000
    3       20  Tuesday     Weekday               473.000
    3       20  Tuesday     Weekday               473.000
    3       20  Tuesday     Weekday               473.000
    3       20  Tuesday     Weekday               473.000
    3       20  Tuesday     Weekday               473.000
    3       20  Tuesday     Weekday               473.000
    3       20  Tuesday     Weekday               473.000
    3       20  Tuesday     Weekday               473.000
    3       20  Tuesday     Weekday               473.000
    3       20  Tuesday     Weekday               473.000
    3       20  Tuesday     Weekday               473.000
    3       20  Tuesday     Weekday               473.000
    3       20  Tuesday     Weekday               473.000
    3       20  Tuesday     Weekday               473.000
    3       20  Tuesday     Weekday               473.000
    3       20  Tuesday     Weekday               473.000
    3       20  Tuesday     Weekday               473.000
    3       20  Tuesday     Weekday               473.000
    3       20  Tuesday     Weekday               473.000
    3       20  Tuesday     Weekday               473.000
    3       20  Tuesday     Weekday               473.000
    3       20  Tuesday     Weekday               473.000
    3       20  Tuesday     Weekday               473.000
    3       20  Tuesday     Weekday               473.000
    3       20  Tuesday     Weekday               473.000
    3       20  Tuesday     Weekday               473.000
    3       20  Tuesday     Weekday               473.000
    3       20  Tuesday     Weekday               473.000
    3       20  Tuesday     Weekday               473.000
    3       20  Tuesday     Weekday               473.000
    3       20  Tuesday     Weekday               473.000
    3       20  Tuesday     Weekday               473.000
    3       20  Tuesday     Weekday               473.000
    3       20  Tuesday     Weekday               473.000
    3       20  Tuesday     Weekday               473.000
    3       20  Tuesday     Weekday               473.000
    3       20  Tuesday     Weekday               473.000
    3       20  Tuesday     Weekday               473.000
    3       20  Tuesday     Weekday               473.000
    3       20  Tuesday     Weekday               473.000
    3       20  Tuesday     Weekday               473.000
    3       20  Tuesday     Weekday               473.000
    3       20  Tuesday     Weekday               473.000
    3       20  Tuesday     Weekday               473.000
    3       21  Wednesday   Weekday               335.000
    3       21  Wednesday   Weekday               335.000
    3       21  Wednesday   Weekday               335.000
    3       21  Wednesday   Weekday               335.000
    3       21  Wednesday   Weekday               335.000
    3       21  Wednesday   Weekday               335.000
    3       21  Wednesday   Weekday               335.000
    3       21  Wednesday   Weekday               335.000
    3       21  Wednesday   Weekday               335.000
    3       21  Wednesday   Weekday               335.000
    3       21  Wednesday   Weekday               335.000
    3       21  Wednesday   Weekday               335.000
    3       21  Wednesday   Weekday               335.000
    3       21  Wednesday   Weekday               335.000
    3       21  Wednesday   Weekday               335.000
    3       21  Wednesday   Weekday               335.000
    3       21  Wednesday   Weekday               335.000
    3       21  Wednesday   Weekday               335.000
    3       21  Wednesday   Weekday               335.000
    3       21  Wednesday   Weekday               335.000
    3       21  Wednesday   Weekday               335.000
    3       21  Wednesday   Weekday               335.000
    3       21  Wednesday   Weekday               335.000
    3       21  Wednesday   Weekday               335.000
    3       21  Wednesday   Weekday               335.000
    3       21  Wednesday   Weekday               335.000
    3       21  Wednesday   Weekday               335.000
    3       21  Wednesday   Weekday               335.000
    3       21  Wednesday   Weekday               335.000
    3       21  Wednesday   Weekday               335.000
    3       21  Wednesday   Weekday               335.000
    3       21  Wednesday   Weekday               335.000
    3       21  Wednesday   Weekday               335.000
    3       21  Wednesday   Weekday               335.000
    3       21  Wednesday   Weekday               335.000
    3       21  Wednesday   Weekday               335.000
    3       21  Wednesday   Weekday               335.000
    3       21  Wednesday   Weekday               335.000
    3       21  Wednesday   Weekday               335.000
    3       21  Wednesday   Weekday               335.000
    3       21  Wednesday   Weekday               335.000
    3       21  Wednesday   Weekday               335.000
    3       21  Wednesday   Weekday               335.000
    3       21  Wednesday   Weekday               335.000
    3       21  Wednesday   Weekday               335.000
    3       21  Wednesday   Weekday               335.000
    3       21  Wednesday   Weekday               335.000
    3       21  Wednesday   Weekday               335.000
    3       21  Wednesday   Weekday               335.000
    3       21  Wednesday   Weekday               335.000
    3       21  Wednesday   Weekday               335.000
    3       21  Wednesday   Weekday               335.000
    3       21  Wednesday   Weekday               335.000
    3       21  Wednesday   Weekday               335.000
    3       21  Wednesday   Weekday               335.000
    3       21  Wednesday   Weekday               335.000
    3       21  Wednesday   Weekday               335.000
    3       21  Wednesday   Weekday               335.000
    3       21  Wednesday   Weekday               335.000
    3       21  Wednesday   Weekday               335.000
    3       21  Wednesday   Weekday               335.000
    3       21  Wednesday   Weekday               335.000
    3       21  Wednesday   Weekday               335.000
    3       21  Wednesday   Weekday               335.000
    4       22  Friday      Weekday                64.000
    4       22  Friday      Weekday                64.000
    4       22  Friday      Weekday                64.000
    4       22  Friday      Weekday                64.000
    4       22  Friday      Weekday                64.000
    4       22  Friday      Weekday                64.000
    4       22  Friday      Weekday                64.000
    4       22  Friday      Weekday                64.000
    4       22  Friday      Weekday                64.000
    4       22  Friday      Weekday                64.000
    4       22  Friday      Weekday                64.000
    4       22  Friday      Weekday                64.000
    4       22  Friday      Weekday                64.000
    4       22  Friday      Weekday                64.000
    4       22  Friday      Weekday                64.000
    4       22  Friday      Weekday                64.000
    4       22  Friday      Weekday                64.000
    4       22  Friday      Weekday                64.000
    4       22  Friday      Weekday                64.000
    4       22  Friday      Weekday                64.000
    4       22  Friday      Weekday                64.000
    4       22  Friday      Weekday                64.000
    4       22  Friday      Weekday                64.000
    4       22  Friday      Weekday                64.000
    4       22  Friday      Weekday                64.000
    4       22  Friday      Weekday                64.000
    4       22  Friday      Weekday                64.000
    4       22  Friday      Weekday                64.000
    4       22  Friday      Weekday                64.000
    4       22  Friday      Weekday                64.000
    4       22  Friday      Weekday                64.000
    4       22  Friday      Weekday                64.000
    4       22  Friday      Weekday                64.000
    4       22  Friday      Weekday                64.000
    4       22  Friday      Weekday                64.000
    4       22  Friday      Weekday                64.000
    4       22  Friday      Weekday                64.000
    4       22  Friday      Weekday                64.000
    4       22  Friday      Weekday                64.000
    4       22  Friday      Weekday                64.000
    4       22  Friday      Weekday                64.000
    4       22  Friday      Weekday                64.000
    4       22  Friday      Weekday                64.000
    4       22  Friday      Weekday                64.000
    4       22  Friday      Weekday                64.000
    4       22  Friday      Weekday                64.000
    4       22  Friday      Weekday                64.000
    4       22  Friday      Weekday                64.000
    4       22  Friday      Weekday                64.000
    4       22  Friday      Weekday                64.000
    4       22  Friday      Weekday                64.000
    4       22  Friday      Weekday                64.000
    4       22  Friday      Weekday                64.000
    4       22  Friday      Weekday                64.000
    4       22  Friday      Weekday                64.000
    4       22  Friday      Weekday                64.000
    4       22  Friday      Weekday                64.000
    4       22  Friday      Weekday                64.000
    4       22  Friday      Weekday                64.000
    4       22  Friday      Weekday                64.000
    4       22  Friday      Weekday                64.000
    4       22  Friday      Weekday                64.000
    4       22  Friday      Weekday                64.000
    4       22  Friday      Weekday                64.000
    4       23  Monday      Weekday              1367.000
    4       23  Monday      Weekday              1367.000
    4       23  Monday      Weekday              1367.000
    4       23  Monday      Weekday              1367.000
    4       23  Monday      Weekday              1367.000
    4       23  Monday      Weekday              1367.000
    4       23  Monday      Weekday              1367.000
    4       23  Monday      Weekday              1367.000
    4       23  Monday      Weekday              1367.000
    4       23  Monday      Weekday              1367.000
    4       23  Monday      Weekday              1367.000
    4       23  Monday      Weekday              1367.000
    4       23  Monday      Weekday              1367.000
    4       23  Monday      Weekday              1367.000
    4       23  Monday      Weekday              1367.000
    4       23  Monday      Weekday              1367.000
    4       23  Monday      Weekday              1367.000
    4       23  Monday      Weekday              1367.000
    4       23  Monday      Weekday              1367.000
    4       23  Monday      Weekday              1367.000
    4       23  Monday      Weekday              1367.000
    4       23  Monday      Weekday              1367.000
    4       23  Monday      Weekday              1367.000
    4       23  Monday      Weekday              1367.000
    4       23  Monday      Weekday              1367.000
    4       23  Monday      Weekday              1367.000
    4       23  Monday      Weekday              1367.000
    4       23  Monday      Weekday              1367.000
    4       23  Monday      Weekday              1367.000
    4       23  Monday      Weekday              1367.000
    4       23  Monday      Weekday              1367.000
    4       23  Monday      Weekday              1367.000
    4       23  Monday      Weekday              1367.000
    4       23  Monday      Weekday              1367.000
    4       23  Monday      Weekday              1367.000
    4       23  Monday      Weekday              1367.000
    4       23  Monday      Weekday              1367.000
    4       23  Monday      Weekday              1367.000
    4       23  Monday      Weekday              1367.000
    4       23  Monday      Weekday              1367.000
    4       23  Monday      Weekday              1367.000
    4       23  Monday      Weekday              1367.000
    4       23  Monday      Weekday              1367.000
    4       23  Monday      Weekday              1367.000
    4       23  Monday      Weekday              1367.000
    4       23  Monday      Weekday              1367.000
    4       23  Monday      Weekday              1367.000
    4       23  Monday      Weekday              1367.000
    4       23  Monday      Weekday              1367.000
    4       23  Monday      Weekday              1367.000
    4       23  Monday      Weekday              1367.000
    4       23  Monday      Weekday              1367.000
    4       23  Monday      Weekday              1367.000
    4       23  Monday      Weekday              1367.000
    4       23  Monday      Weekday              1367.000
    4       23  Monday      Weekday              1367.000
    4       23  Monday      Weekday              1367.000
    4       23  Monday      Weekday              1367.000
    4       23  Monday      Weekday              1367.000
    4       23  Monday      Weekday              1367.000
    4       23  Monday      Weekday              1367.000
    4       23  Monday      Weekday              1367.000
    4       23  Monday      Weekday              1367.000
    4       23  Monday      Weekday              1367.000
    4       24  Saturday    Weekend                64.000
    4       24  Saturday    Weekend                64.000
    4       24  Saturday    Weekend                64.000
    4       24  Saturday    Weekend                64.000
    4       24  Saturday    Weekend                64.000
    4       24  Saturday    Weekend                64.000
    4       24  Saturday    Weekend                64.000
    4       24  Saturday    Weekend                64.000
    4       24  Saturday    Weekend                64.000
    4       24  Saturday    Weekend                64.000
    4       24  Saturday    Weekend                64.000
    4       24  Saturday    Weekend                64.000
    4       24  Saturday    Weekend                64.000
    4       24  Saturday    Weekend                64.000
    4       24  Saturday    Weekend                64.000
    4       24  Saturday    Weekend                64.000
    4       24  Saturday    Weekend                64.000
    4       24  Saturday    Weekend                64.000
    4       24  Saturday    Weekend                64.000
    4       24  Saturday    Weekend                64.000
    4       24  Saturday    Weekend                64.000
    4       24  Saturday    Weekend                64.000
    4       24  Saturday    Weekend                64.000
    4       24  Saturday    Weekend                64.000
    4       24  Saturday    Weekend                64.000
    4       24  Saturday    Weekend                64.000
    4       24  Saturday    Weekend                64.000
    4       24  Saturday    Weekend                64.000
    4       24  Saturday    Weekend                64.000
    4       24  Saturday    Weekend                64.000
    4       24  Saturday    Weekend                64.000
    4       24  Saturday    Weekend                64.000
    4       24  Saturday    Weekend                64.000
    4       24  Saturday    Weekend                64.000
    4       24  Saturday    Weekend                64.000
    4       24  Saturday    Weekend                64.000
    4       24  Saturday    Weekend                64.000
    4       24  Saturday    Weekend                64.000
    4       24  Saturday    Weekend                64.000
    4       24  Saturday    Weekend                64.000
    4       24  Saturday    Weekend                64.000
    4       24  Saturday    Weekend                64.000
    4       24  Saturday    Weekend                64.000
    4       24  Saturday    Weekend                64.000
    4       24  Saturday    Weekend                64.000
    4       24  Saturday    Weekend                64.000
    4       24  Saturday    Weekend                64.000
    4       24  Saturday    Weekend                64.000
    4       24  Saturday    Weekend                64.000
    4       24  Saturday    Weekend                64.000
    4       24  Saturday    Weekend                64.000
    4       24  Saturday    Weekend                64.000
    4       24  Saturday    Weekend                64.000
    4       24  Saturday    Weekend                64.000
    4       24  Saturday    Weekend                64.000
    4       24  Saturday    Weekend                64.000
    4       24  Saturday    Weekend                64.000
    4       24  Saturday    Weekend                64.000
    4       24  Saturday    Weekend                64.000
    4       24  Saturday    Weekend                64.000
    4       24  Saturday    Weekend                64.000
    4       24  Saturday    Weekend                64.000
    4       24  Saturday    Weekend                64.000
    4       24  Saturday    Weekend                64.000
    4       25  Sunday      Weekend                64.000
    4       25  Sunday      Weekend                64.000
    4       25  Sunday      Weekend                64.000
    4       25  Sunday      Weekend                64.000
    4       25  Sunday      Weekend                64.000
    4       25  Sunday      Weekend                64.000
    4       25  Sunday      Weekend                64.000
    4       25  Sunday      Weekend                64.000
    4       25  Sunday      Weekend                64.000
    4       25  Sunday      Weekend                64.000
    4       25  Sunday      Weekend                64.000
    4       25  Sunday      Weekend                64.000
    4       25  Sunday      Weekend                64.000
    4       25  Sunday      Weekend                64.000
    4       25  Sunday      Weekend                64.000
    4       25  Sunday      Weekend                64.000
    4       25  Sunday      Weekend                64.000
    4       25  Sunday      Weekend                64.000
    4       25  Sunday      Weekend                64.000
    4       25  Sunday      Weekend                64.000
    4       25  Sunday      Weekend                64.000
    4       25  Sunday      Weekend                64.000
    4       25  Sunday      Weekend                64.000
    4       25  Sunday      Weekend                64.000
    4       25  Sunday      Weekend                64.000
    4       25  Sunday      Weekend                64.000
    4       25  Sunday      Weekend                64.000
    4       25  Sunday      Weekend                64.000
    4       25  Sunday      Weekend                64.000
    4       25  Sunday      Weekend                64.000
    4       25  Sunday      Weekend                64.000
    4       25  Sunday      Weekend                64.000
    4       25  Sunday      Weekend                64.000
    4       25  Sunday      Weekend                64.000
    4       25  Sunday      Weekend                64.000
    4       25  Sunday      Weekend                64.000
    4       25  Sunday      Weekend                64.000
    4       25  Sunday      Weekend                64.000
    4       25  Sunday      Weekend                64.000
    4       25  Sunday      Weekend                64.000
    4       25  Sunday      Weekend                64.000
    4       25  Sunday      Weekend                64.000
    4       25  Sunday      Weekend                64.000
    4       25  Sunday      Weekend                64.000
    4       25  Sunday      Weekend                64.000
    4       25  Sunday      Weekend                64.000
    4       25  Sunday      Weekend                64.000
    4       25  Sunday      Weekend                64.000
    4       25  Sunday      Weekend                64.000
    4       25  Sunday      Weekend                64.000
    4       25  Sunday      Weekend                64.000
    4       25  Sunday      Weekend                64.000
    4       25  Sunday      Weekend                64.000
    4       25  Sunday      Weekend                64.000
    4       25  Sunday      Weekend                64.000
    4       25  Sunday      Weekend                64.000
    4       25  Sunday      Weekend                64.000
    4       25  Sunday      Weekend                64.000
    4       25  Sunday      Weekend                64.000
    4       25  Sunday      Weekend                64.000
    4       25  Sunday      Weekend                64.000
    4       25  Sunday      Weekend                64.000
    4       25  Sunday      Weekend                64.000
    4       25  Sunday      Weekend                64.000
    4       26  Thursday    Weekday             16706.000
    4       26  Thursday    Weekday             16706.000
    4       26  Thursday    Weekday             16706.000
    4       26  Thursday    Weekday             16706.000
    4       26  Thursday    Weekday             16706.000
    4       26  Thursday    Weekday             16706.000
    4       26  Thursday    Weekday             16706.000
    4       26  Thursday    Weekday             16706.000
    4       26  Thursday    Weekday             16706.000
    4       26  Thursday    Weekday             16706.000
    4       26  Thursday    Weekday             16706.000
    4       26  Thursday    Weekday             16706.000
    4       26  Thursday    Weekday             16706.000
    4       26  Thursday    Weekday             16706.000
    4       26  Thursday    Weekday             16706.000
    4       26  Thursday    Weekday             16706.000
    4       26  Thursday    Weekday             16706.000
    4       26  Thursday    Weekday             16706.000
    4       26  Thursday    Weekday             16706.000
    4       26  Thursday    Weekday             16706.000
    4       26  Thursday    Weekday             16706.000
    4       26  Thursday    Weekday             16706.000
    4       26  Thursday    Weekday             16706.000
    4       26  Thursday    Weekday             16706.000
    4       26  Thursday    Weekday             16706.000
    4       26  Thursday    Weekday             16706.000
    4       26  Thursday    Weekday             16706.000
    4       26  Thursday    Weekday             16706.000
    4       26  Thursday    Weekday             16706.000
    4       26  Thursday    Weekday             16706.000
    4       26  Thursday    Weekday             16706.000
    4       26  Thursday    Weekday             16706.000
    4       26  Thursday    Weekday             16706.000
    4       26  Thursday    Weekday             16706.000
    4       26  Thursday    Weekday             16706.000
    4       26  Thursday    Weekday             16706.000
    4       26  Thursday    Weekday             16706.000
    4       26  Thursday    Weekday             16706.000
    4       26  Thursday    Weekday             16706.000
    4       26  Thursday    Weekday             16706.000
    4       26  Thursday    Weekday             16706.000
    4       26  Thursday    Weekday             16706.000
    4       26  Thursday    Weekday             16706.000
    4       26  Thursday    Weekday             16706.000
    4       26  Thursday    Weekday             16706.000
    4       26  Thursday    Weekday             16706.000
    4       26  Thursday    Weekday             16706.000
    4       26  Thursday    Weekday             16706.000
    4       26  Thursday    Weekday             16706.000
    4       26  Thursday    Weekday             16706.000
    4       26  Thursday    Weekday             16706.000
    4       26  Thursday    Weekday             16706.000
    4       26  Thursday    Weekday             16706.000
    4       26  Thursday    Weekday             16706.000
    4       26  Thursday    Weekday             16706.000
    4       26  Thursday    Weekday             16706.000
    4       26  Thursday    Weekday             16706.000
    4       26  Thursday    Weekday             16706.000
    4       26  Thursday    Weekday             16706.000
    4       26  Thursday    Weekday             16706.000
    4       26  Thursday    Weekday             16706.000
    4       26  Thursday    Weekday             16706.000
    4       26  Thursday    Weekday             16706.000
    4       26  Thursday    Weekday             16706.000
    4       27  Tuesday     Weekday               410.000
    4       27  Tuesday     Weekday               410.000
    4       27  Tuesday     Weekday               410.000
    4       27  Tuesday     Weekday               410.000
    4       27  Tuesday     Weekday               410.000
    4       27  Tuesday     Weekday               410.000
    4       27  Tuesday     Weekday               410.000
    4       27  Tuesday     Weekday               410.000
    4       27  Tuesday     Weekday               410.000
    4       27  Tuesday     Weekday               410.000
    4       27  Tuesday     Weekday               410.000
    4       27  Tuesday     Weekday               410.000
    4       27  Tuesday     Weekday               410.000
    4       27  Tuesday     Weekday               410.000
    4       27  Tuesday     Weekday               410.000
    4       27  Tuesday     Weekday               410.000
    4       27  Tuesday     Weekday               410.000
    4       27  Tuesday     Weekday               410.000
    4       27  Tuesday     Weekday               410.000
    4       27  Tuesday     Weekday               410.000
    4       27  Tuesday     Weekday               410.000
    4       27  Tuesday     Weekday               410.000
    4       27  Tuesday     Weekday               410.000
    4       27  Tuesday     Weekday               410.000
    4       27  Tuesday     Weekday               410.000
    4       27  Tuesday     Weekday               410.000
    4       27  Tuesday     Weekday               410.000
    4       27  Tuesday     Weekday               410.000
    4       27  Tuesday     Weekday               410.000
    4       27  Tuesday     Weekday               410.000
    4       27  Tuesday     Weekday               410.000
    4       27  Tuesday     Weekday               410.000
    4       27  Tuesday     Weekday               410.000
    4       27  Tuesday     Weekday               410.000
    4       27  Tuesday     Weekday               410.000
    4       27  Tuesday     Weekday               410.000
    4       27  Tuesday     Weekday               410.000
    4       27  Tuesday     Weekday               410.000
    4       27  Tuesday     Weekday               410.000
    4       27  Tuesday     Weekday               410.000
    4       27  Tuesday     Weekday               410.000
    4       27  Tuesday     Weekday               410.000
    4       27  Tuesday     Weekday               410.000
    4       27  Tuesday     Weekday               410.000
    4       27  Tuesday     Weekday               410.000
    4       27  Tuesday     Weekday               410.000
    4       27  Tuesday     Weekday               410.000
    4       27  Tuesday     Weekday               410.000
    4       27  Tuesday     Weekday               410.000
    4       27  Tuesday     Weekday               410.000
    4       27  Tuesday     Weekday               410.000
    4       27  Tuesday     Weekday               410.000
    4       27  Tuesday     Weekday               410.000
    4       27  Tuesday     Weekday               410.000
    4       27  Tuesday     Weekday               410.000
    4       27  Tuesday     Weekday               410.000
    4       27  Tuesday     Weekday               410.000
    4       27  Tuesday     Weekday               410.000
    4       27  Tuesday     Weekday               410.000
    4       27  Tuesday     Weekday               410.000
    4       27  Tuesday     Weekday               410.000
    4       27  Tuesday     Weekday               410.000
    4       27  Tuesday     Weekday               410.000
    4       27  Tuesday     Weekday               410.000
    4       28  Wednesday   Weekday              4604.000
    4       28  Wednesday   Weekday              4604.000
    4       28  Wednesday   Weekday              4604.000
    4       28  Wednesday   Weekday              4604.000
    4       28  Wednesday   Weekday              4604.000
    4       28  Wednesday   Weekday              4604.000
    4       28  Wednesday   Weekday              4604.000
    4       28  Wednesday   Weekday              4604.000
    4       28  Wednesday   Weekday              4604.000
    4       28  Wednesday   Weekday              4604.000
    4       28  Wednesday   Weekday              4604.000
    4       28  Wednesday   Weekday              4604.000
    4       28  Wednesday   Weekday              4604.000
    4       28  Wednesday   Weekday              4604.000
    4       28  Wednesday   Weekday              4604.000
    4       28  Wednesday   Weekday              4604.000
    4       28  Wednesday   Weekday              4604.000
    4       28  Wednesday   Weekday              4604.000
    4       28  Wednesday   Weekday              4604.000
    4       28  Wednesday   Weekday              4604.000
    4       28  Wednesday   Weekday              4604.000
    4       28  Wednesday   Weekday              4604.000
    4       28  Wednesday   Weekday              4604.000
    4       28  Wednesday   Weekday              4604.000
    4       28  Wednesday   Weekday              4604.000
    4       28  Wednesday   Weekday              4604.000
    4       28  Wednesday   Weekday              4604.000
    4       28  Wednesday   Weekday              4604.000
    4       28  Wednesday   Weekday              4604.000
    4       28  Wednesday   Weekday              4604.000
    4       28  Wednesday   Weekday              4604.000
    4       28  Wednesday   Weekday              4604.000
    4       28  Wednesday   Weekday              4604.000
    4       28  Wednesday   Weekday              4604.000
    4       28  Wednesday   Weekday              4604.000
    4       28  Wednesday   Weekday              4604.000
    4       28  Wednesday   Weekday              4604.000
    4       28  Wednesday   Weekday              4604.000
    4       28  Wednesday   Weekday              4604.000
    4       28  Wednesday   Weekday              4604.000
    4       28  Wednesday   Weekday              4604.000
    4       28  Wednesday   Weekday              4604.000
    4       28  Wednesday   Weekday              4604.000
    4       28  Wednesday   Weekday              4604.000
    4       28  Wednesday   Weekday              4604.000
    4       28  Wednesday   Weekday              4604.000
    4       28  Wednesday   Weekday              4604.000
    4       28  Wednesday   Weekday              4604.000
    4       28  Wednesday   Weekday              4604.000
    4       28  Wednesday   Weekday              4604.000
    4       28  Wednesday   Weekday              4604.000
    4       28  Wednesday   Weekday              4604.000
    4       28  Wednesday   Weekday              4604.000
    4       28  Wednesday   Weekday              4604.000
    4       28  Wednesday   Weekday              4604.000
    4       28  Wednesday   Weekday              4604.000
    4       28  Wednesday   Weekday              4604.000
    4       28  Wednesday   Weekday              4604.000
    4       28  Wednesday   Weekday              4604.000
    4       28  Wednesday   Weekday              4604.000
    4       28  Wednesday   Weekday              4604.000
    4       28  Wednesday   Weekday              4604.000
    4       28  Wednesday   Weekday              4604.000
    4       28  Wednesday   Weekday              4604.000
    5       29  Friday      Weekday              1148.000
    5       29  Friday      Weekday              1148.000
    5       29  Friday      Weekday              1148.000
    5       29  Friday      Weekday              1148.000
    5       29  Friday      Weekday              1148.000
    5       29  Friday      Weekday              1148.000
    5       29  Friday      Weekday              1148.000
    5       29  Friday      Weekday              1148.000
    5       29  Friday      Weekday              1148.000
    5       29  Friday      Weekday              1148.000
    5       29  Friday      Weekday              1148.000
    5       29  Friday      Weekday              1148.000
    5       29  Friday      Weekday              1148.000
    5       29  Friday      Weekday              1148.000
    5       29  Friday      Weekday              1148.000
    5       29  Friday      Weekday              1148.000
    5       29  Friday      Weekday              1148.000
    5       29  Friday      Weekday              1148.000
    5       29  Friday      Weekday              1148.000
    5       29  Friday      Weekday              1148.000
    5       29  Friday      Weekday              1148.000
    5       29  Friday      Weekday              1148.000
    5       29  Friday      Weekday              1148.000
    5       29  Friday      Weekday              1148.000
    5       29  Friday      Weekday              1148.000
    5       29  Friday      Weekday              1148.000
    5       29  Friday      Weekday              1148.000
    5       29  Friday      Weekday              1148.000
    5       29  Friday      Weekday              1148.000
    5       29  Friday      Weekday              1148.000
    5       29  Friday      Weekday              1148.000
    5       29  Friday      Weekday              1148.000
    5       29  Friday      Weekday              1148.000
    5       29  Friday      Weekday              1148.000
    5       29  Friday      Weekday              1148.000
    5       29  Friday      Weekday              1148.000
    5       29  Friday      Weekday              1148.000
    5       29  Friday      Weekday              1148.000
    5       29  Friday      Weekday              1148.000
    5       29  Friday      Weekday              1148.000
    5       29  Friday      Weekday              1148.000
    5       29  Friday      Weekday              1148.000
    5       29  Friday      Weekday              1148.000
    5       29  Friday      Weekday              1148.000
    5       29  Friday      Weekday              1148.000
    5       29  Friday      Weekday              1148.000
    5       29  Friday      Weekday              1148.000
    5       29  Friday      Weekday              1148.000
    5       29  Friday      Weekday              1148.000
    5       29  Friday      Weekday              1148.000
    5       29  Friday      Weekday              1148.000
    5       29  Friday      Weekday              1148.000
    5       29  Friday      Weekday              1148.000
    5       29  Friday      Weekday              1148.000
    5       29  Friday      Weekday              1148.000
    5       29  Friday      Weekday              1148.000
    5       29  Friday      Weekday              1148.000
    5       29  Friday      Weekday              1148.000
    5       29  Friday      Weekday              1148.000
    5       29  Friday      Weekday              1148.000
    5       29  Friday      Weekday              1148.000
    5       29  Friday      Weekday              1148.000
    5       29  Friday      Weekday              1148.000
    5       29  Friday      Weekday              1148.000
    5       30  Monday      Weekday               931.000
    5       30  Monday      Weekday               931.000
    5       30  Monday      Weekday               931.000
    5       30  Monday      Weekday               931.000
    5       30  Monday      Weekday               931.000
    5       30  Monday      Weekday               931.000
    5       30  Monday      Weekday               931.000
    5       30  Monday      Weekday               931.000
    5       30  Monday      Weekday               931.000
    5       30  Monday      Weekday               931.000
    5       30  Monday      Weekday               931.000
    5       30  Monday      Weekday               931.000
    5       30  Monday      Weekday               931.000
    5       30  Monday      Weekday               931.000
    5       30  Monday      Weekday               931.000
    5       30  Monday      Weekday               931.000
    5       30  Monday      Weekday               931.000
    5       30  Monday      Weekday               931.000
    5       30  Monday      Weekday               931.000
    5       30  Monday      Weekday               931.000
    5       30  Monday      Weekday               931.000
    5       30  Monday      Weekday               931.000
    5       30  Monday      Weekday               931.000
    5       30  Monday      Weekday               931.000
    5       30  Monday      Weekday               931.000
    5       30  Monday      Weekday               931.000
    5       30  Monday      Weekday               931.000
    5       30  Monday      Weekday               931.000
    5       30  Monday      Weekday               931.000
    5       30  Monday      Weekday               931.000
    5       30  Monday      Weekday               931.000
    5       30  Monday      Weekday               931.000
    5       30  Monday      Weekday               931.000
    5       30  Monday      Weekday               931.000
    5       30  Monday      Weekday               931.000
    5       30  Monday      Weekday               931.000
    5       30  Monday      Weekday               931.000
    5       30  Monday      Weekday               931.000
    5       30  Monday      Weekday               931.000
    5       30  Monday      Weekday               931.000
    5       30  Monday      Weekday               931.000
    5       30  Monday      Weekday               931.000
    5       30  Monday      Weekday               931.000
    5       30  Monday      Weekday               931.000
    5       30  Monday      Weekday               931.000
    5       30  Monday      Weekday               931.000
    5       30  Monday      Weekday               931.000
    5       30  Monday      Weekday               931.000
    5       30  Monday      Weekday               931.000
    5       30  Monday      Weekday               931.000
    5       30  Monday      Weekday               931.000
    5       30  Monday      Weekday               931.000
    5       30  Monday      Weekday               931.000
    5       30  Monday      Weekday               931.000
    5       30  Monday      Weekday               931.000
    5       30  Monday      Weekday               931.000
    5       30  Monday      Weekday               931.000
    5       30  Monday      Weekday               931.000
    5       30  Monday      Weekday               931.000
    5       30  Monday      Weekday               931.000
    5       30  Monday      Weekday               931.000
    5       30  Monday      Weekday               931.000
    5       30  Monday      Weekday               931.000
    5       30  Monday      Weekday               931.000
    5       31  Saturday    Weekend                64.000
    5       31  Saturday    Weekend                64.000
    5       31  Saturday    Weekend                64.000
    5       31  Saturday    Weekend                64.000
    5       31  Saturday    Weekend                64.000
    5       31  Saturday    Weekend                64.000
    5       31  Saturday    Weekend                64.000
    5       31  Saturday    Weekend                64.000
    5       31  Saturday    Weekend                64.000
    5       31  Saturday    Weekend                64.000
    5       31  Saturday    Weekend                64.000
    5       31  Saturday    Weekend                64.000
    5       31  Saturday    Weekend                64.000
    5       31  Saturday    Weekend                64.000
    5       31  Saturday    Weekend                64.000
    5       31  Saturday    Weekend                64.000
    5       31  Saturday    Weekend                64.000
    5       31  Saturday    Weekend                64.000
    5       31  Saturday    Weekend                64.000
    5       31  Saturday    Weekend                64.000
    5       31  Saturday    Weekend                64.000
    5       31  Saturday    Weekend                64.000
    5       31  Saturday    Weekend                64.000
    5       31  Saturday    Weekend                64.000
    5       31  Saturday    Weekend                64.000
    5       31  Saturday    Weekend                64.000
    5       31  Saturday    Weekend                64.000
    5       31  Saturday    Weekend                64.000
    5       31  Saturday    Weekend                64.000
    5       31  Saturday    Weekend                64.000
    5       31  Saturday    Weekend                64.000
    5       31  Saturday    Weekend                64.000
    5       31  Saturday    Weekend                64.000
    5       31  Saturday    Weekend                64.000
    5       31  Saturday    Weekend                64.000
    5       31  Saturday    Weekend                64.000
    5       31  Saturday    Weekend                64.000
    5       31  Saturday    Weekend                64.000
    5       31  Saturday    Weekend                64.000
    5       31  Saturday    Weekend                64.000
    5       31  Saturday    Weekend                64.000
    5       31  Saturday    Weekend                64.000
    5       31  Saturday    Weekend                64.000
    5       31  Saturday    Weekend                64.000
    5       31  Saturday    Weekend                64.000
    5       31  Saturday    Weekend                64.000
    5       31  Saturday    Weekend                64.000
    5       31  Saturday    Weekend                64.000
    5       31  Saturday    Weekend                64.000
    5       31  Saturday    Weekend                64.000
    5       31  Saturday    Weekend                64.000
    5       31  Saturday    Weekend                64.000
    5       31  Saturday    Weekend                64.000
    5       31  Saturday    Weekend                64.000
    5       31  Saturday    Weekend                64.000
    5       31  Saturday    Weekend                64.000
    5       31  Saturday    Weekend                64.000
    5       31  Saturday    Weekend                64.000
    5       31  Saturday    Weekend                64.000
    5       31  Saturday    Weekend                64.000
    5       31  Saturday    Weekend                64.000
    5       31  Saturday    Weekend                64.000
    5       31  Saturday    Weekend                64.000
    5       31  Saturday    Weekend                64.000
    5       32  Sunday      Weekend                64.000
    5       32  Sunday      Weekend                64.000
    5       32  Sunday      Weekend                64.000
    5       32  Sunday      Weekend                64.000
    5       32  Sunday      Weekend                64.000
    5       32  Sunday      Weekend                64.000
    5       32  Sunday      Weekend                64.000
    5       32  Sunday      Weekend                64.000
    5       32  Sunday      Weekend                64.000
    5       32  Sunday      Weekend                64.000
    5       32  Sunday      Weekend                64.000
    5       32  Sunday      Weekend                64.000
    5       32  Sunday      Weekend                64.000
    5       32  Sunday      Weekend                64.000
    5       32  Sunday      Weekend                64.000
    5       32  Sunday      Weekend                64.000
    5       32  Sunday      Weekend                64.000
    5       32  Sunday      Weekend                64.000
    5       32  Sunday      Weekend                64.000
    5       32  Sunday      Weekend                64.000
    5       32  Sunday      Weekend                64.000
    5       32  Sunday      Weekend                64.000
    5       32  Sunday      Weekend                64.000
    5       32  Sunday      Weekend                64.000
    5       32  Sunday      Weekend                64.000
    5       32  Sunday      Weekend                64.000
    5       32  Sunday      Weekend                64.000
    5       32  Sunday      Weekend                64.000
    5       32  Sunday      Weekend                64.000
    5       32  Sunday      Weekend                64.000
    5       32  Sunday      Weekend                64.000
    5       32  Sunday      Weekend                64.000
    5       32  Sunday      Weekend                64.000
    5       32  Sunday      Weekend                64.000
    5       32  Sunday      Weekend                64.000
    5       32  Sunday      Weekend                64.000
    5       32  Sunday      Weekend                64.000
    5       32  Sunday      Weekend                64.000
    5       32  Sunday      Weekend                64.000
    5       32  Sunday      Weekend                64.000
    5       32  Sunday      Weekend                64.000
    5       32  Sunday      Weekend                64.000
    5       32  Sunday      Weekend                64.000
    5       32  Sunday      Weekend                64.000
    5       32  Sunday      Weekend                64.000
    5       32  Sunday      Weekend                64.000
    5       32  Sunday      Weekend                64.000
    5       32  Sunday      Weekend                64.000
    5       32  Sunday      Weekend                64.000
    5       32  Sunday      Weekend                64.000
    5       32  Sunday      Weekend                64.000
    5       32  Sunday      Weekend                64.000
    5       32  Sunday      Weekend                64.000
    5       32  Sunday      Weekend                64.000
    5       32  Sunday      Weekend                64.000
    5       32  Sunday      Weekend                64.000
    5       32  Sunday      Weekend                64.000
    5       32  Sunday      Weekend                64.000
    5       32  Sunday      Weekend                64.000
    5       32  Sunday      Weekend                64.000
    5       32  Sunday      Weekend                64.000
    5       32  Sunday      Weekend                64.000
    5       32  Sunday      Weekend                64.000
    5       32  Sunday      Weekend                64.000
    5       33  Thursday    Weekday              2106.000
    5       33  Thursday    Weekday              2106.000
    5       33  Thursday    Weekday              2106.000
    5       33  Thursday    Weekday              2106.000
    5       33  Thursday    Weekday              2106.000
    5       33  Thursday    Weekday              2106.000
    5       33  Thursday    Weekday              2106.000
    5       33  Thursday    Weekday              2106.000
    5       33  Thursday    Weekday              2106.000
    5       33  Thursday    Weekday              2106.000
    5       33  Thursday    Weekday              2106.000
    5       33  Thursday    Weekday              2106.000
    5       33  Thursday    Weekday              2106.000
    5       33  Thursday    Weekday              2106.000
    5       33  Thursday    Weekday              2106.000
    5       33  Thursday    Weekday              2106.000
    5       33  Thursday    Weekday              2106.000
    5       33  Thursday    Weekday              2106.000
    5       33  Thursday    Weekday              2106.000
    5       33  Thursday    Weekday              2106.000
    5       33  Thursday    Weekday              2106.000
    5       33  Thursday    Weekday              2106.000
    5       33  Thursday    Weekday              2106.000
    5       33  Thursday    Weekday              2106.000
    5       33  Thursday    Weekday              2106.000
    5       33  Thursday    Weekday              2106.000
    5       33  Thursday    Weekday              2106.000
    5       33  Thursday    Weekday              2106.000
    5       33  Thursday    Weekday              2106.000
    5       33  Thursday    Weekday              2106.000
    5       33  Thursday    Weekday              2106.000
    5       33  Thursday    Weekday              2106.000
    5       33  Thursday    Weekday              2106.000
    5       33  Thursday    Weekday              2106.000
    5       33  Thursday    Weekday              2106.000
    5       33  Thursday    Weekday              2106.000
    5       33  Thursday    Weekday              2106.000
    5       33  Thursday    Weekday              2106.000
    5       33  Thursday    Weekday              2106.000
    5       33  Thursday    Weekday              2106.000
    5       33  Thursday    Weekday              2106.000
    5       33  Thursday    Weekday              2106.000
    5       33  Thursday    Weekday              2106.000
    5       33  Thursday    Weekday              2106.000
    5       33  Thursday    Weekday              2106.000
    5       33  Thursday    Weekday              2106.000
    5       33  Thursday    Weekday              2106.000
    5       33  Thursday    Weekday              2106.000
    5       33  Thursday    Weekday              2106.000
    5       33  Thursday    Weekday              2106.000
    5       33  Thursday    Weekday              2106.000
    5       33  Thursday    Weekday              2106.000
    5       33  Thursday    Weekday              2106.000
    5       33  Thursday    Weekday              2106.000
    5       33  Thursday    Weekday              2106.000
    5       33  Thursday    Weekday              2106.000
    5       33  Thursday    Weekday              2106.000
    5       33  Thursday    Weekday              2106.000
    5       33  Thursday    Weekday              2106.000
    5       33  Thursday    Weekday              2106.000
    5       33  Thursday    Weekday              2106.000
    5       33  Thursday    Weekday              2106.000
    5       33  Thursday    Weekday              2106.000
    5       33  Thursday    Weekday              2106.000
    5       34  Tuesday     Weekday               660.000
    5       34  Tuesday     Weekday               660.000
    5       34  Tuesday     Weekday               660.000
    5       34  Tuesday     Weekday               660.000
    5       34  Tuesday     Weekday               660.000
    5       34  Tuesday     Weekday               660.000
    5       34  Tuesday     Weekday               660.000
    5       34  Tuesday     Weekday               660.000
    5       34  Tuesday     Weekday               660.000
    5       34  Tuesday     Weekday               660.000
    5       34  Tuesday     Weekday               660.000
    5       34  Tuesday     Weekday               660.000
    5       34  Tuesday     Weekday               660.000
    5       34  Tuesday     Weekday               660.000
    5       34  Tuesday     Weekday               660.000
    5       34  Tuesday     Weekday               660.000
    5       34  Tuesday     Weekday               660.000
    5       34  Tuesday     Weekday               660.000
    5       34  Tuesday     Weekday               660.000
    5       34  Tuesday     Weekday               660.000
    5       34  Tuesday     Weekday               660.000
    5       34  Tuesday     Weekday               660.000
    5       34  Tuesday     Weekday               660.000
    5       34  Tuesday     Weekday               660.000
    5       34  Tuesday     Weekday               660.000
    5       34  Tuesday     Weekday               660.000
    5       34  Tuesday     Weekday               660.000
    5       34  Tuesday     Weekday               660.000
    5       34  Tuesday     Weekday               660.000
    5       34  Tuesday     Weekday               660.000
    5       34  Tuesday     Weekday               660.000
    5       34  Tuesday     Weekday               660.000
    5       34  Tuesday     Weekday               660.000
    5       34  Tuesday     Weekday               660.000
    5       34  Tuesday     Weekday               660.000
    5       34  Tuesday     Weekday               660.000
    5       34  Tuesday     Weekday               660.000
    5       34  Tuesday     Weekday               660.000
    5       34  Tuesday     Weekday               660.000
    5       34  Tuesday     Weekday               660.000
    5       34  Tuesday     Weekday               660.000
    5       34  Tuesday     Weekday               660.000
    5       34  Tuesday     Weekday               660.000
    5       34  Tuesday     Weekday               660.000
    5       34  Tuesday     Weekday               660.000
    5       34  Tuesday     Weekday               660.000
    5       34  Tuesday     Weekday               660.000
    5       34  Tuesday     Weekday               660.000
    5       34  Tuesday     Weekday               660.000
    5       34  Tuesday     Weekday               660.000
    5       34  Tuesday     Weekday               660.000
    5       34  Tuesday     Weekday               660.000
    5       34  Tuesday     Weekday               660.000
    5       34  Tuesday     Weekday               660.000
    5       34  Tuesday     Weekday               660.000
    5       34  Tuesday     Weekday               660.000
    5       34  Tuesday     Weekday               660.000
    5       34  Tuesday     Weekday               660.000
    5       34  Tuesday     Weekday               660.000
    5       34  Tuesday     Weekday               660.000
    5       34  Tuesday     Weekday               660.000
    5       34  Tuesday     Weekday               660.000
    5       34  Tuesday     Weekday               660.000
    5       34  Tuesday     Weekday               660.000
    5       35  Wednesday   Weekday              1435.000
    5       35  Wednesday   Weekday              1435.000
    5       35  Wednesday   Weekday              1435.000
    5       35  Wednesday   Weekday              1435.000
    5       35  Wednesday   Weekday              1435.000
    5       35  Wednesday   Weekday              1435.000
    5       35  Wednesday   Weekday              1435.000
    5       35  Wednesday   Weekday              1435.000
    5       35  Wednesday   Weekday              1435.000
    5       35  Wednesday   Weekday              1435.000
    5       35  Wednesday   Weekday              1435.000
    5       35  Wednesday   Weekday              1435.000
    5       35  Wednesday   Weekday              1435.000
    5       35  Wednesday   Weekday              1435.000
    5       35  Wednesday   Weekday              1435.000
    5       35  Wednesday   Weekday              1435.000
    5       35  Wednesday   Weekday              1435.000
    5       35  Wednesday   Weekday              1435.000
    5       35  Wednesday   Weekday              1435.000
    5       35  Wednesday   Weekday              1435.000
    5       35  Wednesday   Weekday              1435.000
    5       35  Wednesday   Weekday              1435.000
    5       35  Wednesday   Weekday              1435.000
    5       35  Wednesday   Weekday              1435.000
    5       35  Wednesday   Weekday              1435.000
    5       35  Wednesday   Weekday              1435.000
    5       35  Wednesday   Weekday              1435.000
    5       35  Wednesday   Weekday              1435.000
    5       35  Wednesday   Weekday              1435.000
    5       35  Wednesday   Weekday              1435.000
    5       35  Wednesday   Weekday              1435.000
    5       35  Wednesday   Weekday              1435.000
    5       35  Wednesday   Weekday              1435.000
    5       35  Wednesday   Weekday              1435.000
    5       35  Wednesday   Weekday              1435.000
    5       35  Wednesday   Weekday              1435.000
    5       35  Wednesday   Weekday              1435.000
    5       35  Wednesday   Weekday              1435.000
    5       35  Wednesday   Weekday              1435.000
    5       35  Wednesday   Weekday              1435.000
    5       35  Wednesday   Weekday              1435.000
    5       35  Wednesday   Weekday              1435.000
    5       35  Wednesday   Weekday              1435.000
    5       35  Wednesday   Weekday              1435.000
    5       35  Wednesday   Weekday              1435.000
    5       35  Wednesday   Weekday              1435.000
    5       35  Wednesday   Weekday              1435.000
    5       35  Wednesday   Weekday              1435.000
    5       35  Wednesday   Weekday              1435.000
    5       35  Wednesday   Weekday              1435.000
    5       35  Wednesday   Weekday              1435.000
    5       35  Wednesday   Weekday              1435.000
    5       35  Wednesday   Weekday              1435.000
    5       35  Wednesday   Weekday              1435.000
    5       35  Wednesday   Weekday              1435.000
    5       35  Wednesday   Weekday              1435.000
    5       35  Wednesday   Weekday              1435.000
    5       35  Wednesday   Weekday              1435.000
    5       35  Wednesday   Weekday              1435.000
    5       35  Wednesday   Weekday              1435.000
    5       35  Wednesday   Weekday              1435.000
    5       35  Wednesday   Weekday              1435.000
    5       35  Wednesday   Weekday              1435.000
    5       35  Wednesday   Weekday              1435.000
Comments

There is a general decreasing trend from Monday to Wednesday, but a sudden increase on Thursday, and then a general decreasing trend form Thursday to Sunday. 


 Make a single-panel plot that shows the 24-hour activity time courses for each day and use color to indicate day of the week. 


```r
accel_data %>% 
  pivot_longer(
    activity_1:activity_64,
    names_to = "activity_number",
    names_prefix = "activity_",
    values_to = "activity_counts"
  ) %>% 
  ggplot(aes(x = activity_number, y = activity_counts, color = day)) +
  geom_point(alpha = .5)+
  labs(
    title = "24 hour activity timr courses",
    x = "time course",
    y = "activity counts",
    caption = "Data from accel data"
    
  )+
  theme_bw()+
  theme(legend.position = "bottom")
```

<img src="p8105_hw3_jf3286_for_Jingyu_files/figure-html/unnamed-chunk-13-1.png" width="90%" />
Description











