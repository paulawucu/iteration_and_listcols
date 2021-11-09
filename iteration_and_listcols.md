iteration_and_listcols
================
Paula Wu
11/9/2021

``` r
library(tidyverse)
```

    ## ── Attaching packages ─────────────────────────────────────── tidyverse 1.3.1 ──

    ## ✓ ggplot2 3.3.5     ✓ purrr   0.3.4
    ## ✓ tibble  3.1.5     ✓ dplyr   1.0.7
    ## ✓ tidyr   1.1.4     ✓ stringr 1.4.0
    ## ✓ readr   2.0.1     ✓ forcats 0.5.1

    ## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ## x dplyr::filter() masks stats::filter()
    ## x dplyr::lag()    masks stats::lag()

``` r
set.seed(1)
```

## lists

``` r
l = list(
  vec_numeric = 5:8,
  vec_logical = c(TRUE, FALSE),
  summary = summary(rnorm(1000, mean = 5, sd = 3))
)

l[[3]]
```

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ##  -4.024   2.908   4.894   4.965   7.065  16.431

``` r
l[["summary"]]
```

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ##  -4.024   2.908   4.894   4.965   7.065  16.431

``` r
l$summary  # you can use $ here
```

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ##  -4.024   2.908   4.894   4.965   7.065  16.431

## lists of normals

``` r
list_norms = 
  list(
    a = rnorm(50, mean = 2, sd = 1),
    b = rnorm(50, mean = 5, sd = 3),
    c = rnorm(50, mean = 20, sd = 1.2),
    d = rnorm(50, mean = -12, sd = 1),
    e = rnorm(50, mean = 0, sd = 1)
  )
```

``` r
mean_and_sd = function(x) {
  
  if (!is.numeric(x)) {
    stop("Argument x should be numeric")
  } else if (length(x) == 1) {
    stop("Cannot be computed for length 1 vectors")
  }
  
  mean_x = mean(x)
  sd_x = sd(x)

  tibble(
    mean = mean_x, 
    sd = sd_x
  )
}
```

``` r
mean_and_sd(list_norms[[1]]) # you can do this for 5 times, or write a for-loop
```

    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  1.98 0.967

## create an output list and for loop

``` r
# merge?
output = vector("list", length = 5)
for (i in 1:5){
  output[[i]] = mean_and_sd(list_norms[[i]])
}
output
```

    ## [[1]]
    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  1.98 0.967
    ## 
    ## [[2]]
    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  5.01  3.30
    ## 
    ## [[3]]
    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  19.8  1.09
    ## 
    ## [[4]]
    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1 -12.1  1.07
    ## 
    ## [[5]]
    ## # A tibble: 1 × 2
    ##     mean    sd
    ##    <dbl> <dbl>
    ## 1 0.0865  1.09

Let’s us map instead

``` r
output = map(list_norms, mean_and_sd)
output = map(list_norms, summary)
output = map_dbl(list_norms, median)  # collapse down into a simpler version
```

## List columns

``` r
listcol_df = 
  tibble(
    name = c("a","b","c","d"),
    norms = list_norms[1:4]
  )
listcol_df %>% 
  filter(name == "a")
```

    ## # A tibble: 1 × 2
    ##   name  norms       
    ##   <chr> <named list>
    ## 1 a     <dbl [50]>

``` r
listcol_df %>% pull(name)
```

    ## [1] "a" "b" "c" "d"

``` r
listcol_df %>% pull(norms)
```

    ## $a
    ##  [1] 3.1349651 3.1119318 1.1292224 2.2107316 2.0693956 0.3373511 2.8108400
    ##  [8] 0.0876542 0.7532466 2.9981544 1.4591273 1.7836242 0.3780627 0.5490360
    ## [15] 2.3509097 1.8254531 1.4085715 0.6659727 0.9027015 4.0361036 1.6735104
    ## [22] 2.7740052 2.7850064 2.7632461 2.2948088 0.7476441 0.9904962 2.7513912
    ## [29] 0.6916465 2.5275401 1.4664604 1.6016240 1.2104305 1.7698589 2.8771848
    ## [36] 2.4537332 1.7675359 2.8700055 3.6560037 1.9936311 2.4704895 2.2782186
    ## [43] 1.0220971 1.0734139 3.9197705 2.8812778 2.7420818 2.1475734 2.4853886
    ## [50] 2.1518560
    ## 
    ## $b
    ##  [1]  5.1259963  5.6702669  1.9686047 12.2036663  7.4058854  4.2463761
    ##  [7]  8.6386681  3.1182257 10.1334755  3.8168793 -1.9644726  9.0923576
    ## [13]  8.3966874  2.6770510  0.7688751 -0.5035827  4.1929594 -0.5017857
    ## [19]  2.5565959  5.4907164  7.5665577  2.5401106  4.6291917  5.7648447
    ## [25] 10.1567790  2.1243694  0.1870692 -0.5368283  6.6672116  4.8196424
    ## [31]  7.3162589  4.5774818  6.1792818  5.6726557  5.0706260  3.1311120
    ## [37]  8.7860281  3.7826779  7.0002913  5.4939175 10.3445734  7.1336419
    ## [43]  3.9869265  4.9725531  4.6240724 -1.2725383 10.0921817  8.1916435
    ## [49]  2.7001501  6.1460227
    ## 
    ## $c
    ##  [1] 20.29028 18.64069 21.78789 19.70210 20.22030 20.48585 18.80705 18.69748
    ##  [9] 19.94175 20.69130 20.08860 20.84713 20.40198 20.65447 18.31651 20.81246
    ## [17] 19.05224 19.44113 19.87418 18.02258 19.88056 19.47217 19.13779 19.33448
    ## [25] 21.49459 18.48929 19.74154 17.03365 19.19100 19.39844 21.85079 18.84558
    ## [33] 18.95338 18.32284 20.21577 21.38491 18.56176 19.48913 21.63957 19.17884
    ## [41] 20.82261 20.46740 18.43352 21.46027 20.95421 19.41416 18.91521 19.53150
    ## [49] 20.97688 19.32290
    ## 
    ## $d
    ##  [1] -13.874205 -12.142905 -11.228096 -13.159118 -12.237916 -13.222193
    ##  [7] -11.883194 -12.132500 -12.033685 -12.622326 -12.709363 -11.128555
    ## [13] -11.894862 -12.186935 -15.213189 -13.275619 -11.237094 -12.406815
    ## [19] -13.208318 -12.439323 -12.375581 -12.501442 -11.503387 -10.479119
    ## [25] -11.011908 -10.753877 -12.329867 -11.155655 -12.981076 -12.139221
    ## [31]  -9.814562 -12.012828 -12.305309 -12.584213 -11.228731  -9.893811
    ## [37] -11.587843 -12.261265  -9.926216 -12.778830 -10.868467 -12.421345
    ## [43] -13.021747 -10.781695 -13.799761 -12.308250 -11.984485 -12.442318
    ## [49] -13.638008 -12.641401

``` r
mean_and_sd(listcol_df$norms[[1]])
```

    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  1.98 0.967

``` r
map(listcol_df$norms, mean_and_sd)
```

    ## $a
    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  1.98 0.967
    ## 
    ## $b
    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  5.01  3.30
    ## 
    ## $c
    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  19.8  1.09
    ## 
    ## $d
    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1 -12.1  1.07

``` r
listcol_df %>% 
  mutate(summaries = map(listcol_df$norms, mean_and_sd)) %>% 
  pull(summaries)
```

    ## $a
    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  1.98 0.967
    ## 
    ## $b
    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  5.01  3.30
    ## 
    ## $c
    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  19.8  1.09
    ## 
    ## $d
    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1 -12.1  1.07

## Nested data

``` r
weather_df = 
  rnoaa::meteo_pull_monitors(
    c("USW00094728", "USC00519397", "USS0023B17S"),
    var = c("PRCP", "TMIN", "TMAX"), 
    date_min = "2017-01-01",
    date_max = "2017-12-31") %>%
  mutate(
    name = recode(
      id, 
      USW00094728 = "CentralPark_NY", 
      USC00519397 = "Waikiki_HA",
      USS0023B17S = "Waterhole_WA"),
    tmin = tmin / 10,
    tmax = tmax / 10) %>%
  select(name, id, everything())
```

    ## Registered S3 method overwritten by 'hoardr':
    ##   method           from
    ##   print.cache_info httr

    ## using cached file: ~/Library/Caches/R/noaa_ghcnd/USW00094728.dly

    ## date created (size, mb): 2021-10-05 10:29:22 (7.602)

    ## file min/max dates: 1869-01-01 / 2021-10-31

    ## using cached file: ~/Library/Caches/R/noaa_ghcnd/USC00519397.dly

    ## date created (size, mb): 2021-10-05 10:29:25 (1.697)

    ## file min/max dates: 1965-01-01 / 2020-02-29

    ## using cached file: ~/Library/Caches/R/noaa_ghcnd/USS0023B17S.dly

    ## date created (size, mb): 2021-10-05 10:29:27 (0.912)

    ## file min/max dates: 1999-09-01 / 2021-09-30

Nest data within location

``` r
weather_nest = 
  nest(weather_df, data = date:tmin)  # nest all data (date to tmin) into the "data" column

weather_nest
```

    ## # A tibble: 3 × 3
    ##   name           id          data              
    ##   <chr>          <chr>       <list>            
    ## 1 CentralPark_NY USW00094728 <tibble [365 × 4]>
    ## 2 Waikiki_HA     USC00519397 <tibble [365 × 4]>
    ## 3 Waterhole_WA   USS0023B17S <tibble [365 × 4]>

``` r
weather_nest %>% 
  filter(name == "CentralPark_NY") %>% 
  pull(data)
```

    ## [[1]]
    ## # A tibble: 365 × 4
    ##    date        prcp  tmax  tmin
    ##    <date>     <dbl> <dbl> <dbl>
    ##  1 2017-01-01     0   8.9   4.4
    ##  2 2017-01-02    53   5     2.8
    ##  3 2017-01-03   147   6.1   3.9
    ##  4 2017-01-04     0  11.1   1.1
    ##  5 2017-01-05     0   1.1  -2.7
    ##  6 2017-01-06    13   0.6  -3.8
    ##  7 2017-01-07    81  -3.2  -6.6
    ##  8 2017-01-08     0  -3.8  -8.8
    ##  9 2017-01-09     0  -4.9  -9.9
    ## 10 2017-01-10     0   7.8  -6  
    ## # … with 355 more rows

linear regression

``` r
weather_lm = function(df) {
  lm(tmax ~ tmin, data = df)
}
weather_lm(weather_nest$data[[1]])  # weather_lm on central park
```

    ## 
    ## Call:
    ## lm(formula = tmax ~ tmin, data = df)
    ## 
    ## Coefficients:
    ## (Intercept)         tmin  
    ##       7.209        1.039

``` r
map(weather_nest$data, weather_lm)
```

    ## [[1]]
    ## 
    ## Call:
    ## lm(formula = tmax ~ tmin, data = df)
    ## 
    ## Coefficients:
    ## (Intercept)         tmin  
    ##       7.209        1.039  
    ## 
    ## 
    ## [[2]]
    ## 
    ## Call:
    ## lm(formula = tmax ~ tmin, data = df)
    ## 
    ## Coefficients:
    ## (Intercept)         tmin  
    ##     20.0966       0.4509  
    ## 
    ## 
    ## [[3]]
    ## 
    ## Call:
    ## lm(formula = tmax ~ tmin, data = df)
    ## 
    ## Coefficients:
    ## (Intercept)         tmin  
    ##       7.499        1.221

``` r
weather_nest %>% 
  mutate(lm_results = map(data, weather_lm))
```

    ## # A tibble: 3 × 4
    ##   name           id          data               lm_results
    ##   <chr>          <chr>       <list>             <list>    
    ## 1 CentralPark_NY USW00094728 <tibble [365 × 4]> <lm>      
    ## 2 Waikiki_HA     USC00519397 <tibble [365 × 4]> <lm>      
    ## 3 Waterhole_WA   USS0023B17S <tibble [365 × 4]> <lm>

``` r
unnest(weather_nest,data) # unnest the data
```

    ## # A tibble: 1,095 × 6
    ##    name           id          date        prcp  tmax  tmin
    ##    <chr>          <chr>       <date>     <dbl> <dbl> <dbl>
    ##  1 CentralPark_NY USW00094728 2017-01-01     0   8.9   4.4
    ##  2 CentralPark_NY USW00094728 2017-01-02    53   5     2.8
    ##  3 CentralPark_NY USW00094728 2017-01-03   147   6.1   3.9
    ##  4 CentralPark_NY USW00094728 2017-01-04     0  11.1   1.1
    ##  5 CentralPark_NY USW00094728 2017-01-05     0   1.1  -2.7
    ##  6 CentralPark_NY USW00094728 2017-01-06    13   0.6  -3.8
    ##  7 CentralPark_NY USW00094728 2017-01-07    81  -3.2  -6.6
    ##  8 CentralPark_NY USW00094728 2017-01-08     0  -3.8  -8.8
    ##  9 CentralPark_NY USW00094728 2017-01-09     0  -4.9  -9.9
    ## 10 CentralPark_NY USW00094728 2017-01-10     0   7.8  -6  
    ## # … with 1,085 more rows

## Revisiting Napolean

``` r
library(rvest)
```

    ## 
    ## Attaching package: 'rvest'

    ## The following object is masked from 'package:readr':
    ## 
    ##     guess_encoding

``` r
read_page_reviews = function(url) {
  
  html = read_html(url)
  
  title = 
    html %>%
    html_nodes("#cm_cr-review_list .review-title") %>%
    html_text()
  
  stars = 
    html %>%
    html_nodes("#cm_cr-review_list .review-rating") %>%
    html_text() %>%
    str_extract("\\d") %>%
    as.numeric()
  
  text = 
    html %>%
    html_nodes(".review-data:nth-child(5)") %>%
    html_text()
  
  tibble(title, stars, text)
}
```

``` r
url_base = "https://www.amazon.com/product-reviews/B00005JNBQ/ref=cm_cr_arp_d_viewopt_rvwer?ie=UTF8&reviewerType=avp_only_reviews&sortBy=recent&pageNumber="
vec_urls = str_c(url_base, 1:5)
```

``` r
map(vec_urls, read_page_reviews)
```

    ## [[1]]
    ## # A tibble: 10 × 3
    ##    title                               stars text                               
    ##    <chr>                               <dbl> <chr>                              
    ##  1 "\n\n\n\n\n\n\n\n  \n  \n    Vinta…     5 "\n\n\n\n\n\n\n\n\n\n  \n  \n    \…
    ##  2 "\n\n\n\n\n\n\n\n  \n  \n    too m…     1 "\n\n\n\n\n\n\n\n\n\n  \n  \n    \…
    ##  3 "\n\n\n\n\n\n\n\n  \n  \n    this …     5 "\n\n\n\n\n\n\n\n\n\n  \n  \n    \…
    ##  4 "\n\n\n\n\n\n\n\n  \n  \n    Good …     5 "\n\n\n\n\n\n\n\n\n\n  \n  \n    \…
    ##  5 "\n\n\n\n\n\n\n\n  \n  \n    I Jus…     5 "\n\n\n\n\n\n\n\n\n\n  \n  \n    \…
    ##  6 "\n\n\n\n\n\n\n\n  \n  \n    the c…     5 "\n\n\n\n\n\n\n\n\n\n  \n  \n    \…
    ##  7 "\n\n\n\n\n\n\n\n  \n  \n    Best …     5 "\n\n\n\n\n\n\n\n\n\n  \n  \n    \…
    ##  8 "\n\n\n\n\n\n\n\n  \n  \n    Class…     5 "\n\n\n\n\n\n\n\n\n\n  \n  \n    \…
    ##  9 "\n\n\n\n\n\n\n\n  \n  \n    heheh…     5 "\n\n\n\n\n\n\n\n\n\n  \n  \n    \…
    ## 10 "\n\n\n\n\n\n\n\n  \n  \n    Painf…     1 "\n\n\n\n\n\n\n\n\n\n  \n  \n    \…
    ## 
    ## [[2]]
    ## # A tibble: 10 × 3
    ##    title                              stars text                                
    ##    <chr>                              <dbl> <chr>                               
    ##  1 "\n\n\n\n\n\n\n\n  \n  \n    GRAN…     5 "\n\n\n\n\n\n\n\n\n\n  \n  \n    \n…
    ##  2 "\n\n\n\n\n\n\n\n  \n  \n    Hell…     5 "\n\n\n\n\n\n\n\n\n\n  \n  \n    \n…
    ##  3 "\n\n\n\n\n\n\n\n  \n  \n    Cult…     5 "\n\n\n\n\n\n\n\n\n\n  \n  \n    \n…
    ##  4 "\n\n\n\n\n\n\n\n  \n  \n    Form…     4 "\n\n\n\n\n\n\n\n\n\n  \n  \n    \n…
    ##  5 "\n\n\n\n\n\n\n\n  \n  \n    Good…     3 "\n\n\n\n\n\n\n\n\n\n  \n  \n    \n…
    ##  6 "\n\n\n\n\n\n\n\n  \n  \n    Not …     1 "\n\n\n\n\n\n\n\n\n\n  \n  \n    \n…
    ##  7 "\n\n\n\n\n\n\n\n  \n  \n    Your…     5 "\n\n\n\n\n\n\n\n\n\n  \n  \n    \n…
    ##  8 "\n\n\n\n\n\n\n\n  \n  \n    Very…     5 "\n\n\n\n\n\n\n\n\n\n  \n  \n    \n…
    ##  9 "\n\n\n\n\n\n\n\n  \n  \n    Watc…     5 "\n\n\n\n\n\n\n\n\n\n  \n  \n    \n…
    ## 10 "\n\n\n\n\n\n\n\n  \n  \n    A cl…     5 "\n\n\n\n\n\n\n\n\n\n  \n  \n    \n…
    ## 
    ## [[3]]
    ## # A tibble: 10 × 3
    ##    title                               stars text                               
    ##    <chr>                               <dbl> <chr>                              
    ##  1 "\n\n\n\n\n\n\n\n  \n  \n    Can't…     5 "\n\n\n\n\n\n\n\n\n\n  \n  \n    \…
    ##  2 "\n\n\n\n\n\n\n\n  \n  \n    I pit…     5 "\n\n\n\n\n\n\n\n\n\n  \n  \n    \…
    ##  3 "\n\n\n\n\n\n\n\n  \n  \n    I don…     2 "\n\n\n\n\n\n\n\n\n\n  \n  \n    \…
    ##  4 "\n\n\n\n\n\n\n\n  \n  \n    Okay\…     3 "\n\n\n\n\n\n\n\n\n\n  \n  \n    \…
    ##  5 "\n\n\n\n\n\n\n\n  \n  \n    A WHO…     5 "\n\n\n\n\n\n\n\n\n\n  \n  \n    \…
    ##  6 "\n\n\n\n\n\n\n\n  \n  \n    Hilar…     5 "\n\n\n\n\n\n\n\n\n\n  \n  \n    \…
    ##  7 "\n\n\n\n\n\n\n\n  \n  \n    Love …     5 "\n\n\n\n\n\n\n\n\n\n  \n  \n    \…
    ##  8 "\n\n\n\n\n\n\n\n  \n  \n    WORTH…     5 "\n\n\n\n\n\n\n\n\n\n  \n  \n    \…
    ##  9 "\n\n\n\n\n\n\n\n  \n  \n    Funny…     5 "\n\n\n\n\n\n\n\n\n\n  \n  \n    \…
    ## 10 "\n\n\n\n\n\n\n\n  \n  \n    Best …     5 "\n\n\n\n\n\n\n\n\n\n  \n  \n    \…
    ## 
    ## [[4]]
    ## # A tibble: 10 × 3
    ##    title                               stars text                               
    ##    <chr>                               <dbl> <chr>                              
    ##  1 "\n\n\n\n\n\n\n\n  \n  \n    I was…     5 "\n\n\n\n\n\n\n\n\n\n  \n  \n    \…
    ##  2 "\n\n\n\n\n\n\n\n  \n  \n    Funny…     5 "\n\n\n\n\n\n\n\n\n\n  \n  \n    \…
    ##  3 "\n\n\n\n\n\n\n\n  \n  \n    Still…     5 "\n\n\n\n\n\n\n\n\n\n  \n  \n    \…
    ##  4 "\n\n\n\n\n\n\n\n  \n  \n    Love …     5 "\n\n\n\n\n\n\n\n\n\n  \n  \n    \…
    ##  5 "\n\n\n\n\n\n\n\n  \n  \n    LOVE …     5 "\n\n\n\n\n\n\n\n\n\n  \n  \n    \…
    ##  6 "\n\n\n\n\n\n\n\n  \n  \n    Perfe…     5 "\n\n\n\n\n\n\n\n\n\n  \n  \n    \…
    ##  7 "\n\n\n\n\n\n\n\n  \n  \n    Love …     5 "\n\n\n\n\n\n\n\n\n\n  \n  \n    \…
    ##  8 "\n\n\n\n\n\n\n\n  \n  \n    Love …     5 "\n\n\n\n\n\n\n\n\n\n  \n  \n    \…
    ##  9 "\n\n\n\n\n\n\n\n  \n  \n    As de…     3 "\n\n\n\n\n\n\n\n\n\n  \n  \n    \…
    ## 10 "\n\n\n\n\n\n\n\n  \n  \n    GOSH!…     5 "\n\n\n\n\n\n\n\n\n\n  \n  \n    \…
    ## 
    ## [[5]]
    ## # A tibble: 10 × 3
    ##    title                              stars text                                
    ##    <chr>                              <dbl> <chr>                               
    ##  1 "\n\n\n\n\n\n\n\n  \n  \n    Watc…     5 "\n\n\n\n\n\n\n\n\n\n  \n  \n    \n…
    ##  2 "\n\n\n\n\n\n\n\n  \n  \n    At t…     5 "\n\n\n\n\n\n\n\n\n\n  \n  \n    \n…
    ##  3 "\n\n\n\n\n\n\n\n  \n  \n    💕\n…     5 "\n\n\n\n\n\n\n\n\n\n  \n  \n    \n…
    ##  4 "\n\n\n\n\n\n\n\n  \n  \n    Good…     5 "\n\n\n\n\n\n\n\n\n\n  \n  \n    \n…
    ##  5 "\n\n\n\n\n\n\n\n  \n  \n    funn…     5 "\n\n\n\n\n\n\n\n\n\n  \n  \n    \n…
    ##  6 "\n\n\n\n\n\n\n\n  \n  \n    Best…     5 "\n\n\n\n\n\n\n\n\n\n  \n  \n    \n…
    ##  7 "\n\n\n\n\n\n\n\n  \n  \n    Vote…     5 "\n\n\n\n\n\n\n\n\n\n  \n  \n    \n…
    ##  8 "\n\n\n\n\n\n\n\n  \n  \n    So F…     5 "\n\n\n\n\n\n\n\n\n\n  \n  \n    \n…
    ##  9 "\n\n\n\n\n\n\n\n  \n  \n    Best…     5 "\n\n\n\n\n\n\n\n\n\n  \n  \n    \n…
    ## 10 "\n\n\n\n\n\n\n\n  \n  \n    Funn…     5 "\n\n\n\n\n\n\n\n\n\n  \n  \n    \n…

``` r
nepolean_df = 
  tibble(
    urls = vec_urls
  )
nepolean_df %>% 
  mutate(reviews = map(urls, read_page_reviews)) %>% 
  select(reviews) %>% 
  unnest()
```

    ## Warning: `cols` is now required when using unnest().
    ## Please use `cols = c(reviews)`

    ## # A tibble: 50 × 3
    ##    title                               stars text                               
    ##    <chr>                               <dbl> <chr>                              
    ##  1 "\n\n\n\n\n\n\n\n  \n  \n    Vinta…     5 "\n\n\n\n\n\n\n\n\n\n  \n  \n    \…
    ##  2 "\n\n\n\n\n\n\n\n  \n  \n    too m…     1 "\n\n\n\n\n\n\n\n\n\n  \n  \n    \…
    ##  3 "\n\n\n\n\n\n\n\n  \n  \n    this …     5 "\n\n\n\n\n\n\n\n\n\n  \n  \n    \…
    ##  4 "\n\n\n\n\n\n\n\n  \n  \n    Good …     5 "\n\n\n\n\n\n\n\n\n\n  \n  \n    \…
    ##  5 "\n\n\n\n\n\n\n\n  \n  \n    I Jus…     5 "\n\n\n\n\n\n\n\n\n\n  \n  \n    \…
    ##  6 "\n\n\n\n\n\n\n\n  \n  \n    the c…     5 "\n\n\n\n\n\n\n\n\n\n  \n  \n    \…
    ##  7 "\n\n\n\n\n\n\n\n  \n  \n    Best …     5 "\n\n\n\n\n\n\n\n\n\n  \n  \n    \…
    ##  8 "\n\n\n\n\n\n\n\n  \n  \n    Class…     5 "\n\n\n\n\n\n\n\n\n\n  \n  \n    \…
    ##  9 "\n\n\n\n\n\n\n\n  \n  \n    heheh…     5 "\n\n\n\n\n\n\n\n\n\n  \n  \n    \…
    ## 10 "\n\n\n\n\n\n\n\n  \n  \n    Painf…     1 "\n\n\n\n\n\n\n\n\n\n  \n  \n    \…
    ## # … with 40 more rows

``` r
output = vector("list", 5)

for (i in 1:5) {
  output[[i]] = read_page_reviews(vec_urls[[i]])
}
```
