WEEK-11
================
Sneha
2024-11-15

``` r
library(tidyverse)
```

    ## ── Attaching core tidyverse packages ──────────────────────── tidyverse 2.0.0 ──
    ## ✔ dplyr     1.1.4     ✔ readr     2.1.5
    ## ✔ forcats   1.0.0     ✔ stringr   1.5.1
    ## ✔ ggplot2   3.5.1     ✔ tibble    3.2.1
    ## ✔ lubridate 1.9.3     ✔ tidyr     1.3.1
    ## ✔ purrr     1.0.2     
    ## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ## ✖ dplyr::filter() masks stats::filter()
    ## ✖ dplyr::lag()    masks stats::lag()
    ## ℹ Use the conflicted package (<http://conflicted.r-lib.org/>) to force all conflicts to become errors

``` r
library(ggthemes)
library(ggrepel)
library(broom)
library(lindia)
library(car)
```

    ## Loading required package: carData
    ## 
    ## Attaching package: 'car'
    ## 
    ## The following object is masked from 'package:dplyr':
    ## 
    ##     recode
    ## 
    ## The following object is masked from 'package:purrr':
    ## 
    ##     some

``` r
options(scipen = 6)
```

``` r
astro <- read_delim('/Users/sneha/H510-Statistics/astronaut-data.csv')
```

    ## Rows: 1277 Columns: 23
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr (10): name, sex, nationality, military_civilian, selection, occupation, ...
    ## dbl (13): id, number, nationwide_number, year_of_birth, year_of_selection, m...
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

Building a Linear model :

Here i am using sex and military/civilian status as predictors and
hours_mission as response variable.

``` r
model <- lm(hours_mission ~ sex + military_civilian, data = astro)
```

``` r
summary(model)
```

    ## 
    ## Call:
    ## lm(formula = hours_mission ~ sex + military_civilian, data = astro)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -1103.1  -885.1  -795.1  -563.3  9401.9 
    ## 
    ## Coefficients:
    ##                           Estimate Std. Error t value     Pr(>|t|)    
    ## (Intercept)                 872.48     145.76   5.986 0.0000000028 ***
    ## sexmale                     230.62     157.12   1.468        0.142    
    ## military_civilianmilitary   -43.83     101.23  -0.433        0.665    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 1715 on 1274 degrees of freedom
    ## Multiple R-squared:  0.001692,   Adjusted R-squared:  0.0001249 
    ## F-statistic:  1.08 on 2 and 1274 DF,  p-value: 0.34

- The residuals show a large spread, with values ranging from -1103.1 to
  9401.9. This suggests that the model has a significant variance in the
  errors, which could indicate that the model isn’t capturing the
  patterns well.

**Coefficients**:

- The **Intercept** of 872.48 means that for a female civilian (the
  baseline categories for sex and military/civilian are female and
  civilian), the estimated average hours spent on a mission is around
  872 hours.

- The **sexmale** coefficient (230.62) suggests that being male is
  associated with an increase in mission hours by about 230 hours, but
  the **p-value** (0.142) is greater than 0.05, which indicates that
  this effect is **not statistically significant**.

- The **military_civilian (military)** coefficient (-43.83) suggests
  that military personnel, on average, spend fewer hours on missions
  compared to civilians. However, the **p-value** (0.665) is also large,
  indicating that this difference is not statistically significant.

  #### Hence i am going to change the explanatory variables: Trying a logistic model this time

``` r
astro$military_binary <- ifelse(astro$military_civilian == "military", 1, 0)
```

I am going to change the occupation column data to “pilot” and others,
just to see if there is any connection between occupation and
military/civilian status

``` r
astro$occupation_skewed <- ifelse(astro$occupation == "pilot", "pilot", "other")
```

``` r
logistic_model <- glm(military_binary ~ sex + occupation_skewed, data = astro, family = binomial)
```

``` r
summary(logistic_model)
```

    ## 
    ## Call:
    ## glm(formula = military_binary ~ sex + occupation_skewed, family = binomial, 
    ##     data = astro)
    ## 
    ## Coefficients:
    ##                        Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept)             -1.1744     0.1967  -5.971 2.35e-09 ***
    ## sexmale                  1.5442     0.2063   7.483 7.24e-14 ***
    ## occupation_skewedpilot   2.0181     0.2575   7.837 4.61e-15 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 1716.6  on 1276  degrees of freedom
    ## Residual deviance: 1544.6  on 1274  degrees of freedom
    ## AIC: 1550.6
    ## 
    ## Number of Fisher Scoring iterations: 4

**Intercept** (-1.1744):

- The intercept represents the log-odds of being in the military
  (military_binary = 1) when both sex is female and occupation is other.

- The negative value (-1.1744) suggests that the odds of being in the
  military are lower when the person is female and in an occupation
  other than “pilot”.

  This is a great insight!

  **sexmale** (1.5442):

  The positive coefficient (1.5442) for sex-male means that being male
  increases the log-odds of being in the military compared to females,
  assuming all other factors are constant. which is true considering the
  fact that, males have higher odds of being in the military.

  The **p-value** (7.24e-14) is highly significant, indicating that sex
  is a statistically significant predictor of military membership.

  **occupation_skewedpilot** (2.0181):

  The coefficient for occupation-pilot indicates that being a pilot
  significantly increases the log-odds of being in the military.

  The **p-value** (4.61e-15) is also highly significant, suggesting that
  occupation (pilot) plays a strong role in determining military
  membership.

  This is also a great insight!

**Residual deviance**: 1544.6, which represents the deviance of the
model with the predictors. The reduction in deviance suggests that the
model is a better fit that the linear model before.

#### **Model Diagnostics (linear model)**

1)Akaike Information Criterion (AIC)

``` r
aic_value <- AIC(model)
print(paste("AIC of the model: ", aic_value))
```

    ## [1] "AIC of the model:  22648.5641741986"

``` r
aic_value_2 <- AIC(logistic_model)
print(paste("AIC of the model: ", aic_value_2))
```

    ## [1] "AIC of the model:  1550.60295673977"

As we know, **Lower AIC values indicate a better model**. Hence it is
evident that my logistic model with is a better model in explaining the
data well without overfitting.

2\. ANOVA - Analysis of Variance

``` r
anova_model <- anova(model)
print(anova_model)
```

    ## Analysis of Variance Table
    ## 
    ## Response: hours_mission
    ##                     Df     Sum Sq Mean Sq F value Pr(>F)
    ## sex                  1    5797530 5797530  1.9719 0.1605
    ## military_civilian    1     551267  551267  0.1875 0.6651
    ## Residuals         1274 3745743827 2940144

Neither sex nor military/civilian status significantly explains the
variation in hours_mission, as indicated by the high p-values (both are
greater than 0.05).

The residual variation is much larger than the variation explained by
either predictor, suggesting that these predictors do not capture much
of the variation in mission hours.

Now, lets see how our logistic model works:

``` r
anova_model_two <- anova(logistic_model)
print(anova_model_two)
```

    ## Analysis of Deviance Table
    ## 
    ## Model: binomial, link: logit
    ## 
    ## Response: military_binary
    ## 
    ## Terms added sequentially (first to last)
    ## 
    ## 
    ##                   Df Deviance Resid. Df Resid. Dev  Pr(>Chi)    
    ## NULL                               1276     1716.6              
    ## sex                1   78.436      1275     1638.1 < 2.2e-16 ***
    ## occupation_skewed  1   93.536      1274     1544.6 < 2.2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

From anova_model_two, Both **sex and occupation are highly significant**
predictors of military membership, with very small p-values (\<
2.2e-16), indicating that these variables significantly reduce the
deviance in the model.

This analysis confirms that gender (male vs. female) and occupation
(pilot vs. other) are important factors in determining military
membership.

We can use the correlations to capture collinearity by gauging the
magnitudes of the correlation coefficients between variables (From
notes-11)

``` r
vif_values <- vif(model)
print(vif_values)
```

    ##               sex military_civilian 
    ##          1.066204          1.066204

``` r
vif_values_two <- vif(logistic_model)
print(vif_values_two)
```

    ##               sex occupation_skewed 
    ##          1.000017          1.000017

Both models VIF values are essentially identical and very close to 1,
which means there is no multicollinearity between these predictors.

A VIF of 1 indicates that the predictor does not have any linear
relationship with other predictors in the model.

### **Interpretations of coefficients:**

trying to check confident intervals because confidence intervals help
you understand the range in which the true coefficient lies with a
certain level of confidence

``` r
conf_intervals <- confint(model, level = 0.95)
print("95% Confidence Intervals for Coefficients:")
```

    ## [1] "95% Confidence Intervals for Coefficients:"

``` r
print(conf_intervals)
```

    ##                                2.5 %    97.5 %
    ## (Intercept)                586.52354 1158.4423
    ## sexmale                    -77.61572  538.8605
    ## military_civilianmilitary -242.42690  154.7608

Since both CI for both coefficients have zero, this means that their
effects may not be significant predictors in our model.

trying out the second logistic model

``` r
conf_intervals_two <- confint(logistic_model, level = 0.95)
```

    ## Waiting for profiling to be done...

``` r
print("95% Confidence Intervals for Coefficients:")
```

    ## [1] "95% Confidence Intervals for Coefficients:"

``` r
print(conf_intervals_two)
```

    ##                            2.5 %     97.5 %
    ## (Intercept)            -1.572996 -0.7997889
    ## sexmale                 1.149389  1.9603556
    ## occupation_skewedpilot  1.540325  2.5556478

The 95% CI for the intercept does not include 0, indicating that the
intercept is **statistically significant**. We can be 95% confident that
the true intercept value lies between -1.57 and -0.80.

The The 95% CI for both predictors does not include 0, indicating that
both variables sex and occupation are significant. These are strong
predictors, with relatively wide confidence intervals, suggesting a
meaningful relationship with the outcome variable(military_binary).
