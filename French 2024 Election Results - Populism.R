library(ggplot2)
 
   ggplot(france, aes(x = factor(lrscale > 7), y = immigration)) 
       geom_boxplot() +
       labs(x = "Right-wing (proxy RN)", y = "Immigration concern")
model <- glm((lrscale > 7) ~ immigration + economy + education + trust,
                          data = france,
                           family = "binomial")
 
   summary(model)

Call:
  glm(formula = (lrscale > 7) ~ immigration + economy + education + 
        trust, family = "binomial", data = france)

Coefficients:

 france_clean <- france[!is.na(france$lrscale), ]
 library(ggplot2)
 
   france_clean <- france[!is.na(france$lrscale), ]
 
   france_clean$rightwing <- ifelse(france_clean$lrscale > 7, 
                                     +                                  "Right-wing (RN proxy)", 
                                     +                                  "Other voters")

   + ggplot(france_clean, aes(x = rightwing, y = immigration)) +
       geom_boxplot() +
       labs(
             title = "Figure 1: Immigration Concern by Right-Wing Alignment (France)",
             x = "Voter group",
             y = "Immigration concern (0–10 scale)"
         ) +
       theme_minimal()
model

Call:  glm(formula = (lrscale > 7) ~ immigration + economy + education + 
             trust, family = "binomial", data = france)

Coefficients:
  (Intercept)  immigration      economy    education        trust  
-0.08074     -0.39807     -0.02844      0.02598      0.04227  

Degrees of Freedom: 519 Total (i.e. Null);  515 Residual
(89 observations deleted due to missingness)
Null Deviance:	    476 
Residual Deviance: 396.1 	AIC: 406.1
modelsummary(model)
 ggplot(france_clean, aes(x = rightwing, y = economy)) +
       geom_boxplot() +
       labs(
             title = "Figure 2: Economic Dissatisfaction Is More Weakly Structured by Right-Wing Alignment (France)",
             x = "Voter group",
             y = "Satisfaction with the Economy (0–10 scale)"
         ) +
       theme_minimal()
france$imm_culture_threat <- 10 - france$imueclt
 france$imm_econ_threat <- 10 - france$imbgeco
 france$imm_country_threat <- 10 - france$imwbcnt
 
   france$imm_threat_index <- rowMeans(
         france[, c("imm_culture_threat", "imm_econ_threat", "imm_country_threat")],
         na.rm = TRUE
     )
 model1 <- glm((lrscale > 7) ~ imm_threat_index,
                               data = france,
                               family = "binomial")
 
   model2 <- glm((lrscale > 7) ~ imm_threat_index + economy + education + trust,
                                 data = france,
                                 family = "binomial")
 
   model3 <- glm((lrscale > 7) ~ imm_threat_index * trust + economy + education + agea + gndr,
                                 data = france,
                                 family = "binomial")
 
   summary(model1)

Call:
  glm(formula = (lrscale > 7) ~ imm_threat_index, family = "binomial", 
      data = france)

Coefficients:
  Estimate Std. Error z value Pr(>|z|)    
(Intercept)      -4.26289    0.38849 -10.973  < 2e-16 ***
  imm_threat_index  0.50355    0.06266   8.036 9.26e-16 ***
  ---
  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

(Dispersion parameter for binomial family taken to be 1)

Null deviance: 487.33  on 541  degrees of freedom
Residual deviance: 405.71  on 540  degrees of freedom
(67 observations deleted due to missingness)
AIC: 409.71

Number of Fisher Scoring iterations: 5

 summary(model2)

Call:
  glm(formula = (lrscale > 7) ~ imm_threat_index + economy + education + 
        trust, family = "binomial", data = france)

Coefficients:
  Estimate Std. Error z value Pr(>|z|)    
(Intercept)      -4.70578    0.83033  -5.667 1.45e-08 ***
  imm_threat_index  0.51279    0.06916   7.415 1.22e-13 ***
  economy          -0.03378    0.06910  -0.489    0.625    
education         0.02620    0.04217   0.621    0.534    
trust             0.05415    0.06409   0.845    0.398    
---
  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

(Dispersion parameter for binomial family taken to be 1)

Null deviance: 477.89  on 524  degrees of freedom
Residual deviance: 398.85  on 520  degrees of freedom
(84 observations deleted due to missingness)
AIC: 408.85

Number of Fisher Scoring iterations: 5

 summary(model3)

Call:
  glm(formula = (lrscale > 7) ~ imm_threat_index * trust + economy + 
        education + agea + gndr, family = "binomial", data = france)

Coefficients:
  Estimate Std. Error z value Pr(>|z|)    
(Intercept)            -2.9498032  1.1413917  -2.584 0.009755 ** 
  imm_threat_index        0.3932845  0.1075192   3.658 0.000254 ***
  trust                  -0.1819162  0.1797704  -1.012 0.311568    
economy                -0.0318461  0.0706882  -0.451 0.652339    
education               0.0268079  0.0443897   0.604 0.545896    
agea                    0.0002049  0.0089571   0.023 0.981754    
gndr                   -0.6790458  0.2620965  -2.591 0.009575 ** 
  imm_threat_index:trust  0.0388248  0.0281126   1.381 0.167265    
---
  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

(Dispersion parameter for binomial family taken to be 1)

Null deviance: 477.89  on 524  degrees of freedom
Residual deviance: 389.76  on 517  degrees of freedom
(84 observations deleted due to missingness)
AIC: 405.76

Number of Fisher Scoring iterations: 5

 library(ggplot2)
 
   france_clean$rightwing <- ifelse(france_clean$lrscale > 7,
                                                                       "Right-wing voters (proxy for RN support)",
                                                                       "Other voters")
 
   ggplot(france_clean,
                   aes(x = rightwing,
                                    y = imm_threat_index)) +
       geom_boxplot() +
      labs(
             title = "Figure 1: Perceived Immigration Threat Is More Strongly Associated with Right-Wing Alignment (France, ESS 2024)",
             x = "Voter group",
            y = "Perceived immigration threat (0–10 scale)"
         ) +
       theme_minimal()

 ggplot(france_clean,
                aes(x = rightwing,
                               y = imm_threat_index,
                                  fill = rightwing)) +
       geom_boxplot() +
     scale_fill_manual(values = c(
             "Other voters" = "grey80",
            "Right-wing voters (proxy for RN support)" = "steelblue"
         )) +
       labs(
             title = "Figure 1: Perceived Immigration Threat Is More Strongly Associated with Right-Wing Alignment (France, ESS 2024)",
             x = "Voter group",
             y = "Perceived immigration threat (0–10 scale)"
         ) +
       theme_minimal() +
       theme(legend.position = "none")
model2 <- glm(
       (lrscale > 7) ~ imm_threat_index + economy + education + trust,
       data = france,
       family = "binomial"
   )
 
   summary(model2)

Call:
  glm(formula = (lrscale > 7) ~ imm_threat_index + economy + education + 
        trust, family = "binomial", data = france)

Coefficients:
  Estimate Std. Error z value Pr(>|z|)    
(Intercept)      -4.70578    0.83033  -5.667 1.45e-08 ***
  imm_threat_index  0.51279    0.06916   7.415 1.22e-13 ***
  economy          -0.03378    0.06910  -0.489    0.625    
education         0.02620    0.04217   0.621    0.534    
trust             0.05415    0.06409   0.845    0.398    
---
  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

(Dispersion parameter for binomial family taken to be 1)

Null deviance: 477.89  on 524  degrees of freedom
Residual deviance: 398.85  on 520  degrees of freedom
(84 observations deleted due to missingness)
AIC: 408.85

Number of Fisher Scoring iterations: 5

 model3 <- glm(
       (lrscale > 7) ~ imm_threat_index + economy +
             education + trust + agea + gndr,
       data = france,
       family = "binomial"
   )
 
   summary(model3)

Call:
  glm(formula = (lrscale > 7) ~ imm_threat_index + economy + education + 
        trust + agea + gndr, family = "binomial", data = france)

Coefficients:
  Estimate Std. Error z value Pr(>|z|)    
(Intercept)      -3.686e+00  1.023e+00  -3.602 0.000316 ***
  imm_threat_index  5.134e-01  7.040e-02   7.293 3.04e-13 ***
  economy          -3.807e-02  7.033e-02  -0.541 0.588325    
education         2.780e-02  4.393e-02   0.633 0.526887    
trust             5.022e-02  6.524e-02   0.770 0.441453    
agea             -3.959e-06  8.933e-03   0.000 0.999646    
gndr             -6.866e-01  2.621e-01  -2.619 0.008812 ** 
  ---
  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

(Dispersion parameter for binomial family taken to be 1)

Null deviance: 477.89  on 524  degrees of freedom
Residual deviance: 391.73  on 518  degrees of freedom
(84 observations deleted due to missingness)
AIC: 405.73

Number of Fisher Scoring iterations: 5

 model4 <- glm(
       (lrscale > 7) ~ imm_threat_index * trust +
             economy + education + agea + gndr,
       data = france,
       family = "binomial"
   )
  
   summary(model4)

Call:
  glm(formula = (lrscale > 7) ~ imm_threat_index * trust + economy + 
        education + agea + gndr, family = "binomial", data = france)

Coefficients:
  Estimate Std. Error z value Pr(>|z|)    
(Intercept)            -2.9498032  1.1413917  -2.584 0.009755 ** 
  imm_threat_index        0.3932845  0.1075192   3.658 0.000254 ***
  trust                  -0.1819162  0.1797704  -1.012 0.311568    
economy                -0.0318461  0.0706882  -0.451 0.652339    
education               0.0268079  0.0443897   0.604 0.545896    
agea                    0.0002049  0.0089571   0.023 0.981754    
gndr                   -0.6790458  0.2620965  -2.591 0.009575 ** 
  imm_threat_index:trust  0.0388248  0.0281126   1.381 0.167265    
---
  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

(Dispersion parameter for binomial family taken to be 1)

Null deviance: 477.89  on 524  degrees of freedom
Residual deviance: 389.76  on 517  degrees of freedom
(84 observations deleted due to missingness)
AIC: 405.76

Number of Fisher Scoring iterations: 5

 install.packages("modelsummary")

 library(modelsummary)
 
   modelsummary(
         list(
               "Model 1" = model1,
               "Model 2" = model3,
               "Model 3" = model4
           ),
         output = "table1.docx"
     )modelsummary(
           list(
                 "Model 1" = model1,
                 "Model 2" = model3,
                 "Model 3" = model4
             )
       )
 modelsummary(
       list(
             "Model 1" = model1,
             "Model 2" = model3,
             "Model 3" = model4
         ),
       stars = TRUE
   )
ggplot(france_clean,
                 aes(x = rightwing,
                                  y = trust,
                                  fill = rightwing)) +
geom_boxplot() 
       scale_fill_manual(values = c(
             "Other voters" = "grey80",
             "Right-wing voters (proxy for RN support)" = "steelblue"
         )) +
       labs(
             title = "Figure 3: Political Distrust Is More Strongly Associated with Right-Wing Alignment (France, ESS 2024)",
             x = "Voter group",
             y = "Trust in politicians (0–10 scale)"
         ) +
       theme_minimal() +
       theme(legend.position = "none")
model2 <- glm(
       (lrscale > 7) ~ imm_threat_index + economy + education + trust,
       data = france,
       family = "binomial"
   )
 
   summary(model2)

Call:
  glm(formula = (lrscale > 7) ~ imm_threat_index + economy + education + 
        trust, family = "binomial", data = france)

Coefficients:
  Estimate Std. Error z value Pr(>|z|)    
(Intercept)      -4.70578    0.83033  -5.667 1.45e-08 ***
  imm_threat_index  0.51279    0.06916   7.415 1.22e-13 ***
  economy          -0.03378    0.06910  -0.489    0.625    
education         0.02620    0.04217   0.621    0.534    
trust             0.05415    0.06409   0.845    0.398    
---
  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

(Dispersion parameter for binomial family taken to be 1)

Null deviance: 477.89  on 524  degrees of freedom
Residual deviance: 398.85  on 520  degrees of freedom
(84 observations deleted due to missingness)
AIC: 408.85

Number of Fisher Scoring iterations: 5

 model3 <- glm(
       (lrscale > 7) ~ imm_threat_index + economy +
             education + trust + agea + gndr,
     data = france,
       family = "binomial"
   )
 
   summary(model3)

Call:
  glm(formula = (lrscale > 7) ~ imm_threat_index + economy + education + 
        trust + agea + gndr, family = "binomial", data = france)

Coefficients:
  Estimate Std. Error z value Pr(>|z|)    
(Intercept)      -3.686e+00  1.023e+00  -3.602 0.000316 ***
  imm_threat_index  5.134e-01  7.040e-02   7.293 3.04e-13 ***
  economy          -3.807e-02  7.033e-02  -0.541 0.588325    
education         2.780e-02  4.393e-02   0.633 0.526887    
trust             5.022e-02  6.524e-02   0.770 0.441453    
agea             -3.959e-06  8.933e-03   0.000 0.999646    
gndr             -6.866e-01  2.621e-01  -2.619 0.008812 ** 
  ---
  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

(Dispersion parameter for binomial family taken to be 1)

Null deviance: 477.89  on 524  degrees of freedom
Residual deviance: 391.73  on 518  degrees of freedom
(84 observations deleted due to missingness)
AIC: 405.73

Number of Fisher Scoring iterations: 5

 model4 <- glm(
       (lrscale > 7) ~ imm_threat_index * trust +
             economy + education + agea + gndr,
     data = france,
       family = "binomial"
   )
 
   summary(model4)

Call:
  glm(formula = (lrscale > 7) ~ imm_threat_index * trust + economy + 
        education + agea + gndr, family = "binomial", data = france)

Coefficients:
  Estimate Std. Error z value Pr(>|z|)    
(Intercept)            -2.9498032  1.1413917  -2.584 0.009755 ** 
  imm_threat_index        0.3932845  0.1075192   3.658 0.000254 ***
  trust                  -0.1819162  0.1797704  -1.012 0.311568    
economy                -0.0318461  0.0706882  -0.451 0.652339    
education               0.0268079  0.0443897   0.604 0.545896    
agea                    0.0002049  0.0089571   0.023 0.981754    
gndr                   -0.6790458  0.2620965  -2.591 0.009575 ** 
  imm_threat_index:trust  0.0388248  0.0281126   1.381 0.167265    
---
  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

(Dispersion parameter for binomial family taken to be 1)

Null deviance: 477.89  on 524  degrees of freedom
Residual deviance: 389.76  on 517  degrees of freedom
(84 observations deleted due to missingness)
AIC: 405.76

Number of Fisher Scoring iterations: 5

 install.packages("modelsummary")

 library(modelsummary)
 
   modelsummary(
         list(
               "Model 1" = model1,
               "Model 2" = model3,
               "Model 3" = model4
           ),
         output = "table1.docx"
     )modelsummary(
           list(
                 "Model 1" = model1,
                 "Model 2" = model3,
                 "Model 3" = model4
             )
       )
 modelsummary(
       list(
             "Model 1" = model1,
             "Model 2" = model3,
             "Model 3" = model4
         ),
       stars = TRUE
   )
 + ggplot(france_clean,
                 aes(x = rightwing,
                                  y = trust,
                                  fill = rightwing)) +
       geom_boxplot() +
      scale_fill_manual(values = c(
             "Other voters" = "grey80",
         "Right-wing voters (proxy for RN support)" = "steelblue"
   )) +
  labs(
            title = "Figure 3: Political Distrust Is More Strongly Associated with Right-Wing Alignment (France, ESS 2024)",
             x = "Voter group",
             y = "Trust in politicians (0–10 scale)"
         ) +
      theme_minimal() +
       theme(legend.position = "none"