# Examples of Real life data machine learning projects
---

# Project 1:CREDIT RISK MODELLING:Overview

* Problem definition: Given a borrower's characteristics, can we predict itsprobability of default

* Data: We use the lending club data set provided by Kaggle.The original data set can be found here https://www.kaggle.com/wordsforthewise/lending-club

* Methodology:

  `PD`(probability of defaut) using a Logistic regression

  `EAD`(Exposure at Default) Using a linear regression

  `LGD`(Lost Given Default) using a linear regression

* For our PD Model, The variable loan Status will be our target. The model will predict a customer's PD
  notably whether he has defaulted or not giving certains features.

* For the LGD Model. We need to calculate how much of a loan was recovered after the borrower had defaulted.
   To do so, our target will be the variable `Recovery`.

* For the EAD Model, We have to calculate the total exposure at the moment the borrower defaulted compared to
  the total exposure in the past. The target will be the total recovery principal variable will be our target for this Model.

 ---

# Project 2: BULLDOZER SALE PRICE PREDICTION USING LINEAR REGRESSION:Overview

* Problem definition: given bulldozers characteristics and previous examples of how much similar bulldozers
  have been sold for, can we predict the future sale price of a bulldozer

* Data: There are 3 datasets: Train.csv - Historical bulldozer sales examples up to 2011 (close to 400,000
  examples with 50+ different attributes, including SalePrice which is the target variable). Valid.csv - Historical bulldozer sales examples from January 1 2012 to April 30 2012 (close to 12,000 examples with the same attributes as Train.csv). Test.csv - Historical bulldozer sales examples from May 1 2012 to November 2012 (close to 12,000 examples but missing the SalePrice attribute, as this is what we’ll be trying to predict).

* Methodology:Created a linear regression model that predict the future sale price. Trained and test the
  model with sales data up to 2011 and validated the model with sales data from May 1 2012 to November 2012.

---
