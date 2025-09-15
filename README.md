# Forecasting `[ECON 4753]`

Fall 2024 • Instructor: Kyle Butts

Monday, Wednesday 3:05 PM - 4:20 PM at Kimpell Hall 206A

Office Hours: Monday, Wednesday 11am -- 1pm at WCOB 408

> [!IMPORTANT]
> 
> [Even Snoop Dogg wants you to read the syllabus](https://www.cameo.com/recipient/5f2b392a0299b100202e624a)


## Course Summary

This course will provide an introduction to forecasting methods. The class will teach you how to take a set of input variables and produce predictions of some outcome variable. We will survey a set of forecasting methods for your toolbox including: bivariate and multivariate regression; smoothing methods; time-series regression; and ARIMA methods. The class will teach these methods theoretically and also teach you to estimate these models in the `R` programming language.

Though the class will also teach you fundamental principles of forecasting: goals of forecasting, fitting of models, evaluating model fit, and limitations of the models. By doing this, the class will equip you with the foundations to expand your toolbox over time. 

Last, the course will try to highlight limitations of forecasting methods; trade-offs between forecasting methods (e.g. interpretability versus predictive accuracy); and help you understand what forecasting methods can not due (e.g. establish causality). 

## Course Materials

### Textbook

The class will pull materials from two textbooks that are freely available online. You may buy a print version, but it is not necessary for the course. 

- Gareth, J., Daniela, W., Trevor, H., \& Robert, T. (2013). "[An introduction to statistical learning: with applications in R 2nd ed.](https://www.statlearning.com)". Spinger.

- Hyndman, R. J., \& Athanasopoulos, G. (2018). "[Forecasting: principles and practice 3rd ed.](https://otexts.com/fpp3/)". OTexts.

I refer to the first as "ISLR" and the second as "FPP3".

In addition, we may have readings from different articles. These will be available in pdf form on Blackboard.

### Coding Software

You will need to download *two* programs:
1. Install R from <https://cloud.r-project.org/>.
2. Install RStudio Desktop from <https://posit.co/download/rstudio-desktop/>

Mastering `R` will take time and dedication, but it is a powerful and adaptable tool that is highly valued by many employers. Invest the necessary effort and time, and you will see the benefits.

Your first assignment will be to download the software and compile an `.Rmd` file.

## Assignments and Exams

You will have a set of homework assignments in this course that correspond with topics. The questions will be a mix of free-response and coding problems. For full-credit, the code and the output of the code must be submitted. We will discuss how to do so in the class. 

There will be two midterms and one final in this course. The final exam will be on Wednesday, December 11th from 3 to 5pm. Each will be worth 25\% of your grade with assignments filling the remaining 25\%. The breakdown is as follows:

| Assignments | Percent of grade |
|-------------|------------------|
| Homework    | 35%              |
| Midterm     | 20%              |
| Midterm     | 20%              |
| Final       | 25%              | 


## Course Outline

#### Introduction to Forecasting 

<!-- A short topic introducing the goal of forecasting, building vocabulary, and giving many examples of forecasting in the world.  -->

*Readings*: 
- The Signal and the Noise Introduction
- https://www.youtube.com/watch?v=z4zhI9uLs4U

#### Topic 2: Fundamentals of Basic Algebra, Probability and Statistics

<!-- This topic will review important concepts in algebra, probability, and statistics that will be essential for the remainder of the course. Since this is a review, the material will be covered somewhat quickly. While covering the material, the R programming language will be introduced and basic skills taught. During this topic, you will be expected to download and setup your R coding environment. -->

*Readings*: 
- Review notes on Algebra
- Review notes on Probability
- Review notes on Statistics 

*Labs*:
- Introduction to R

#### Topic 2: An Introduction to Forecasting Techniques and Exploring Data Patterns

*Readings*:
- ISLR 2.1 intro, 2.1.1, 2.1.2, 2.1.3
- ISLR 2.2 intro, 2.2.1, 2.2.2

#### Topic 3: Simple Linear Regression

*Readings*: 
- ISLR 3 intro, 3.1 intro, 3.1.1, 3.1.2, 3.1.3
- ISLR 3.3.1, 3.5
- ILSR 7 intro, 7.1, 7.2

#### Topic 4: Multiple Regression Analysis

*Readings*: 
- ISLR 3.2 (no `Deciding on Important Variables'), 3.3
- ILSR 7 intro, 7.1, 7.2

#### Topic 5: Regression with Time Series Data 

*Readings*:
- Time-series regression predictors: FPP3 7.1, 7.2, 7.3, 7.4, 7.6, 7.7

#### Topic 6: Smoothing Methods for Time Series

*Readings*:
- Introduction to Forecasting: FPP3 1.7
- Autocorrelation: FPP3 2.8
- Smoothing Averages: FPP3 3.3, 8.1
- Time-series Decomposition: FPP3 3.4, 3.2
- Prophet Model: FPP3 12.2 and [Introduction to Prophet](https://www.youtube.com/watch?v=pOYAXv15r3A)


*Note:* The first time I taught this, I did smoothing methods first and then time-series. I think I am going to switch it next time. Time-series regression first; then smoothing methods; and end with [Prophet](https://facebook.github.io/prophet/)



### Tentative Schedule

This is a tentative schedule. This is the first time I've taught this course, so take this with a heavy dose of skepticism. In particular, do not set up holidays a class before or after the midterm. 

<!-- Schedule -->
| Week | Dates | Monday | Wednesday | Assignments |
|----|----|----|----|----|
| 1 | 08/18 - 08/20 | Syllabus + Intro | Stats Review | R Lab 0, *Sunday* |
| 2 | 08/25 - 08/27 | Stats Review (Class Cancelled) | Stats Review |  |
| 3 | 09/01 - 09/03 | No Class | R Lab 1 – *Introduction to R* |  |
| 4 | 09/08 - 09/10 | R Lab 1 – *Introduction to R* | R Lab 2 – *Working with data in R* | R Lab 1, *Sunday* |
| 5 | 09/15 - 09/17 | Stats Review |  | R Lab 2, *Sunday* |
| 6 | 09/22 - 09/24 | Introduction to Forecasting | Simple Linear Regression |  |
| 7 | 09/29 - 10/01 | Simple Linear Regression | Simple Linear Regression | R Lab 3, *Sunday* |
| 8 | 10/06 - 10/08 | Multiple Regression Analysis | Multiple Regression Analysis |  |
| 9 | 10/13 - 10/15 | No Class | Multiple Regression Analysis |  |
| 10 | 10/20 - 10/22 | Midterm | R Lab 4 – \*Data Analysis Project in R | R Lab 4, *Sunday* |
| 11 | 10/27 - 10/29 | Time Series Smoothing Methods | R Lab 5 – *Intro to working with Time-Series Data in R* | R Lab 4, *Sunday* |
| 12 | 11/03 - 11/05 | Time Series Smoothing Methods | Regression with Time Series Data | R Lab 5, *Sunday* |
| 13 | 11/10 - 11/12 | Regression with Time Series Data | Regression with Time Series Data |  |
| 14 | 11/17 - 11/19 | R Lab 6 – *Time-series Regression in R* | Midterm | R Lab 6, *Sunday* |
| 15 | 11/24 - 11/26 | R Lab 7 – *Time-series Data Analysis Project* | No Class |  |
| 16 | 12/01 - 12/03 | R Lab 7 – *Time-series Data Analysis Project* | Review | R Lab 7, *Sunday* |
| Final | TBD |  |  | Final Exam |
<!-- Schedule -->

