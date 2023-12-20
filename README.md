# Manufactory Profits

## Objective

This project primarily objective is to adjust different SARIMA and ETS models to a Time Series from the M3 competition, in order to find the best fitting one.


## Justification

The M3 competition is the most well-known Time Series competition in the world, and it's famous for the data diversity and innovations the dispute provides.

Considering this fact, Professor Jose Augusto Fiorucci suggested a challenge for me to tackle, to try and use different techniques and models to analyze a Time Series from the M3 Competition. He then pointed out a specific Time Series that contained data about the weekly mean profits of the USA manufacturing industry, which is the dataset that this project is based on.


## Choices and Metodology

Since the data is provided by the M3 competition, it is retrievable via the *MComp* R package, and can be found by the id 2726 on the "M3" array object.Considering the source, the data already came in perfect conditions, with no need of direct data cleaning or transformation. However, considering it is a real Time Series, trend and seasonal effects were almost unavoidable; which it indeed had, and some treatment was necessary in order to adjust the data for modeling.

In total, 28 SARIMA and ETS models were created, with an additional "M" model created by the mean of the best SARIMA and ETS models found. Many techniques were applied during the analysis, which are detailed in the "Trabalho2\_MatheusErbisti.Rmd" file, that can be used to produce a PDF file for better reading. Even so, please note two requirements in order to successfully knit the PDF:

* Make sure to have the dependant files in the same directory as the .Rmd file;
* Change the working directory to the correct one on your computer;

Be warned, the whole analysis is written in Portuguese. A translator should solve this issue for other languages readers, but I am available for contacting should any questions arise.


## License and Contact

This project was created by Matheus Erbisti, with guidance from Professor Jose Augusto Fiorucci. It falls under the MIT License, which means you are free to use and adapt this code at your will, just make sure to reference us!

If you need to make contact about this project, you can reach out to me on LinkedIn (https://www.linkedin.com/in/matheus-erbisti-b74168172/) or via e-mail at matheuserbisti@hotmail.com.


Thank you for your attention!
