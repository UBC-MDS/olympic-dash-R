# olympic-dash

_**An interactive dashboard illustrating historic olympic data and trends**_

## Welcome!

Thank you for visiting the olympic-dash project repository!

This README file is a hub to give you some information about the project. Jump straight to one of the sections below, or just scroll down to find out more.

* [What Are We Doing?](#what-are-we-doing)
* [Description of The Dashboard](#description-of-the-dashboard)
* [Proposed Sketch](#proposed-sketch)
* [Contribute to This Dashboard](#contribute-to-this-dashboard)
* [Contributors](#contributors)
* [Code of Conduct](#code-of-conduct)
* [License](#license)

## What Are We Doing?

> “The goal of the Olympic Movement is to contribute to building a peaceful and better world by educating youth through sport practiced without discrimination of any kind and in the Olympic spirit, which requires mutual understanding with a spirit of friendship, solidarity and fair play.” 
> 
> -- <cite>International Olympic Committee</cite>

We propose building a visualization app to allow IOC members (and members of the general public) to explore historical Olympic results to review the success trends of various countries to determine abnormal performance trends and which countries could be future high medal achievers. All in the interest of making the olympic games more competitive and reward the countries that are developing their athletes and improving their performance in the games.

## Description of The Dashboard

To explore the current dashboard, please click [here](https://olympic-dash-r-22.herokuapp.com/).

This app contains a dashboard which visualizes Olympic data from 1896 up until 2016. 
Key metrics of interest will be displayed including: 
- Medals earned per country
- Medals earned depending on athlete age
- Athlete height based on Olympic events 
- Medals per country will be displayed 

Medals earned per country will be displayed via a bubble chart. Countries
are color coded by the IOC continent region they belong to.

A bar chart will show the medals earned for each athlete age bracket, while a histogram 
will show the distribution of athlete heights based on the event selected. These figures 
will contain a slider allowing users to adjust athlete age ranges and a dropdown list, 
allowing users to select the event visualized respectively. 

Lastly, a line graph will display the number of medals earned by countries over time. This 
figure will be accompanied by a dropdown list allowing users to select a subset of countries 
to display on the graph. 

Radio buttons on the side of the dashboard will allow for filtering of 
summer/winter Olympics data, in addition to allowing for users to filter data 
by the type of medals. Additionally a year slider will allow for users to filter 
data in the bubble chart and both histograms by year. Using these filters, users 
will be able to investigate trends in Olympic success between countries, medal types, 
athlete demographics, and more.

## Example Usage

![Alt text](img/dash-R-usage.gif)

## Contribute to This Dashboard

You are welcome to contribute to olympic-dash if you have any idea regarding to this dashboard. Please go through the [contributing guidelines](CONTRIBUTING.md) for the recommended ways if you want to contribute or report/fix any existing bugs.

### How to install and run locally

Run the following command at the root directory of the project:

1. Copy and paste the following link: git clone https://github.com/UBC-MDS/olympic-dash-R.git to your Terminal.

2. On your terminal, type: cd olympic-dash-R.

3. To run an app instance locally, first install the dependencies by typing: Rscript init.R

4. Launch app.R by typing: Rscript app.R

5. Using any modern web browser, visit http://127.0.0.1:8050/ to access the app.


## Contributors

This app was developed by the following contributors:

|  Contributor  |  Github Username |
|--------------|------------------|
|  Allyson Stoll |  @datallurgy |
|  Helin Wang |  @helingogo  |
|  Rubén De la Garza Macías  |  @ruben1dlg |
|  Andy Yang  |  @AndyYang80  |

## Code of Conduct

In the interest of fostering an open and welcoming environment, we as contributors and maintainers pledge to making participation in our project and our community a harassment-free experience for everyone, regardless of age, body size, disability, ethnicity, gender identity and expression, level of experience, nationality, personal appearance, race, religion, or sexual identity and orientation.
For more details, see our [code of conduct.](CONDUCT.md)

## License

`olympic-dash-R` was created by Allyson Stoll, Helin Wang, Rubén De la Garza Macías and Song Bo Andy Yang . It is licensed under the terms of the MIT license.
