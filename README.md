# Streeteasy Sales and Rental Dashboard

This Shiny app provides buyers, sellers and investors insights into the New York City housing market. Through the use of StreetEasy, a real estate listings website focused on New York City, the app provides various metrics in the housing sales and rental markets by borough, submarket and neighborhood. Therefore, the app provides two interactive dashboards: sales and rentals.

The sales dashboard includes:
* An interactive choropleth (“heatmap”) map, in which the user can select the inputs (property type (all properties, condo, co-op, and single-family), time, sales metric) to render the map.
* Historical trends, in which the user can select multiple boroughs, submarkets or neighborhoods for a selected property type and sales metric for comparison.
* 12-month median listing (“asking”) sales price forecast for a selected neighborhood or submarket, using Prophet, Meta's machine learning time series forecast algorithm.
* Mortgage calculation to estimate the monthly payment on a 30-year mortgage for a selected borough, submarket, or neighborhood, and property type, given the median listing (“asking”) price, current benchmark mortgage interest rate for New York and a 20% down payment.

NYC_Housing_Sales_demo.mov

The rental dashboard includes: 
* An interactive choropleth (“heatmap”) map, in which the user can select the inputs (rental type (all rentals, studio, 1 bedroom, 2 bedroom, 3+ bedroom, time, rental metric) to render the map.
* Historical trends, in which the user can select multiple boroughs, submarkets or neighborhoods for a selected rental type and rental metric for comparison.
* Affordability of neighborhoods bubble chart that uses the average household income for New York City and the median sales rent for all properties in a neighborhood (represented by the bubble size) as the components for affordability metric. Affordability is defined as the percentage of monthly household income spent on rent. 

Future areas of exploration include:
* Subway stations serving the neighborhoods, as well the time to travel to Midtown Manhattan taking the subway.
* Schools in the neighborhood
* Amenities, such as parks, libraries, museums, theaters, grocery stores, restaurants, hospitals and clinics, etc.
* Neighborhood profile page.

The link to the raw data for this app can be found [here](https://streeteasy.com/blog/data-dashboard/[object%20Object]?agg=Total&metric=Inventory&type=Sales&bedrooms=Any%20Bedrooms&property=Any%20Property%20Type&minDate=2010-01-01&maxDate=2023-10-01&area=Flatiron,Brooklyn%20Heights).

A special thanks to Siyuan Li (https://github.com/siyuanligit) who developed a similiar web application using StreetEasy data (https://siyuanli.shinyapps.io/REexplorer/), which was a immense help in providing guidance on cleansing the raw data.
