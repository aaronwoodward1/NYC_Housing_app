# Streeteasy Sales and Rental Dashboard

This Shiny app provides buyers, sellers and investors insights into the New York City housing market. Through the use of StreetEasy, a real estate listings website focused on New York City, the app provides various metrics in the housing sales and rental markets by borough, submarket and neighborhood. Therefore, the app provides two interactive dashboards: sales and rentals.

The sales dashboard includes:
* An interactive choropleth (“heatmap”) map, in which the user can select the inputs (property type (all properties, condo, co-op, and single-family), time, sales metric) to render the map.
* Historical trends, in which the user can select multiple boroughs, submarkets or neighborhoods for a selected property type and sales metric for comparison.
* 12-month median listing (“asking”) sales price forecast for a selected neighborhood or submarket, using Prophet, Meta's machine learning time series forecast algorithm.
* Mortgage calculation to estimate the monthly payment on a 30-year mortgage for a selected borough, submarket, or neighborhood, and property type, given the median listing (“asking”) price, current benchmark mortgage interest rate for New York and a 20% down payment.

![sales_demo1](https://media.giphy.com/media/v1.Y2lkPTc5MGI3NjExYm5hYjlnbGZmMWVva3J3aXp5aWlsdnpobnRrMmY5cTA2eXg3Y284NSZlcD12MV9pbnRlcm5hbF9naWZfYnlfaWQmY3Q9Zw/inDsdZOQVFxsX7PAVs/giphy.gif) ![sales_demo2](https://media.giphy.com/media/v1.Y2lkPTc5MGI3NjExNHF5enpobWRtcG1ha3I1cWN3OGo5aHZyZGM0dnZraXgwcmJjdGdiMCZlcD12MV9pbnRlcm5hbF9naWZfYnlfaWQmY3Q9Zw/vpWW63XbxZ2T7lO0Ir/giphy.gif)
![sales_demo3](https://media.giphy.com/media/v1.Y2lkPTc5MGI3NjExYzR1cXEyMnh6Z2xneGE1a2l2d21xb20xMm9iYTFveWZ6OTUzdWxvbSZlcD12MV9pbnRlcm5hbF9naWZfYnlfaWQmY3Q9Zw/ywubiJ907EGHP3sF8t/giphy.gif) ![sales_demo4](https://media.giphy.com/media/v1.Y2lkPTc5MGI3NjExc2RpMWZ1M3Q0b3lxcHVwdDQwN3Vwc20yczZmY2QxNDA1NGd6ZDRmMCZlcD12MV9pbnRlcm5hbF9naWZfYnlfaWQmY3Q9Zw/58Z6oa4K0TKe2etvZq/giphy.gif)

The rental dashboard includes: 
* An interactive choropleth (“heatmap”) map, in which the user can select the inputs (rental type (all rentals, studio, 1 bedroom, 2 bedroom, 3+ bedroom, time, rental metric) to render the map.
* Historical trends, in which the user can select multiple boroughs, submarkets or neighborhoods for a selected rental type and rental metric for comparison.
* Affordability of neighborhoods bubble chart that uses the average household income for New York City and the median sales rent for all properties in a neighborhood (represented by the bubble size) as the components for affordability metric. Affordability is defined as the percentage of monthly household income spent on rent. 

![rental_demo1](https://media.giphy.com/media/v1.Y2lkPTc5MGI3NjExNjBtNzE3ZDdhOWg5c2J3NTdrZHo1NXc5b2k4MWdsNG56dWcybWQ3eiZlcD12MV9pbnRlcm5hbF9naWZfYnlfaWQmY3Q9Zw/8fqQX6vHZZCJ37SjT0/giphy.gif) ![rental_demo2](https://media.giphy.com/media/v1.Y2lkPTc5MGI3NjExdDZuZ2tvMzY5YjNucHV6M2ZtYTU5dmJwN2VxZTNvdXV5cGRrd3RjeSZlcD12MV9pbnRlcm5hbF9naWZfYnlfaWQmY3Q9Zw/IUnMcTc1IpQ64thPhy/giphy.gif) ![rental_demo3](https://media.giphy.com/media/v1.Y2lkPTc5MGI3NjExZWNpYW5xc281YTcxbGdsaDRxdmtqYTYzZTFrNDhnNTdsdzhvd2sxNCZlcD12MV9pbnRlcm5hbF9naWZfYnlfaWQmY3Q9Zw/9TNpbd9Q9IrXP5GC5H/giphy.gif)

Future areas of exploration include:
* Subway stations serving the neighborhoods, as well the time to travel to Midtown Manhattan taking the subway.
* Schools in the neighborhood
* Amenities, such as parks, libraries, museums, theaters, grocery stores, restaurants, hospitals and clinics, etc.
* Neighborhood profile page.

Current debugging issues:
* Fixing confidence interval (gray-shaded area) feauture for the listing price forecasts.

The link to the raw data for this app can be found [here](https://streeteasy.com/blog/data-dashboard/[object%20Object]?agg=Total&metric=Inventory&type=Sales&bedrooms=Any%20Bedrooms&property=Any%20Property%20Type&minDate=2010-01-01&maxDate=2023-10-01&area=Flatiron,Brooklyn%20Heights).

A special thanks to Siyuan Li (https://github.com/siyuanligit) who developed a similiar web application using StreetEasy data (https://siyuanli.shinyapps.io/REexplorer/), which was a immense help in providing guidance on cleansing the raw data.
