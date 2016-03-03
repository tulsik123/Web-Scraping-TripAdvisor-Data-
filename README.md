###Overview

This script scraps search results from TripAdvisor and extracts some relevent data using rvest package created by [Hadley Wickham] (https://github.com/hadley). Here, the search results are for restaurants in Cologne-Germany *(which could be replaced by another city)*.

The script:
- takes the URL of the first page and loop through the pages to extract the names and URLs of all the restaurants.

- loops through the restaurants' URLs, parse each HTML page and extract some data if available:

 * Number of reviews
 
 * Number of photos
 
 * Stars/Rating
 
 * Cuisine
 
 * Nearby restaurants

