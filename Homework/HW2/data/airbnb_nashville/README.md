# Nashville AirBnB Listings


[Inside AirBnB](https://insideairbnb.com/get-the-data/) is an online
website that scrapes airbnb data and posts datasets online. It contains
information about the listing as well as reviews received. I downloaded
the current “snapshot” for Nashville, Tennessee

A full
[codebook](https://docs.google.com/spreadsheets/d/1iWCNJcSutYqpULSQHlNyGInUvHg2BoUGoNRIGa6Szc4/edit?gid=1322284596#gid=1322284596)
is posted online.

| variable | description |
|----|----|
| `name` | Name of listing |
| `description` | Descrption |
| `host_since` | When the host signed-up on the website |
| `host_response_time` | String description of how long it takes the host to respond |
| `host_response_rate` | The percent of the time that the host replies to a message (100 = 100%) |
| `host_is_superhost` | =1 if the host is a ‘superhost’ |
| `host_neighbourhood` | The neighborhood of the home (categorical) |
| `room_type` | What kind of room the listing is (e.g. entire home vs private room) |
| `price` | The cost per night of the listing |
| `bedrooms` | The number of bedrooms |
| `beds` | The number of beds |
| `bathrooms` | The number of bathrooms |
| `accommodates` | The number of people allowed to stay at the listing |
| `has_availability` | =1 if the listing has any availability |
| `availability_30` | The number of days available in the next month |
| `number_of_reviews` | The number of reviews the listing has received |
| `review_scores_rating` | Average review score for ‘rating’ |
| `review_scores_cleanliness` | Average review score for ‘cleanliness’ |
| `review_scores_accuracy` | Average review score for ‘accuracy’ |
| `review_scores_checkin` | Average review score for ‘checkin’ |
| `review_scores_location` | Average review score for ‘location’ |
| `review_scores_communication` | Average review score for ‘communication’ |
| `calculated_host_listings_count` | The (estimated) number of listings the host rungs |
