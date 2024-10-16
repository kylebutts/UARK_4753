# IMDB Movies


The [internet Movie
Database](https://developer.imdb.com/documentation/bulk-data-documentation/data-dictionary/box-office/?ref_=side_nav)
collects user reviews of movies as well as some basic info of the film.

I have downloaded the data and subset to films made after 2010.

| variable          | description                                            |
|-------------------|--------------------------------------------------------|
| `title`           | Title of film                                          |
| `tconst`          | IMDB unique identifier                                 |
| `release_year`    | Year of film release                                   |
| `average_rating`  | The average user rating for the film                   |
| `num_votes`       | The number of user ratings for the film                |
| `runtime_minutes` | The runtime of the film                                |
| `title_type`      | String containing either ‘movie’ or ‘home movie’       |
| `is_tv_movie`     | =1 if the film is a TV-movie (from a tv show)          |
| `is_adult_movie`  | =1 if the movie is an film targeted to adult audiences |
| `genres`          | A string of up to 3 genres                             |
| `is_comedy`       | =1 if the film’s genre contains ‘comedy’               |
| `is_drama`        | =1 if the film’s genre contains ‘drama’                |
| `is_horror`       | =1 if the film’s genre contains ‘horror’               |
| `is_action`       | =1 if the film’s genre contains ‘action’               |
| `is_documentary`  | =1 if the film’s genre contains ‘documentary’          |
| `is_scifi`        | =1 if the film’s genre contains ‘scifi’                |
