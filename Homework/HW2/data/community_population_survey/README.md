# Community Population Survey


The Community Population Survey is a survey administered by the US
Goverment and collections information about workers. We use the [CPS
1985 dataset](https://rdrr.io/cran/AER/man/CPS1985.html).

experience age occupation sector union married female hispanic

| variable | description |
|----|----|
| `wage` | Wage (in dollars per hour). |
| `education` | Number of years of education. |
| `has_hs_degree` | =1 if the worker has a HS degree |
| `has_college_exp` | =1 if the worker has more than 12 years of schooling |
| `experience` | Number of years of potential work experience (age - education - 6). |
| `age` | Age in years. |
| `hispanic` | =1 if the worker is of Hispanic ethnicity. |
| `female` | =1 if the worker is a female. |
| `occupation` | Occupation of the worker: ‘worker’ (tradesperson or assembly line worker), ‘technical’ (technical or professional worker), ‘services’ (service worker), ‘office’ (office and clerical worker), ‘sales’ (sales worker), ‘management’ (management and administration). |
| `sector` | Sector of the worker: ‘manufacturing’ (manufacturing or mining), ‘construction’, ‘other’. |
| `union` | =1 if the individual works in a union job |
| `married` | =1 if the worker is married |
