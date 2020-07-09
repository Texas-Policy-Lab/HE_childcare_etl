api <- function(port = "http://router.project-osrm.org",
                url = "{port}/table/v1/driving/{points1};{points2}",
                points1 = "29.7604,95.3698",
                points2 = "32.7767,96.7970") {
  browser()
  url <- glue::glue(url, port = port, points1 = points1)
  
  r <- httr::GET(url)
  
  
  browser()
  
  if (r$status_code == 200) {
    c <- httr::content(r)
    
    
  }
  # 
  #  r <- httr::GET("http://router.project-osrm.org/route/v1/driving/13.38886,52.51703;13.39763,52.52940;13.42855,52.52321?overview=false")
  # 
  #  c <- httr::content(r)
}

api()


# http://127.0.0.1:5000/route/

r <- httr::GET("http://router.project-osrm.org/table/v1/driving/13.3888,52.5170;13.3976,52.5294;13.42855,52.5232?sources=0")
r$status_code
c <- httr::content(r)

r <- httr::GET("http://router.project-osrm.org/table/v1/driving/29.54970,-95.13201;29.48268,-95.68019?sources=0", verbose = TRUE)
r$status_code

c <- httr::content(r)
