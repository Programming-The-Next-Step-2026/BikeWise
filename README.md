# BikeWise
A decision-support tool that helps students optimize study time and cycling commute windows based on real-time weather forecasts.

BikeWise uses weather data along a user’s cycling route (e.g., from Uni to Home) to determine whether it is better to leave immediately and “pedal now” to avoid rain, or stay longer and “lock in” for another productive study session.

Users can save frequently used locations such as Home, Uni, Work, Family, and Friends for future use. Additionally, users can define their personal minimum “lock-in” duration (i.e., the minimum amount of uninterrupted time needed for a productive study session).

After selecting a current location and destination, BikeWise provides one of several recommendations:

->  Pedal now! - if the route is currently dry and can be completed before rainfall begins.
  
->  Lock in! - if it is currently raining, but the estimated time until the rain stops exceeds the user’s minimum productive study duration.
  
->  Take a break! - if it is raining, but not long enough to justify starting another focused study session.
  
->  Better take the metro! - if rain persists until late evening.

The application also provides an estimated safe departure time indicating when users can likely leave without encountering rain during their ride.


# Additional Tools
Open-Meteo API (https://open-meteo.com) for weather forecasts

OSRM API (https://project-osrm.org) for cycling routes
