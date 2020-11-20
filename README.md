# Predicting-EMS-calls-in-Virginia-Beach
This project aims to create a model that predicts EMS calls to better allocate ambulances using the the Virginia Beach EMS call data. This is a final project of University Pennsylvania City Planning Program's Public Policy Analytics course (MUSA508) during the fall 2020 semester. 


# Initial Questionnaire #
What is the use case?
- The City of Virginia Beach has shared data on emergency management responses to 911 calls. This data could be used to help assess response time performance for Emergency Medical Services and develop anticipatory operations recommendations to insure that, in the event of a medical emergency, all residents are able to receive medical care as soon as possible. Some questions to consider:
    -> Are there more delays in response time when call volumes are highest?
    -> Are delays in response times predictable? Spatially and Temporally?
    -> Are call volumes predictable? Spatially and Temporally?

How could data make a difference in answering this question? Do you have a sense for the business as usual decision making?
- Currently, medical emergencies which occur far from a hospital are likely to experience a delay before an ambulance arrives.This data from Virginia beach could help reduce delays if we can assess the likelihood of a medical emergency occurring based on prior calls.

What datasets have you identified to help you answer this question?
- Hospitals
- Census data (17-18)
- Age
- CDC health outcomes data (17-18) (https://nccd.cdc.gov/500_Cities/rdPage.aspx?rdReport=DPH_500_Cities.InteractiveMap&islCategories=HLTHOUT&islMeasures=ARTHRITIS&islStates=51&rdRnd=59489)
- Traffic Collisions
- Suicide Rates (by tract?)
- Isochrones from hospitals (Mapbox API) 

What kind of model would you build and what is the dependent variable?
- Dependent variables will include distance/duration to a hospitals
- Age-related census data
- Traffic collisions
- Suicide rates

How will you validate this model (cross-validation & goodness of fit metrics that relate to the business process)?
-We will cross-validate the model on Santa Monica
 
How do you think that stakeholders would want to consume this data?
- Emergency Management Planners

What are the use cases for your app and what should the app do?
- This app will allow a user to hail an ambulance. In the interface, it will show how long an Ambulance will take, and display this information alongside time durations for other modes of transportation, such as travel by car, or public transportation.





# TASK LIST #

1) initial planning



# MEETING NOTES #
Friday, November 20
- project planning
