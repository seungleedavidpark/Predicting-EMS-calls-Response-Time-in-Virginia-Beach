# Predicting-EMS-calls-in-Virginia-Beach
This project aims to create a model that predicts EMS calls to better allocate ambulances using the the Virginia Beach EMS call data. This is a final project of University Pennsylvania City Planning Program's Public Policy Analytics course (MUSA508) during the fall 2020 semester. 

# TASK LIST #
  -  DP: email Michael re office hour on Tuesday, 1030-430
  -  DP: clean up script (upload exp. analysis)
  -  DP: test for spatial process and correlations
  -  AG: model building
  -  AG: cross validation
  
  -  DP: Markdown document
  -  AG: recording of the video


# MEETING NOTES #


# SCHEDULE AND DELIVERABLES #

11/13 – Project introduced
11/20 – Prepare to discuss your chosen project, data, methods in lab breakout. Sharper wireframe exercise in lab.
12/4   -  Presentations in class
12/15 – Final markdown and video due

1.  Deliverable 1 (Due 11/20): Have a partner chosen, pick and project, and be prepared in lab to talk for a few minutes about the questions at the bottom of this document.

2. Deliverable 2 (Due 12/4): Team member 1 will be responsible for a 4 minute 'PechaKucha' presentation that ‘sells’ us on the idea of this fancy new planning app that you’ve designed to solve an important problem. Spend ~50% of your time on exploratory analysis and model results/validation. The expectation is that you will have preliminary model at this stage, which you will sharpen by the time the assignment is due. The other 50% should focus on questions like, What is the use case? Who is the user? How does the app put the model into the hands of a non-technical decision maker? Who is creating the app? Have you created something that is usable by the client? This is a presentation where the slides are set to change automatically, every 20 seconds. This is a requirement. Remember – sell it to us. What should come first - the model or the app? Don’t forget to constantly remind the audience about the use case to keep your solution relevant. 

3.  Deliverable 3 (12/15): 
Team member 1 will have the pechakucha uploaded on youtube with a recorded narration. Link to the video in your markdown.

Team member 2 will be responsible for a markdown write up that would allow someone to replicate your analysis (show your code blocks). Post this markdown on your Github not in a google folder. At minimum, please hit on the below components:
a. Motivate the analysis – “What is the use case; why would someone want to replicate your analysis and why would they use this approach?”
	b. Describe the data you used.
	c. Describe your exploratory analysis using maps and plots.
	d. What is the spatial or space/time process?
d. Describe your modeling approach and show how you arrived at your final model.
e. Validate your model with cross-validation and describe how your predictions are useful (accuracy vs. generalizability).
f. Provide additional maps and data visualizations to show that your model is useful.
g. Talk about how your analysis meets the use case you set out to address.
h. What could you do to make the analysis better?

I expect to see data visualizations that are of high quality. Please include codeblocks.



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
- drug overdose
- top causes of the calls
- Isochrones from hospitals (Mapbox API) 

What kind of model would you build and what is the dependent variable?
- Dependent variables will include distance/duration to a hospitals
- Age-related census data
- Traffic collisions
- Suicide rates

How will you validate this model (cross-validation & goodness of fit metrics that relate to the business process)?
- We will cross-validate the model on Santa Monica
 
How do you think that stakeholders would want to consume this data?
- Emergency Management Planners

What are the use cases for your app and what should the app do?
- This app will allow a user to hail an ambulance. In the interface, it will show how long an Ambulance will take, and display this information alongside time durations for other modes of transportation, such as travel by car, or public transportation.

