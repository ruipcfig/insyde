# Exposure variables

# Geometry
FA <- 100       # Footprint area (m2)
IA <- 0.9 * FA  # Internal area (m2)
BA <- 0.5 * FA  # Basement area (m2)
EP <- 40        # External Perimeter (m)
IH <- 3.5       # Interstorey height (m)
BH <- 3.2       # Basement height (m)
GL <- 0.1       # Ground floor level (m)
NF <- 2         # Number of floors

# Others
BT <- 1     # Building type: 1- Detached, 2- Semi-detached, 3- Apartment house 
BS <- 2     # Building structure: 1- Reinforced concrete, 2- Masonry, 3- Wood
PD <- 1     # Plant distribution: 1- Centralized, 2- Distributed
PT <- 1	    # Heating system type: 1- Radiator, 2- Underfloor heating
FL <- 1.2   # Finishing level coefficient: High 1.2, Medium 1, Low 0.8
YY <- 1994  # Year of construction
LM <- 1.1   # Level of maintanance coefficient: High 1.1, Medium 1, Low 0.9
