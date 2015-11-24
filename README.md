# BML
The simulation is about cars moving on a grid. We have two types of cars, “blue” and “red”. These move on a twodimensional
grid of size r by c. We populate the grid by placing ρ × r × c (with 0 < ρ < 1) cars at random positions
in the r × c cells, but with no two cars occupying the same cell. The type of each car is selected randomly from “red”
and “blue” with equal probability. ( You can explore different probabilities, or forcing the numbers to be the same.)
Now the cars can move. In our configuration, the blue cars move at time periods t = 1, 3, 5, ... and the red cars
move at time periods t = 2, 4, 6, ..., i.e., they alternate in time. The “blue” cars move vertically upward. The “red”
cars move horizontally rightwards. When a car gets to the edge of the grid, it “wraps” around, i.e., when a blue car
gets to the top row, the next time it moves it goes to the bottom row of the same column. Similarly a red car that gets
to the right edge of the grid will move next to the first column of the grid, i.e., the extreme left.
A car cannot move to cell if that cells is already occupied by another car (of any color).

This process is called the Biham-Middleton-Levine Traffic Model and is of interest because it is one of the very
simplest processes that exhibits a phase-transition, and also self-organizing behavior. As ρ changes from .2 to .5, there is a point at which the behavior dramatically shifts. And recent research has identified sub-structures. Additionally,
while each car has a simple rule to move that only depends on whether another car is in its target location, the cars
form a collective group and establish a clear pattern of self-organization motion.