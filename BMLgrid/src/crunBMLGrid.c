#include <stdio.h> 
/*
There are two functions in this code, the first one is "move", which returns the grid that move in one step, and the 
corresponding velocity (parameter "v"), the number of moved cars (parameter "carmove"); the second function is "crunBMLGrid", 
which returns the grid that move several steps which can be decided by the users,and the velocity in every step, and 
corresponding number of cars moved in every step.
*/

/*
move function: grid move in one step, store the the grid which move one step, and the corresponding velocity, the number of cars
that move

parameter:
grid - the initial grid before the step
nrow - the number of rows of the grid; ncol - the number of column in the grid
nblue - the number of blue cars in the grid; nred - the number of red cars in the grid
step - the time step, i.e which time step to run. For example, the seond step, means when the time =2
v - the velocity
carmove - the number of car moves in this step
*/


void move(int *grid, int *nrow, int *ncol, int *nblue, int *nred, int step, double *v, int *carmove)
{
/*
totcar - the number of cars in the grid
length - the length of the grid. Because the grid is a array, I should initalize the length of it.
j - the red cars index; k - the blue cars index; t - for the cars index;
movecar - store the number of cars that moves, the initial value is 0. I do not use the carmove to directly calculate the number of moved cars,
          because it easily to be wrong

Arrays:
cur_long - the length of it is the same as the grid, but when there is a car, it turns to be 1; if not, turns to be 0
cur_have - instore the position which have cars
next_blue - the next positions for the blue cars after the "step"th time; 
next_red - the next positions for the red cars after the "step"th time; 
*/

  int numrow = *nrow, numcol = *ncol, totcar = (*nblue) + (*nred), length = (*nrow) * (*ncol), i, j = 0, k = 0, t = 0, movecar = 0;
  int cur_long[length], cur_have[totcar], next_blue[*nblue], next_red[*nred];

/* get the position of the none car and the position which have a car*/
 for ( i = 0; i < length; i++)
 {
  if (grid[i] != 0)  {
      cur_long[i] = 1; /*if it is not "", the coresponding position means there is a car, give the value 1; otherwise, give the value 0*/
      cur_have[t] = i; /*store the position in the grid*/
      t++;
    }else cur_long[i] = 0;
 }


  /* first, consider for the blue car */
    if ( (step%2) == 1) /* in odd step*/
    { for ( i = 0; i < totcar; i++) /* every cars' position is in the totcar, so make a for loop in it can decrease the times of loop */
      { 
      if ( grid[cur_have[i]] == 1) /* if the position have blue cars */
         {

         /* if the blue car in the fist row, it will wraps, if not, the position will -1 */
         if( (cur_have[i] % numrow) == 0) next_blue[k] = cur_have[i] + numrow - 1; 
         else next_blue[k] = cur_have[i] - 1;      

        /* means there is a car in the current position, so the car can not move */
          if ( cur_long[next_blue[k]] == 1 ) next_blue[k] = cur_have[i];
          else movecar++;
       
       /* change the original position to be no car, and the next position to be blue can*/
       grid[cur_have[i]] = 0;
       grid[next_blue[k]] = 1;
    
      /* calculate the velocity of the car, "*0.1" means it is double */
       v[step-1] = movecar*1.0/(*nblue);
 
        k++;
      }

     }

   }

  /* consider for the red car */
  if ( (step%2) == 0 ) /* for the even step */
    { for ( i = 0; i < totcar; i++) 
      {  
      if ( grid[cur_have[i]] == 2) /*find the current position which is red car*/
       {
           if ( (cur_have[i] / numrow) == numcol - 1 ) next_red[j] = (cur_have[i]) % numrow;
           else next_red[j] = cur_have[i] + numrow;

             /* means there is a car in the current position, so the car can not move */
           if ( cur_long[next_red[j]] == 1 )  next_red[j] = cur_have[i];
           else movecar++;
 
         /* same as the blue car case, change the original position to be no car, and the next position to be blue can*/
         grid[cur_have[i]] = 0;
         grid[next_red[j]] = 2;

         v[step-1] = movecar*1.0/(*nred);

         j++;
      }
    }
  }

  carmove[step-1] = movecar;
  //v[step-1] = movecar*1.0/totcar; 

}

/*
crunBMLGrid function used to run the grid for several time steps, and calculate the velocity in
every step, the number of moved cars in every step

Parameter:
grid, nrow, ncol, nblue, nred - have the same meanings as the move function
numSteps - how many step should be run
vstep - the initial velocity for every step. The initial value is 0 for the length of step
carmovestep - the moved cars for every step. The initial value is 0 for the length of step
*/

void crunBMLGrid(int *grid, int *nrow, int *ncol, int *nblue, int *nred, int *numSteps, double *vstep, int *carmovestep)
{ int i, step = *numSteps;

  for( i = 1; i <= step; i++ ) 
  move(grid, nrow, ncol, nblue, nred, i, vstep, carmovestep);
}
























