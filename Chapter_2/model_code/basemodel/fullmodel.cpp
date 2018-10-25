/* Conversion of full model from python
David Hope
March 13, 2017
*/

#include <stdio.h>
#include <iostream>
#include <stdlib.h>
#include <time.h>
#include <math.h>
#include <typeinfo>
#include <random>
#include "global.h"
#include "backfun.h"
//~ #include "global.cpp"
//~ #include "backfun.cpp"
#include "forfun.h"
//~ #include "forfun.cpp"


using namespace std;


// Initialize all the functions
// long addr(int fuel, int time, int site);
// double ConvertToRealFuel(int fuel_in);
// double ProbabilitySuriveOnwardsMigration(int fuel);
// void TerminalFitness(double* fitness);
// double mu(int fuel, int time,int site);
// double CalculateValues(int fuel, int time, int site,int newSite, double* fitness);
// double interpolate_fitness(double next_fuel_load, int site, int time, double* fitness);
// void CalculateFitness( double* fitness,int* decisions,int time);
// void fullback(char* backfile_, bool* plotout);


// ########### Main program ##################

// ******************************************
void printarr(double *fitness) {
  int x,t,s;
  for(s=0;s<=number_of_sites; s++){
	  for ( x=0; x<=max_fuel_load; x++ ) {
	    for ( t=0; t<=final_time; t++ ) {
	    	
	      printf("%4.3f ", *(fitness + addr(x,t,s)));
	    }
	    printf("\n");
	  }
	  printf("------------------- Site change ----------\n");
	}
}


int main(int argc, char const *argv[])
{
	auto filename = "full_back_single.txt"; // File name for backwards output
	double fitness[(max_fuel_load + 1)*(final_time+1)*(number_of_sites + 1) ]; // Define the fitness array
	std::fill_n(fitness, (max_fuel_load + 1)*(final_time+1)*(number_of_sites + 1), -9);
	int decisions[(max_fuel_load + 1)*(final_time+1)*(number_of_sites + 1)] ; // Define the decision array
	std::fill_n(decisions, (max_fuel_load + 1)*(final_time+1)*(number_of_sites + 1), -9);
	// cout<< *(decisions + addr(15, 10, 0)) << endl;
	// printarr(fitness);
	fullback(fitness,decisions, filename, true	); // Run the backwards programming equation
	// cout<< *(decisions + addr(15, 10, 0)) << endl;
	if ( argc ==2){
	if(strcmp( argv[1], "NoSim") == 0) cout << "No forward simulation" << endl;
	} else fullForward(fitness,decisions); // Simulate the forwards model
	// printarr(fitness);
 return 0;

}
