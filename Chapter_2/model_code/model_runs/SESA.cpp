/* Scenario Simulation
David Hope
February 22, 2018
*/

#include <stdio.h>
#include <iostream>
#include <stdlib.h>
#include <time.h>
#include <math.h>
#include <typeinfo>
#include <chrono>
#include <random>
#include "global.h"
#include "backfun.h"
// #include "Oiled.h"
//~ #include "global.cpp"
//~ #include "backfun.cpp"
#include "forfun.h"
//~ #include "forfun.cpp"
using namespace std;


int runmod(string backfilename, string directory)
{	
	auto filename = directory + backfilename; // File name for backwards output
	double fitness[(max_fuel_load + 1)*(final_time+1)*(number_of_sites + 1) ]; // Define the fitness array
	std::fill_n(fitness, (max_fuel_load + 1)*(final_time+1)*(number_of_sites + 1), -9);
	int decisions[(max_fuel_load + 1)*(final_time+1)*(number_of_sites + 1)] ; // Define the decision array
	std::fill_n(decisions, (max_fuel_load + 1)*(final_time+1)*(number_of_sites + 1), -9);
	cout<< filename.c_str() << endl;
	fullback(fitness,decisions, filename.c_str(), true	); // Run the backwards programming equation
	
 return 0;

}

// void modifypredation(double pm){
// 	flywayPredation = pm;
// 	siteDanger[0] = flywayPredation; 
// 	siteDanger[1] = proportionD_safesite*flywayPredation;
// 	siteDanger[2] = proportionD_safesite*(1-dep_benefit)*flywayPredation;
// }

// void modifyfood(double fm){
// 	flywayFood = fm;
// 	siteFuelIntake[0] = flywayFood; 
// 	siteFuelIntake[1] = proportionF_safesite*flywayFood;
// 	siteFuelIntake[2] = proportionF_safesite*flywayFood;
// }


int main()
{ 
	// SESA modifications
	// 1. Departure benefit
	// final_time = 200; // Terminal time for model
	distance_to_migrate = 5000; // Distance to migrate
	max_fuel_load = 200; // maximum potential fuel load
	max_true_fuel_load = 2.0;//1.0; // maximum fuel load in relative fuel units
	wetleanbodymass = 21.8;//22.7; //wet lean body mass
	// dep_benefit = 0.01;//0.23;
	// max_p_fuel_departure = .3;
	// shape_fuel_departure = 1.2;
	// flywayPredationX = (7E-2) /2;
	// pred_time_shape = 0.06623284;
	// flywayPredationDep = flywayPredationX*.2;
	// attackrate = 2;
	// fuel_modifier = .5;
	maxSeasonal = 1;
	arrival_dates[0] = 40;
	arrival_dates[1] = 70;
	// first_move_free = true;
	f_arrival_mod = -28;
	breeding_falc = 0;
	departureBreedingpop = 0.5;
	// siteDanger[1] = 1;
	// siteDanger[0] = 1/0.14;
	flywayFood = 1.5;
	siteFuelIntake[0]=1.3;
	settle_time = .5;
	 // flywayPredationDep =  flywayPredationX;
	// fuel_loading_rate = 2.0;
	// modifypredation(1);
	
	
	string basedir = "./OrganizedResults/noU/SESA/";
	// flywayPredation = 0.2;
	runmod("back_noresPred.txt", basedir);
	double baselinedepartureBreedingpop = departureBreedingpop;
	double baselinebreeding_falc = breeding_falc;
	// Resident population
	// maxSeasonal =1.5;
	flywayPredation = 1;
	breeding_falc = baselinebreeding_falc+ 1;
	departureBreedingpop = baselinedepartureBreedingpop+1;
	// dep_benefit = 0.1;
	runmod("back_resPred.txt", basedir);
	// flywayPredation =0.2;
	breeding_falc = baselinebreeding_falc + 0.1;
	departureBreedingpop =baselinedepartureBreedingpop+0.1 ;
	// dep_benefit = 0.001;
	runmod("back_lowresPred.txt", basedir);	
	
	return 0;
}