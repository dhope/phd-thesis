/* Running scenarios
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
{	forwardsOutput=false;
	outputExtra=false;
	auto filename = directory + backfilename; // File name for backwards output
	double fitness[(max_fuel_load + 1)*(final_time+1)*(number_of_sites + 1) ]; // Define the fitness array
	std::fill_n(fitness, (max_fuel_load + 1)*(final_time+1)*(number_of_sites + 1), -9);
	int decisions[(max_fuel_load + 1)*(final_time+1)*(number_of_sites + 1)] ; // Define the decision array
	std::fill_n(decisions, (max_fuel_load + 1)*(final_time+1)*(number_of_sites + 1), -9);
	// cout<< *(decisions + addr(15, 10, 0)) << endl;
	// printarr(fitness);
	cout<< filename.c_str() << endl;
	fullback(fitness,decisions, filename.c_str(), false	); // Run the backwards programming equation
	// cout<< *(decisions + addr(15, 10, 0)) << endl;
	
	string forFile = directory + "SenRes_pk.txt";

	FILE *simfile;
    if ((simfile = fopen(forFile.c_str(), "wt")) == NULL)
    {
      printf("%s%s\n", "Error opening", forFile.c_str());
      exit(1);
    }
    fprintf(simfile, "run\ttime\tsite0\tsite1\n");
    fclose(simfile);

	for(int run=0;run<nruns; run++){
		// cout<< food_dist(e2)<<"\t";
		int seed = std::chrono::high_resolution_clock::now().time_since_epoch().count();
		std::mt19937 e2(seed);
		std::fill_n(siteCount, (3)*(91), 0);
		fullForward(fitness,decisions, "NULL2.txt", false); // Simulate the forwards model
		if ((simfile = fopen(forFile.c_str(), "at")) == NULL)
		    {
		      printf("%s%s\n", "Error opening", forFile.c_str());
		      exit(1);
		    }
		    for (int t =0; t<= final_time; t++){
		    	if (outputAll || t == outputTimes[0] || t==outputTimes[1]) {
			    	for(int s = 0; s<number_of_sites; s++)
			    	{ 
			    		// cout<<siteCount[(s*(final_time+1)+t)]<<endl;
			    		if(s==0){fprintf(simfile, "%d\t%d\t%d", run, t, siteCount[(s*(final_time+1)+t)]);}
			    		else fprintf(simfile, "\t%d", siteCount[(s*(final_time+1)+t)]);
			  		} fprintf(simfile, "\n"); 
		  	} else{continue;} 
		    }
		    fclose(simfile);
		    // cout<<food_dist(e2)<< endl;
	}

// printarr(fitness);
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
	// Original modifications
	// 1. Departure benefit
	// dep_benefit = 0.958;
	first_move_free = false;
	// modifypredation(1);
	// pred_time_shape = pred_time_shape * 0.5;
	// fuel_arrival[1] = 1.0;
	// proportionF_safesite = 0.3;
	// modifyfood(1);
	// cost_move = 0.21;
	
	string basedir = "./OrganizedResults/noU/match_mc/no_pk/";
	runmod("full_back_single.txt", basedir);
	first_move_free = true;
	basedir = "./OrganizedResults/noU/match_mc/pk/";
	runmod("full_back_single.txt", basedir);
	
	return 0;
}