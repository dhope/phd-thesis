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

extern bool run_mc;

using namespace std;


int runmc(int dist)
{	
	string forFile;
	forFile = "./OrganizedResults/noU/distancemod/mc/distance_run_" +std::to_string(dist)+ ".txt" ;
				FILE *simfile;
			    if ((simfile = fopen(forFile.c_str(), "wt")) == NULL)
			    {
			      printf("%s%s\n", "Error opening this", forFile.c_str());
			      exit(1);
			    }
			    fprintf(simfile, "pk\trun\ttime\tsite0\tsite1\tdist\n");
			    fclose(simfile);


	bool fmf[2]={true, false};
	printf("Distance: %d\n",dist );
	
	forwardsOutput = false;
	// seed = chrono::high_resolution_clock::now().time_since_epoch().count();
	// std::mt19937 mt_rand(seed);
	string txt = ".txt";

	double fitness[(max_fuel_load + 1)*(final_time+1)*(number_of_sites + 1) ]; // Define the fitness array
	std::fill_n(fitness, (max_fuel_load + 1)*(final_time+1)*(number_of_sites + 1), -9);
	int decisions[(max_fuel_load + 1)*(final_time+1)*(number_of_sites + 1)] ; // Define the decision array
	std::fill_n(decisions, (max_fuel_load + 1)*(final_time+1)*(number_of_sites + 1), -9);
	// cout<< *(decisions + addr(15, 10, 0)) << endl;
	// printarr(fitness);
	fullback(fitness,decisions, "NULL.txt", false	); // Run the backwards programming equation
	for(int run=0;run<nruns; run++)
	{
		for (int pk = 0;pk<=1;pk++)
		{
			first_move_free = fmf[pk];
			int seed = std::chrono::high_resolution_clock::now().time_since_epoch().count();
			std::mt19937 e2(seed);
			std::fill_n(siteCount, (3)*(91), 0);
			fullForward(fitness,decisions, "NULL2.txt", false); // Simulate the forwards model
			if ((simfile = fopen(forFile.c_str(), "at")) == NULL)
			{
		      printf("%s%s\n", "Error opening", forFile.c_str());
		      exit(1);
			}
		    for (int t =0; t<= final_time; t++)
		    {
		    	if (outputAll || t == outputTimes[0] || t==outputTimes[1])
		    	{
			    	for(int s = 0; s<number_of_sites; s++)
			    	{ 
			    		if(s==0){fprintf(simfile, "%d\t%d\t%d\t%d",pk, run, t, siteCount[(s*(final_time+1)+t)]);}
			    		else fprintf(simfile, "\t%d", siteCount[(s*(final_time+1)+t)]);
			  		} fprintf(simfile, "\t%d\n", dist); 
			  	} else{continue;} 
		    } fclose(simfile);
	}
		
	}
 return 0;
}

// ########### Main program ##################


int main(int argc, char const *argv[])
{ 
	bool run_mc = false;
	// int total_birds[2] = {number_birds[0], number_birds[1]};
	for (int i =0;i<6;i++)
	{	distance_to_migrate = distances[i];

		if(run_mc)
		{
			runmc(distance_to_migrate);
			// cout<< "Monte carlo, place your bets" << endl;
		}

		else 
		{	

			auto filename = "./OrganizedResults/noU/distancemod/decisionmatrix/full_back_dist_" + std::to_string((int)distance_to_migrate) +".txt"; // File name for backwards output
			double fitness[(max_fuel_load + 1)*(final_time+1)*(number_of_sites + 1) ]; // Define the fitness array
			std::fill_n(fitness, (max_fuel_load + 1)*(final_time+1)*(number_of_sites + 1), -9);
			int decisions[(max_fuel_load + 1)*(final_time+1)*(number_of_sites + 1)] ; // Define the decision array
			std::fill_n(decisions, (max_fuel_load + 1)*(final_time+1)*(number_of_sites + 1), -9);
			// cout<< *(decisions + addr(15, 10, 0)) << endl;
			// printarr(fitness);
			// number_birds[0] = (int) (prop_dist_a[i]* total_birds[0]);
			// number_birds[1] = (int) (prop_dist_j[i]*total_birds[1]);
			fullback(fitness,decisions, filename.c_str(), true	); // Run the backwards programming equation
			// cout<< *(decisions + addr(15, 10, 0)) << endl;
			if ( argc ==2){
			if(strcmp( argv[1], "NoSim") == 0) cout << "No forward simulation" << endl;
			} else 
			{
				if(first_move_free)
				{
					auto filenamefor = "./OrganizedResults/noU/distancemod/pk/full_forward_dist_" + std::to_string((int)distance_to_migrate) +".txt"; // File name for backwards output
					fullForward(fitness,decisions,filenamefor.c_str()); // Simulate the forwards model
				} else
				{
				auto filenamefor = "./OrganizedResults/noU/distancemod/nopk/full_forward_dist_" + std::to_string((int)distance_to_migrate) +".txt"; // File name for backwards output
				fullForward(fitness,decisions,filenamefor.c_str()); // Simulate the forwards model}
				}

			
			// printarr(fitness);
			}
		}
	}
 	return 0;

}
