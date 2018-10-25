/* Mixing prior knowledge in base model
David Hope
May 18, 2018
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
#include "forfun.h"



using namespace std;









int main()
{	
	string forFile;
	forwardsOutput = false;
	// seed = chrono::high_resolution_clock::now().time_since_epoch().count();
	// std::mt19937 mt_rand(seed);
	double fitness[(max_fuel_load + 1)*(final_time+1)*(number_of_sites + 1) ]; // Define the fitness array
	std::fill_n(fitness, (max_fuel_load + 1)*(final_time+1)*(number_of_sites + 1), -9);
	int decisions[(max_fuel_load + 1)*(final_time+1)*(number_of_sites + 1)] ; // Define the decision array
	std::fill_n(decisions, (max_fuel_load + 1)*(final_time+1)*(number_of_sites + 1), -9);
	// cout<< *(decisions + addr(15, 10, 0)) << endl;
	fullback(fitness,decisions, "NULL.txt", false	); // Run the backwards programming equation


	forFile = "./OrganizedResults/noU/proportion_pk/forwardsOutput.txt";
	// string forFile = "./output/MC/noU/MC_Counts_full.txt";//+std::to_string(run)+txt ;
	FILE *simfile;
    if ((simfile = fopen(forFile.c_str(), "wt")) == NULL)
    {
      printf("%s%s\n", "Error opening", forFile.c_str());
      exit(1);
    }
    if(number_of_sites==2)
    {
    	fprintf(simfile, "time\tsite0\tsite1\n");
    }
    if(number_of_sites==3)
    {
    	fprintf(simfile, "time\tsite0\tsite1\tsite2\n");
    }
    fclose(simfile);

    int seed = std::chrono::high_resolution_clock::now().time_since_epoch().count();
	std::mt19937 e2(seed);

	std::fill_n(siteCount, (3)*(final_time+1), 0);
	int total_birds[2] = {number_birds[0],number_birds[1]};
	number_birds[0] = int(prop_pk[0]* total_birds[0]);
	number_birds[1] = int(prop_pk[1]* total_birds[1]);
	first_move_free = true;
	fullForward(fitness,decisions, "NULL2.txt", false); // Simulate the forwards model
	number_birds[0] = int((1-prop_pk[0])* total_birds[0]);
	number_birds[1] = int((1-prop_pk[1])* total_birds[1]);
	first_move_free = false;
	fullForward(fitness,decisions, "NULL2.txt", false); // Simulate the forwards model
	if ((simfile = fopen(forFile.c_str(), "at")) == NULL)
		    {
		      printf("%s%s\n", "Error opening", forFile.c_str());
		      exit(1);
		    }
		    for (int t =0; t<= final_time; t++){
		    	for(int s = 0; s<number_of_sites; s++)
		    	{
		    		// cout<<siteCount[(s*(final_time+1)+t)]<<endl;
		    		if(s==0){fprintf(simfile, "%d\t%d",  t, siteCount[(s*(final_time+1)+t)]);}
		    		else fprintf(simfile, "\t%d", siteCount[(s*(final_time+1)+t)]);
		  		} fprintf(simfile, "\n");
		    }
		    fclose(simfile);
		    // cout<<food_dist(e2)<< endl;

		    return 0;

}

