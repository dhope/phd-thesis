/* Model loops for danger/food variation
David Hope
July 2, 2017
*/

#include <stdio.h>
#include <iostream>
#include <stdlib.h>
#include <time.h>
#include <math.h>
#include <typeinfo>
#include <random>
#include <algorithm>
#include <mpi.h> // Boost to use mpi
#include "global.h"
#include "backfun.h"
#include "forfun.h"



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
// extern int siteCount[2][91];

// ########### Main program ##################
// extern int siteCount[2*91];

// void modifypredation(double pm){
// 	proportionD_safesite = pm;
// 	siteDanger[0] = flywayPredation; 
// 	siteDanger[1] = proportionD_safesite*flywayPredation;
// 	siteDanger[2] = proportionD_safesite*(1-dep_benefit)*flywayPredation;
// }

// void modifyfood(double fm){
// 	proportionF_safesite = fm;
// 	siteFuelIntake[0] = flywayFood; 
// 	siteFuelIntake[1] = proportionF_safesite*flywayFood;
// 	siteFuelIntake[2] = proportionF_safesite*flywayFood;
// }



int main(int argc, char *argv[])
{ // Setup
	using namespace std;
    int rank, numprocs;
    MPI_Init(&argc, &argv);
	MPI_Comm_rank(MPI_COMM_WORLD, &rank);
	MPI_Comm_size(MPI_COMM_WORLD, &numprocs);
	double fitness[(max_fuel_load + 1)*(final_time+1)*(number_of_sites + 1) ]; // Define the fitness array
	std::fill_n(fitness, (max_fuel_load + 1)*(final_time+1)*(number_of_sites + 1), -9);
	int decisions[(max_fuel_load + 1)*(final_time+1)*(number_of_sites + 1)] ; // Define the decision array
	std::fill_n(decisions, (max_fuel_load + 1)*(final_time+1)*(number_of_sites + 1), -9);
	outputExtra = false;
	// int siteCount[3*91]={0};
    forwardsOutput = false;
    // siteCount[number_of_sites][(final_time+1)] = {0};
    string txt = ".txt";
    string forFile; 
    // Begin loop
    float danger_i = 1.0;//0.0;
    float food_i = 1.0;
    // cout << "Got to start of loop" <<endl;
    // initialize the output file
    forFile = "/scratch/dhope/noU/MC_Loop_proc"+std::to_string(rank)+txt ;
				FILE *simfile;
			    if ((simfile = fopen(forFile.c_str(), "wt")) == NULL)
			    {
			      printf("%s%s\n", "Error opening", forFile.c_str());
			      exit(1);
			    }
			    fprintf(simfile, "Run\ttime\tdanger\tfood\tsite0\tsite1\tpk\n");
			    fclose(simfile);
	// ------------------------------
    while(danger_i <= 10.0) {
    	food_i = 1.0;
    	while (food_i<=10.){
    		// Modify the danger and food at each site
    		siteDanger[0] = danger_i;
			siteFuelIntake[0] = food_i;
    		// modifypredation(danger_i);
    		// modifyfood(food_i);
	    	// siteCount[number_of_sites][(final_time+1)] = {0};
	    	// siteCount.fill(0);// = {0};
			std::fill_n(decisions, (max_fuel_load + 1)*(final_time+1)*(number_of_sites + 1), -9);
	    	std::fill_n(fitness, (max_fuel_load + 1)*(final_time+1)*(number_of_sites + 1), -9);
	    	fullback(fitness,decisions, "NULL.txt",  false	); // Run the backwards programming equation
			for(int run=rank+1;run<=nruns; run+=numprocs){
				// cout<< food_dist(e2)<<"\t";
				int seed = std::chrono::high_resolution_clock::now().time_since_epoch().count();
				std::mt19937 e2(seed);
				for(int pk =0; pk<2; pk++){
					if(pk==0) first_move_free=false;
					else first_move_free = true;
					std::fill_n(siteCount, (number_of_sites+1)*(final_time+1), 0);
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
								if(s==0){fprintf(simfile, "%d\t%d\t%f\t%f\t%d", run, t, danger_i, food_i, siteCount[(s*(final_time+1)+t)]);}
			    				else fprintf(simfile, "\t%d", siteCount[(s*(final_time+1)+t)]);
					  		} fprintf(simfile, "\t%d\n", pk);
					    }
					    fclose(simfile);
		    // cout<<food_dist(e2)<< endl;
					}
			}

		// End loop




    		// cout << food_i << "\t" << danger_i << endl;
// 
		if(round(danger_i*100)==400) food_i =food_i + 0.1;
		else food_i =food_i +0.5;
    	}
    	if(round(food_i*100)==200) danger_i + 0.1;
    	else danger_i =danger_i+0.25;
    }
    	// cout << "Got to end of loop" <<endl;
    	

	    // cout << "This actually did work" <<endl;
    MPI_Finalize();
  	return 0;
}