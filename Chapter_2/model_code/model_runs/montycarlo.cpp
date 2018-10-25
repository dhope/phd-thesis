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
#include <chrono>
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


int main()
{	
	string forFile;
	bool fmf[2]={true, false};
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
	if(!useRandomDraw){
	fullback(fitness,decisions, "NULL.txt", false	); // Run the backwards programming equation
	// cout<< *(decisions + addr(15, 10, 0)) << endl;
	}
	std::normal_distribution<double> danger_draw_M;
	std::normal_distribution<double> food_draw_M;
	// std::normal_distribution<double> danger_draw_L;
	// std::normal_distribution<double> food_draw_L;
	// std::normal_distribution<double> danger_draw;
	// std::normal_distribution<double> food_draw;
	// if(number_of_sites==2)
	// {
		std::normal_distribution<double> food_draw(data_food[0], sd_food_sites[0]);
		std::normal_distribution<double> food_draw_L(data_food[1], sd_food_sites[1]);

		std::normal_distribution<double> danger_draw(data_danger[0], sd_danger_sites[0]);
		std::normal_distribution<double> danger_draw_L(data_danger[1], sd_danger_sites[1]);

	// }

	if(number_of_sites==3)
	{
		std::normal_distribution<double> food_draw(data_food[0], sd_food_sites[0]);
		std::normal_distribution<double> food_draw_M(data_food[1], sd_food_sites[1]);
		std::normal_distribution<double> food_draw_L(data_food[2], sd_food_sites[2]);

		std::normal_distribution<double> danger_draw(data_danger[0], sd_danger_sites[0]);
		std::normal_distribution<double> danger_draw_M(data_danger[1], sd_danger_sites[1]);
		std::normal_distribution<double> danger_draw_L(data_danger[2], sd_danger_sites[2]);
	}

	for (int pk = 0;pk<=1;pk++){

			first_move_free = fmf[pk];
			if(first_move_free == true) forFile = "./OrganizedResults/noU/MonteCarloRuns/pk/MC_Counts_full.txt";
			else forFile = "./OrganizedResults/noU/MonteCarloRuns/no_pk/MC_Counts_full.txt";

			// string forFile = "./output/MC/noU/MC_Counts_full.txt";//+std::to_string(run)+txt ;
			FILE *simfile;
		    if ((simfile = fopen(forFile.c_str(), "wt")) == NULL)
		    {
		      printf("%s%s\n", "Error opening", forFile.c_str());
		      exit(1);
		    }
		    if(number_of_sites==2)
		    {
		    	fprintf(simfile, "run\ttime\tsite0\tsite1\n");
		    }
		    if(number_of_sites==3)
		    {
		    	fprintf(simfile, "run\ttime\tsite0\tsite1\tsite2\n");
		    }
		    fclose(simfile);

		

	for(int run=0;run<nruns; run++){
		// cout<< food_dist(e2)<<"\t";
		int seed = std::chrono::high_resolution_clock::now().time_since_epoch().count();
		std::mt19937 e2(seed);
		
		if(useRandomDraw)
		{
			siteFuelIntake[0]= std::max(0.1,food_draw(e2));
			siteDanger[0] = std::max(0.1,danger_draw(e2));
			siteFuelIntake[1]= std::max(0.1,food_draw_L(e2));
			siteDanger[1] = std::max(0.1,danger_draw_L(e2));
			// cout<< siteDanger[0]<< "\t" << siteDanger[1] << "\t" <<siteFuelIntake[0] << "\t" << siteFuelIntake[1] << endl;

			if(number_of_sites==3)
			{
				siteFuelIntake[1]= std::max(0.00001,food_draw_M(e2));
				siteDanger[1] = std::max(0.00001,danger_draw_M(e2));
			}
			std::fill_n(fitness, (max_fuel_load + 1)*(final_time+1)*(number_of_sites + 1), -9);
			std::fill_n(decisions, (max_fuel_load + 1)*(final_time+1)*(number_of_sites + 1), -9);
			fullback(fitness,decisions, "NULL.txt", false	); 
	
		}		

		std::fill_n(siteCount, (3)*(91), 0);
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
		    		if(s==0){fprintf(simfile, "%d\t%d\t%d", run, t, siteCount[(s*(final_time+1)+t)]);}
		    		else fprintf(simfile, "\t%d", siteCount[(s*(final_time+1)+t)]);
		  		} fprintf(simfile, "\n");
		    }
		    fclose(simfile);
		    // cout<<food_dist(e2)<< endl;
	}
}
	// fullForward(fitness,decisions); // Simulate the forwards model
	// printarr(fitness);
 return 0;

}
