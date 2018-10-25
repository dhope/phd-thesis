/* Running scenarios
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
#include <algorithm>
#include <mpi.h> // Boost to use mpi
#include "global.h"
#include "backfun.h"
// #include "Oiled.h"
//~ #include "global.cpp"
//~ #include "backfun.cpp"
#include "forfun.h"
//~ #include "forfun.cpp"


using namespace std;

void modifyarrival(int deltaA, int deltaJ)
{

		int newA = arrival_dates[0] + deltaA;
		int newJ = arrival_dates[1] + deltaJ;
		date_a_dist = std::normal_distribution<float>(newA,sd_arrival[0]);
		date_j_dist = std::normal_distribution<float>(newJ,sd_arrival[1]);
}	


int montycarlo(string filename, string scenario, float id)
{	string forFile;
	// bool fmf[2]={false, true};
	forwardsOutput = false;
	// seed = chrono::high_resolution_clock::now().time_since_epoch().count();
	// std::mt19937 mt_rand(seed);
	string txt = ".txt";
	double fitness[(max_fuel_load + 1)*(final_time+1)*(number_of_sites + 1) ]; // Define the fitness array
	std::fill_n(fitness, (max_fuel_load + 1)*(final_time+1)*(number_of_sites + 1), -9);
	int decisions[(max_fuel_load + 1)*(final_time+1)*(number_of_sites + 1)] ; // Define the decision array
	std::fill_n(decisions, (max_fuel_load + 1)*(final_time+1)*(number_of_sites + 1), -9);
	
	if(!useRandomDraw){
	fullback(fitness,decisions, "NULL.txt", false	); // Run the backwards programming equation
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
		// std::normal_distribution<double> food_draw(data_food[0], sd_food_sites[0]);
		// std::normal_distribution<double> food_draw_M(data_food[1], sd_food_sites[1]);
		// std::normal_distribution<double> food_draw_L(data_food[2], sd_food_sites[2]);

		std::normal_distribution<double> danger_draw(data_danger[0], sd_danger_sites[0]);
		std::normal_distribution<double> danger_draw_M(data_danger[1], sd_danger_sites[1]);
		std::normal_distribution<double> danger_draw_L(data_danger[2], sd_danger_sites[2]);
	}
	forFile = "/scratch/dhope/noU/my_scenarios/"+ filename;
	//

	FILE *simfile;
	    if ((simfile = fopen(forFile.c_str(), "wt")) == NULL)
	    {
	      printf("%s%s\n", "Error opening", forFile.c_str());
	      exit(1);
	    }
	    fprintf(simfile, "run\ttime\tsite0\tsite1\tscenario\tid\n");
	    fclose(simfile);
	for(int run=0;run<nruns; run++){
		// cout<< food_dist(e2)<<"\t";
		int seed = std::chrono::high_resolution_clock::now().time_since_epoch().count();
		std::mt19937 e2(seed);
		
		if(useRandomDraw)
		{
			
			
			siteFuelIntake[1]= std::max(0.1,food_draw_L(e2));
			siteDanger[1] = std::max(0.1,danger_draw_L(e2));
			siteDanger[0] = std::max(siteDanger[1]+.0001,danger_draw(e2));
			siteFuelIntake[0]= std::max(siteFuelIntake[1]+0.0001,food_draw(e2));
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



		std::fill_n(siteCount, (number_of_sites+1)*(final_time+1), 0);
		int total_birds[2] = {number_birds[0],number_birds[1]};
		number_birds[0] = int(prop_pk[0]* total_birds[0]);
		number_birds[1] = int(prop_pk[1]* total_birds[1]);
		first_move_free = true;
		fullForward(fitness,decisions, "NULL2.txt", false); // Simulate the forwards model
		number_birds[0] = int((1-prop_pk[0])* total_birds[0]);
		number_birds[1] = int((1-prop_pk[1])* total_birds[1]);
		first_move_free = false;
		fullForward(fitness,decisions, "NULL2.txt", false); // Simulate the forwards model
		number_birds[0] = total_birds[0];
		number_birds[1] = total_birds[1];
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
			  		} fprintf(simfile, "\t%s\t%.3f\n", scenario.c_str(), id); 
		  	} else{continue;} 
		    }
		    fclose(simfile);
		    // cout<<food_dist(e2)<< endl;
	}
//}

	// fullForward(fitness,decisions); // Simulate the forwards model
	// printarr(fitness);
 return 0;

}

int runmc_dist(int dist, string filename, string scenario, float id)
{	distance_to_migrate = distances[dist];
	string forFile;
	//"./my_scenarios/"
	forFile = "/scratch/dhope/noU/my_scenarios/" + filename + "_dist_" + std::to_string(roundf(distance_to_migrate)) + ".txt";
	forwardsOutput = false;
	// seed = chrono::high_resolution_clock::now().time_since_epoch().count();
	// std::mt19937 mt_rand(seed);
	string txt = ".txt";
	double fitness[(max_fuel_load + 1)*(final_time+1)*(number_of_sites + 1) ]; // Define the fitness array
	std::fill_n(fitness, (max_fuel_load + 1)*(final_time+1)*(number_of_sites + 1), -9);
	int decisions[(max_fuel_load + 1)*(final_time+1)*(number_of_sites + 1)] ; // Define the decision array
	std::fill_n(decisions, (max_fuel_load + 1)*(final_time+1)*(number_of_sites + 1), -9);
	fullback(fitness,decisions, "NULL.txt", false	); // Run the backwards programming equation

	bool fmf[2]={true, false};
	printf("Distance: %f\n",distance_to_migrate );
	FILE *simfile;
    if ((simfile = fopen(forFile.c_str(), "wt")) == NULL)
    {
      printf("%s%s\n", "Error opening", forFile.c_str());
      exit(1);
    }
    fprintf(simfile, "run\ttime\tsite0\tsite1\tscenario\tid\tpk\tdist\n");
    fclose(simfile);

	for(int run=0;run<nruns; run++)
	{
		for (int pk = 0;pk<=1;pk++)
		{
			first_move_free = fmf[pk];
			int seed = std::chrono::high_resolution_clock::now().time_since_epoch().count();
			std::mt19937 e2(seed);
			std::fill_n(siteCount, (3)*(final_time+1), 0);
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
			  		if(s==0){fprintf(simfile, "%d\t%d\t%d", run, t, siteCount[(s*(final_time+1)+t)]);}
			    		else fprintf(simfile, "\t%d", siteCount[(s*(final_time+1)+t)]);
			  		} fprintf(simfile, "\t%s\t%.3f\t%d\t%d\n", scenario.c_str(), id, pk, (int)distance_to_migrate); 
			  	} else{continue;} 
		    } fclose(simfile);
	}
		
	}
 return 0;
}



int main(int argc, char *argv[]){
	using namespace std;
    int rank, numprocs;
    MPI_Init(&argc, &argv);
	MPI_Comm_rank(MPI_COMM_WORLD, &rank);
	MPI_Comm_size(MPI_COMM_WORLD, &numprocs);
	// int r = rank +1 ;
	string scenarios[6] = {
		"Mass",
		"Predator Timing",
		"Predator Numbers",
		"Food abundance",
		"WESA Numbers",
		"WESA Timing"};
	string scenario;
	// int rank_quotent = rank /3;
	// int rank_remainder = rank % 3;
	int scenarioRun = 1;
	for(int d_ist = 0; d_ist<6; d_ist++)
	// int d_ist = 3;
	// if(d_ist == 5000)
	{

		for(int s=0; s<6;s++)
		{
			scenario = scenarios[s].c_str();
			if(scenario == "Mass")
			{ // cout << (scenarioRun % rank)<< "kd\t"<<rank << endl;
				double baseline_fuel[2] = {fuel_arrival[0],fuel_arrival[1]};
				for(double i=0.1;i<=5; i+=0.1)
				{	
						if ((scenarioRun % numprocs) == rank)
						{
							// printf("%.2f\t%d\t%d\t%d\n",i,scenarioRun, rank, (scenarioRun % numprocs));
							fuel_arrival[0] = i;
							fuel_arrival[1] = i;
							// montycarlo("Mass"+std::to_string(roundf(i * 1000) / 1000)+".txt" , scenario, i);
							runmc_dist(d_ist,"Mass"+std::to_string(roundf(i * 1000) / 1000) , scenario, i);
							//"_"+std::to_string(rank)+
						} scenarioRun++;
						
				}
				fuel_arrival[0] = baseline_fuel[0];
				fuel_arrival[1] = baseline_fuel[1];
			}

			if(scenario=="Predator Timing")
			{
				for(int i=-30;i<=30; i++)
				{	
						if ((scenarioRun % numprocs) == rank)
						{
							f_arrival_mod = i;
							// montycarlo("PredTime"+std::to_string(i)+".txt" , scenario, i);
							runmc_dist(d_ist,"PredTime"+std::to_string(i) , scenario, i);
						} scenarioRun++;
						
				}
				f_arrival_mod = 0;
			}

			if(scenario=="Predator Numbers")
			{double baselinepred =flywayPredation;
				for(double i=0.1;i<=10; i+=0.1)
				{	
						if ((scenarioRun % numprocs) == rank)
						{
							flywayPredation = baselinepred*i;
							// montycarlo("PredPop"+std::to_string(roundf(i * 100) / 100)+".txt" , scenario, i);
							runmc_dist(d_ist, "PredPop"+std::to_string(roundf(i * 100) / 100) , scenario, i);
						} scenarioRun++;
						
				}
				flywayPredation = baselinepred;
			}

			if(scenario=="Food abundance")
			{double baselinefood = flywayFood;
				for(double i=0.1;i<=5; i+=0.1)
				{	

					if ((scenarioRun % numprocs) == rank)
							{
								flywayFood=baselinefood*i;
								// montycarlo("Food"+std::to_string(roundf(i * 100) / 100)+".txt" , scenario, i);
								runmc_dist(d_ist,"Food"+std::to_string(roundf(i * 100) / 100) , scenario, i);
							} scenarioRun++;

				} flywayFood = baselinefood;
			}
			if(scenario=="WESA Numbers")
			{int baseNumbers = number_birds[0];
				for(double i=0.1;i<=3; i+=0.05)
				{	
					if ((scenarioRun % numprocs) == rank)
						{
							number_birds[0] = baseNumbers*i;
							number_birds[1] = baseNumbers*i;
							// montycarlo("WESApop"+std::to_string(roundf(i * 100) / 100)+".txt" , scenario, i);
							runmc_dist(d_ist,"WESApop"+std::to_string(roundf(i * 100) / 100) , scenario, i);
						} scenarioRun++;
				}
				number_birds[0] = baseNumbers;
				number_birds[1] = baseNumbers;
			} 

			if(scenario=="WESA Timing")
			{
				for(int i=-20;i<=20; i++)
				{	
					if ((scenarioRun % numprocs) == rank)
							{
								modifyarrival(i, i);
								// montycarlo("WESAArrival"+std::to_string(roundf(i * 100) / 100)+".txt" , scenario, i);
								runmc_dist(d_ist,"WESAArrival"+std::to_string(roundf(i * 100) / 100) , scenario, i);
							} scenarioRun++;
				}
				modifyarrival(0,0);
			}



		}
	}


	MPI_Finalize();
	return 0;
	
}