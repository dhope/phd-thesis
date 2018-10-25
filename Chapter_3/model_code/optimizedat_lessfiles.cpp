/* Attempting to find best fit to data in cpp
David Hope
July 31, 2018
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
#include "forfun.h"
#include "csv.h"

using namespace std;

bool SameSign(double x, double y)
{
    return (x >= 0) ^ (y < 0);
}

bool SameSign_i(int x, int y)
{
    return (x >= 0) ^ (y < 0);
}

void modifyarrival(int deltaA, int deltaJ)
{

		int newA = arrival_dates[0] + deltaA;
		int newJ = arrival_dates[1] + deltaJ;
		date_a_dist = std::normal_distribution<float>(newA,sd_arrival[0]);
		date_j_dist = std::normal_distribution<float>(newJ,sd_arrival[1]);
}

int runmodel()
{	
	n_skippers[0] = 0;
	n_skippers[1] = 0;
	forwardsOutput = false;
	double fitness[(max_fuel_load + 1)*(final_time+1)*(number_of_sites + 1) ]; // Define the fitness array
	std::fill_n(fitness, (max_fuel_load + 1)*(final_time+1)*(number_of_sites + 1), -9);
	int decisions[(max_fuel_load + 1)*(final_time+1)*(number_of_sites + 1)] ; // Define the decision array
	std::fill_n(decisions, (max_fuel_load + 1)*(final_time+1)*(number_of_sites + 1), -9);
	fullback(fitness,decisions, "NULL.txt", false	); // Run the backwards programming equation
	int seed = std::chrono::high_resolution_clock::now().time_since_epoch().count();
	std::mt19937 e2(seed);
		
		


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


 return 0;

}


double modify_variable(string variable_, double modifier, string modifier_type, int date)
{
	if(variable_ == "f_arrival_mod"){
		if(modifier_type=="plus")f_arrival_mod += (int)modifier;
		if(modifier_type=="replace")f_arrival_mod = (int)modifier;
		if(modifier_type=="return")return(f_arrival_mod);
	}
	if(variable_ == "flywayFood"){
		if(modifier_type=="plus")flywayFood += modifier;
		if(modifier_type=="replace")flywayFood = modifier;
		if(modifier_type=="return")return(flywayFood);
	}
	if(variable_ == "flywayPredation"){
			if(modifier_type=="plus")flywayPredation += modifier;
			if(modifier_type=="replace")flywayPredation = modifier;
			if(modifier_type=="return")return(flywayPredation);
		}
	if(variable_ == "arrival"){
			if(modifier_type=="plus") modifyarrival((int)modifier,(int)modifier);
			if(modifier_type=="replace")
			{
				date_a_dist = std::normal_distribution<float>(modifier,sd_arrival[0]);
				date_j_dist = std::normal_distribution<float>(modifier,sd_arrival[1]);
			}
			if(modifier_type=="return"){
					if(date<42) return(date_a_dist.mean());
						else return(date_j_dist.mean());
		
		}
	}

		if(variable_ == "mass")
		{
			if(modifier_type=="plus")
			{
				fuel_arrival[0] += modifier;
				fuel_arrival[1] += modifier;
			}
			if(modifier_type=="replace")
				{
				fuel_arrival[0] += modifier;
				fuel_arrival[1] += modifier;
				}
			if(modifier_type=="return")
				{
					if(date<42) return(fuel_arrival[0]) ;
						else return(fuel_arrival[1]);
				}
		}



return 0;
}


int find_ideal(FILE *simfile, int r,double delta_var, int survey_total, double survey_p, int date, string variable)
{ 	
	bool pIdealFound = false;
	bool idealN_found=false;
	// f_arrival_mod = 0;
	double previous_variable_value = 0;
	double deltaSign = -1;

/// Set up here, still need some variables
	runmodel();
	int siteL = siteCount[(1*(final_time+1)+date)];
	int siteS = siteCount[(0*(final_time+1)+date)];
	int totalBirds = siteL + siteS;
	double pLarge = (double)siteL / (double)totalBirds;
	int delta_T = survey_total - totalBirds;
	double delta_p = survey_p - pLarge;
	int p = 0;
	int k = 0;
	int finalNumbers;
	int prev_delta_total= survey_total - totalBirds;
	double prev_val;
	while(!idealN_found)
	{ //if(p>200 || k >200) return(1);
		if(p>1000)pIdealFound = true;
		if(k>400)idealN_found = true;

		runmodel();
		siteL = siteCount[(1*(final_time+1)+date)];
		siteS = siteCount[(0*(final_time+1)+date)];
		totalBirds = siteL + siteS;
		pLarge = (double)siteL / (double)totalBirds;
		prev_val = delta_p;
		delta_p = survey_p - pLarge;
		if(variable=="popsize") pIdealFound = true;
		if(!pIdealFound)
		{
			if(p==0)modify_variable(variable,(delta_var * deltaSign), "plus", date); //f_arrival_mod += (delta_var * deltaSign);
			else{
			if(SameSign(prev_val, delta_p) && p!= 0)
			{
				if(abs(prev_val) <= abs(delta_p) ) 
					{//f_arrival_mod = previous_variable_value;
						deltaSign *= -1;
					}
				modify_variable(variable,(delta_var * deltaSign), "plus", date);
				//f_arrival_mod += (delta_var * deltaSign);
				
			} else
			{
				if(abs(prev_val) < abs(delta_p)&& p!=0) {
					modify_variable(variable,previous_variable_value, "replace", date);
					//f_arrival_mod = previous_variable_value;
					// f_arrival_mod += (delta_var * deltaSign);
				pIdealFound = true; }
				// previous_variable_value = f_arrival_mod;
			}
		}
			// printf("%d\t%d\t%d\t%f\t%f\t%f\t%d\t%d\n",
			// 	modify_variable(variable,1, "return"), k, p, prev_val, delta_p, pLarge, siteL, siteS);
			previous_variable_value = modify_variable(variable,1, "return", date);
			p++;
			// if(pIdealFound) printf("-------------------------------------------------\n" );
		}

		if(pIdealFound) 
			{int i; int prev_birds[2];
				delta_T = survey_total-totalBirds;
				if(date<42) i = 0;
				else i = 1;
				int currentbirds = number_birds[i];
				if(k==0) number_birds[i] *= 1.2;
				else if(abs(delta_T)<10) {idealN_found=true;
											finalNumbers =number_birds[i]; } 
											else{
					if(!SameSign_i(prev_delta_total, delta_T))
						{ //cout << "WTF" << endl;
							number_birds[i] = (prev_birds[i]+currentbirds)/2 ;
						} else
						{
						if(delta_T<0){
						if(abs(prev_delta_total)>abs(delta_T)) number_birds[i] /=2;
						if(abs(prev_delta_total)<abs(delta_T)) number_birds[i] = prev_birds[i]/2; 
						}
						if(delta_T>0){
						if(abs(prev_delta_total)>abs(delta_T)) number_birds[i] *=2;
						if(abs(prev_delta_total)<abs(delta_T)) number_birds[i] = number_birds[i]*2; 
						}					}
						if(number_birds[i] < 100) number_birds[i] = 10000;
				}
				// printf("%d\t%d\t%d\t%d\t%d\t%d\t%d\n",
				//  k, p, prev_delta_total, delta_T,number_birds[i], siteL, siteS);
			previous_variable_value = modify_variable(variable,1, "return", date);
				// optimize finding bst fit of birds by searching through halfing difference of different values if sign changes.
					prev_delta_total = delta_T;
					prev_birds[i] = currentbirds;
					k++;
			}
			// cout<< f_arrival_mod << '\t' << k << "\t" << p << endl;
			
	}

	// if ((simfile = fopen(forFile.c_str(), "at")) == NULL)
	//     {
	//       printf("%s%s\n", "Error opening", forFile.c_str());
	//       exit(1);
	//     }
	int skippedBirds;
	if(date<42)skippedBirds = n_skippers[0];
	if(date>=42)skippedBirds = n_skippers[1];


    fprintf(simfile, "%s\t%d\t%f\t%d\t%d\t%d\t%f\t%d\t%d\t%d\n", variable.c_str(), date,(double)modify_variable(variable,1, "return", date),finalNumbers, p,k, pLarge, totalBirds,skippedBirds, r);
    // fclose(simfile);


	// printf("Final optimal value is %d - Final optimal numbers are %d\n",modify_variable(variable,1, "return"), finalNumbers  );
	return 0;
}


int main(int argc, char *argv[])
{	int rank, numprocs;
    MPI_Init(&argc, &argv);
	MPI_Comm_rank(MPI_COMM_WORLD, &rank);
	MPI_Comm_size(MPI_COMM_WORLD, &numprocs);
	string forFile = "/scratch/dhope/noU/optvar/surveyres_optimizedValues" + std::to_string(rank) + ".txt";
	// variable + std::to_string(date)+"_"+std::to_string(r) + ".txt";
	 FILE *simfile;
	    if ((simfile = fopen(forFile.c_str(), "wt")) == NULL)
	    {
	      printf("%s%s\n", "Error opening", forFile.c_str());
	      exit(1);
	    }
	    fprintf(simfile, "var\tdate\toptvar\toptN\tp\tk\tpLarge\ttotalBirds\tskippers\trun\n");
	    

	int baselineArrival = f_arrival_mod;
	double baselineFood = flywayFood;
	double baselinePred = flywayPredation;
	double baseline_fuel[2] = {fuel_arrival[0],fuel_arrival[1]};
	double baselineN = number_birds[0];
	// importData("/home/dhope/DSVM/noU/.dat/");  //"noU/.dat/");
	importData("surveyres.csv","/home/dhope/DSVM/noU/.dat/");  //"noU/.dat/");
	int nROWS = date_d.size();
	int scenarioRun =1;
	for(int r=0; r<50; r++){
		for(int row=0;row<nROWS; row++)
		{	
			if ((scenarioRun % numprocs) == rank)
			{find_ideal(simfile, r,1, totalBirds[row], pLarge[row], date_d[row],"popsize");
					number_birds[0]=baselineN;
					number_birds[1]=baselineN;} scenarioRun++;

			if ((scenarioRun % numprocs) == rank){		
					find_ideal(simfile, r,1, totalBirds[row], pLarge[row], date_d[row],"f_arrival_mod");
					f_arrival_mod = baselineArrival;}scenarioRun++;

			if ((scenarioRun % numprocs) == rank)
				{
					find_ideal(simfile, r,0.01, totalBirds[row], pLarge[row], date_d[row],"flywayFood");
					flywayFood=baselineFood;
				}scenarioRun++;

			if ((scenarioRun % numprocs) == rank)
				{
					find_ideal(simfile, r,0.01, totalBirds[row], pLarge[row], date_d[row],"flywayPredation");
					flywayPredation = baselinePred;
				}scenarioRun++;

			// if ((scenarioRun % numprocs) == rank)
			// 	{
			// 		find_ideal(r,1, totalBirds[row], pLarge[row], date_d[row],"arrival");
			// 		modifyarrival(0,0);
			// 			} scenarioRun++;

			// if ((scenarioRun % numprocs) == rank)
			// {
			// 	find_ideal(r,0.1, totalBirds[row], pLarge[row], date_d[row],"mass");
			// 	fuel_arrival[0] = baseline_fuel[0];
			// 	fuel_arrival[1] = baseline_fuel[1];
			// }	scenarioRun++;

		}
	}
	// cout << std::mean(date_a_dist) <<endl;
	// find_ideal(1, 1713, 0.595, 30,"f_arrival_mod");
	// find_ideal(1, 4031, 0.26, 58,"f_arrival_mod");
	fclose(simfile);
	MPI_Finalize();
	return 0;
}