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

int convertProp(int N, float p){
		int a_ = (int)round( p*(float)N);
		return(a_);
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

void modifyarrival(int deltaA, int deltaJ)
{

		int newA = arrival_dates[0] + deltaA;
		int newJ = arrival_dates[1] + deltaJ;
		date_a_dist = std::normal_distribution<float>(newA,sd_arrival[0]);
		date_j_dist = std::normal_distribution<float>(newJ,sd_arrival[1]);
}	



void OiledForward(double fitness[],int decisions[],
				double fitness_oiled[],int decisions_oiled[],
				std::vector<float> propOiled, 
	char const forFile[], bool plotout)
{
	using namespace std;
	if(plotout)
    {
	
		FILE *file2;                     //Clear output file
		if ((file2 = fopen(forFile, "wt")) == NULL)
		{
		printf("%s\n", "Error opening file");
		exit(1);
		}
		//~ //   fprintf(file2, "");
		fprintf(file2,"id\tAge\tt\tintFuel\tfuel\ts\tLoS\td(s,t,x)\tF(s,t,x)\n");
		fclose(file2);
	}



	
	int N_unoiled[2] = {convertProp(number_birds[0],(1-propOiled[0])), 
						convertProp(number_birds[1],(1-propOiled[1])) };
	int N_oiled[2] = {convertProp(number_birds[0],propOiled[0]), 
						convertProp(number_birds[1],propOiled[1]) };
	
	WESA unoiled_birds[(N_unoiled[0]+N_unoiled[1])];

	// WESA birds[(number_birds[0] + number_birds[1])];
	// printf("%d\n", sizeof(birds)/ sizeof(*birds));
	int id = 0;
	for(int a_j =0; a_j <=1; a_j++){
	for (int bird =0; bird<N_unoiled[a_j]; bird++){
		unoiled_birds[id].BirdArrive(id, a_j, fitness, decisions, "NULL.txt");
		if(!unoiled_birds[id].dead && !unoiled_birds[id].departed)unoiled_birds[id].migrate(fitness, decisions, "NULL.txt");
		id ++;
	}}
	// Change parameters for oiled birds;
	id = 0;
	// a_escape = 0.9*0.63;
	// b_escape = 5.3333;
	flywayFood = 0.2;
	flywayPredation = 2.0;
	// modifypredation(flywayPredation);
	// modifyfood(flywayFood);


	// Run through oiled birds
	WESA oiled_birds[(N_oiled[0]+N_oiled[1])];
	for(int a_j =0; a_j <=1; a_j++){
		for (int bird =0; bird<N_oiled[a_j]; bird++){
			oiled_birds[id].BirdArrive(id, a_j, fitness_oiled, decisions_oiled, "NULL.txt");
			if(!oiled_birds[id].dead && !oiled_birds[id].departed)oiled_birds[id].migrate(fitness_oiled, decisions_oiled, "NULL.txt");
		id ++;


					}}
	// a_escape = 0.9;
	// b_escape = 1.3333;
	flywayFood = 1.;
	flywayPredation = 1.0;
	// modifypredation(flywayPredation);
	// modifyfood(flywayFood);
					
}

int montycarlo(string filename)
{	string forFile;
	bool fmf[2]={false, true};
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

	// for (int pk = 0;pk<=1;pk++){

			first_move_free = true;//fmf[pk];
			if(first_move_free == true) forFile = "/scratch/dhope/noU/scenarios/pk/"+ filename;
			else forFile = "/scratch/dhope/noU/scenarios/no_pk/"+ filename;

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
//}

	// fullForward(fitness,decisions); // Simulate the forwards model
	// printarr(fitness);
 return 0;

}




int Oil_montycarlo(std::vector<float>propOiled, string filename)
{
	string forFile = "/scratch/dhope/noU/scenarios/pk/"+ filename;
		FILE *simfile;
	    if ((simfile = fopen(forFile.c_str(), "wt")) == NULL)
	    {
	      printf("%s%s\n", "Error opening", forFile.c_str());
	      exit(1);
	    }
	    fprintf(simfile, "run\ttime\tsite0\tsite1\n");
	    fclose(simfile);
	forwardsOutput = false;
	first_move_free = true;

	double fitness[(max_fuel_load + 1)*(final_time+1)*(number_of_sites + 1) ]; // Define the fitness array
	std::fill_n(fitness, (max_fuel_load + 1)*(final_time+1)*(number_of_sites + 1), -9);
	int decisions[(max_fuel_load + 1)*(final_time+1)*(number_of_sites + 1)] ; // Define the decision array
	std::fill_n(decisions, (max_fuel_load + 1)*(final_time+1)*(number_of_sites + 1), -9);
	fullback(fitness,decisions, "NULL.txt", false	); // Run the backwards programming equation

	// Change parameters
	// a_escape = 0.9*0.63;
	// b_escape = 5.3333;
	flywayFood = 0.1;
	flywayPredationX = 7E-1;
	max_p_fuel_departure=0.1;
	min_fuel_load = 0.02;
	a=2;
	// flywayPredation = 2.0;
		// modifypredation(flywayPredation);
	// modifyfood(flywayFood);
	double fitness_oiled[(max_fuel_load + 1)*(final_time+1)*(number_of_sites + 1) ]; // Define the fitness array
	std::fill_n(fitness_oiled, (max_fuel_load + 1)*(final_time+1)*(number_of_sites + 1), -9);
	int decisions_oiled[(max_fuel_load + 1)*(final_time+1)*(number_of_sites + 1)] ; // Define the decision array
	std::fill_n(decisions_oiled, (max_fuel_load + 1)*(final_time+1)*(number_of_sites + 1), -9);
	fullback(fitness_oiled,decisions_oiled, "NULL.txt", false	); // Run the backwards programming equation
	// a_escape = 0.9;
	// b_escape = 1.3333;
	flywayFood = 1.;
	flywayPredationX = 7E-2;
	max_p_fuel_departure=0.3;
	min_fuel_load = 0.01;
	a=3;

	// modifyfood(flywayFood);
	// flywayPredation = 1.0;
	// modifypredation(flywayPredation);

	for(int run=0;run<nruns; run++){
	
		int seed = std::chrono::high_resolution_clock::now().time_since_epoch().count();
		std::mt19937 e2(seed);
	
		std::fill_n(siteCount, (3)*(91), 0);
		OiledForward(fitness,decisions,fitness_oiled, decisions_oiled,propOiled, "NULL2.txt", false); // Simulate the forwards model
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
			    		if(s==0){fprintf(simfile, "%d\t%d\t%d", senRun, t, siteCount[(s*(final_time+1)+t)]);}
			    		else fprintf(simfile, "\t%d", siteCount[(s*(final_time+1)+t)]);
			  		} fprintf(simfile, "\n"); 
		  	} else{continue;} 
		    }
		    fclose(simfile);
		    // cout<<food_dist(e2)<< endl;
	}

	// fullForward(fitness,decisions); // Simulate the forwards model
	// printarr(fitness);
 return 0;

}




int main(int argc, char *argv[]){
	// Original modifications to model from Chapter 2
	// 1. Departure benefit
	// dep_benefit = 0.958;
	// first_move_free = true;
	// modifypredation(1);
	// pred_time_shape = pred_time_shape * 0.5;
	// fuel_arrival[1] = 1.0;
	// proportionF_safesite = 0.3;
	// modifyfood(1);
	// cost_move = 0.21;
	using namespace std;
    int rank, numprocs;
    MPI_Init(&argc, &argv);
	MPI_Comm_rank(MPI_COMM_WORLD, &rank);
	MPI_Comm_size(MPI_COMM_WORLD, &numprocs);


	// string scenario;
	string scenarios[9] = {
		"predArrival",
		"popDecl", 
		"predPopulation",
		"foodChange",
		"wesaArrival",
		"predANDwesaArrival",
		"arrivalMass",
		"oil",
		"poison"};

	int rank_quotent = rank /3;
	int rank_remainder = rank % 3;
	string scenario = scenarios[rank_quotent].c_str();
		// string scenario = "predANDwesaArrival";
	cout<<scenario<<"\t"<<rank_remainder<<endl;
	if(scenario == "predArrival"){ 
		// Set and run parameters for model scenarios
		// pred_time_shape = 0.066233;
		// pred_max = -3.611108; 
		if (rank_remainder==0)
			{f_arrival_mod = 0;
				montycarlo("baselinePred.txt");}
		// pred_max = -2.713038; 
		// pred_time_shape = 0.083713;
		if (rank_remainder==1)
		{		f_arrival_mod = 10;
		montycarlo("EarlyArrival.txt");}
		// pred_time_shape = 0.048753;
		// pred_max = -4.509178;
		if (rank_remainder==2)
			{f_arrival_mod = -10;
				montycarlo("LateArrival.txt");}
		// pred_time_shape = 0.066233;
		// pred_max = -3.611108;
		f_arrival_mod = 0;
	}
	if (scenario == "popDecl")
	{ int baseNumbers = number_birds[0];
		if (rank_remainder==0)
			{number_birds[0] = baseNumbers*.9;
				number_birds[1] = baseNumbers*.9;
				montycarlo("tenpercentDecl.txt");
		
				number_birds[0] = baseNumbers*.8;
				number_birds[1] = baseNumbers*.8;
				montycarlo("twentypercentDecl.txt");}

		if (rank_remainder==1)
			{number_birds[0] = baseNumbers*.7;
				number_birds[1] = baseNumbers*.7;
				montycarlo("thirtypercentDecl.txt");
		
				number_birds[0] = baseNumbers*1.1;
				number_birds[1] = baseNumbers*1.1;
				montycarlo("tenpercentInc.txt");}

		if (rank_remainder==2)
			{number_birds[0] = baseNumbers*1.3;
				number_birds[1] = baseNumbers*1.3;
				montycarlo("thirtypercentInc.txt");}

		// number_birds[0] = 1000;
		// number_birds[1] = 800;
		// montycarlo("tenpercentDeclJ.txt");

		// number_birds[0] = 1000;
		// number_birds[1] = 600;
		// montycarlo("twentypercentDeclJ.txt");

		// number_birds[0] = 1000;
		// number_birds[1] = 400;
		// montycarlo("thirtypercentDeclJ.txt");

		number_birds[0] = baseNumbers;
		number_birds[1] = baseNumbers;
	}
	if (scenario == "oil")
	{
		// cout<< "Not yet" << endl;
		std::vector<float> propOiled = {0.1,0.1};
		if (rank_remainder==0)
			{Oil_montycarlo(propOiled, "MontyOil_0.1.txt");
				 propOiled = {0.2,0.2};
				Oil_montycarlo(propOiled, "MontyOil_0.2.txt");
				 propOiled = {0.3,0.3};
				Oil_montycarlo(propOiled, "MontyOil_0.3.txt");
				 propOiled = {0.4,0.4};
				Oil_montycarlo(propOiled, "MontyOil_0.4.txt");}
		 if (rank_remainder==1)
		 	{propOiled = {0.5,0.5};
		 		Oil_montycarlo(propOiled, "MontyOil_0.5.txt");
		 		 propOiled = {0.6,0.6};
		 		Oil_montycarlo(propOiled, "MontyOil_0.6.txt");
		 		 propOiled = {0.7,0.7};
		 		Oil_montycarlo(propOiled, "MontyOil_0.7.txt");}
		 if (rank_remainder==2){propOiled = {0.8,0.8};
		 		Oil_montycarlo(propOiled, "MontyOil_0.8.txt");
		 		 propOiled = {0.9,0.9};
		 		Oil_montycarlo(propOiled, "MontyOil_0.9.txt");
		 		 propOiled = {0.0,0.6};
		 		Oil_montycarlo(propOiled, "MontyOilJ_0.6.txt");
		 		
		 		 propOiled = {0.9,0.6};
		 		Oil_montycarlo(propOiled, "MontyOilA_0.9.txt");}



	}

	if(scenario == "predPopulation")
	{


		double baselinepred =flywayPredation;
		// modifypredation(0.);
		
		if (rank_remainder==0)
			{montycarlo("predPop_0.txt");
				flywayPredation = baselinepred*0.001;
				montycarlo("predPop_0.001.txt");
				flywayPredation = baselinepred*0.01;
				montycarlo("predPop_0.01.txt");
				flywayPredation = baselinepred*0.1;
				montycarlo("predPop_0.1.txt");}
		if (rank_remainder==1)
			{flywayPredation = baselinepred*0.4;
				montycarlo("predPop_0.4.txt");
				flywayPredation = baselinepred*0.8;
				montycarlo("predPop_0.8.txt");
				flywayPredation = baselinepred*1.5;
				montycarlo("predPop_1.5.txt");}
		if (rank_remainder==2){flywayPredation = baselinepred*2.;
				montycarlo("predPop_2.txt");
				flywayPredation = baselinepred*3.;
				montycarlo("predPop_3.txt");
				flywayPredation = baselinepred*5.;
				montycarlo("predPop_5.txt");}
		flywayPredation = baselinepred*1.;
	}

	if(scenario == "foodChange")
	{

		double baselinefood = flywayFood;
		if (rank_remainder==0)
			{flywayFood=baselinefood*0.01;
				montycarlo("flywayFood_0.01.txt");
				flywayFood=baselinefood*0.1;
				montycarlo("flywayFood_0.1.txt");
				flywayFood=baselinefood*0.5;
				montycarlo("flywayFood_0.4.txt");}
		if (rank_remainder==1){flywayFood=baselinefood*0.75;
				montycarlo("flywayFood_0.8.txt");
				flywayFood=baselinefood*1.5;
				montycarlo("flywayFood_1.5.txt");}
		if (rank_remainder==2){flywayFood=baselinefood*2.;
				montycarlo("flywayFood_2.txt");
				flywayFood=baselinefood*3.;
				montycarlo("flywayFood_3.txt");}
		flywayFood=baselinefood*1.;
	}

	if(scenario == "wesaArrival")
	{
		// cout<<"CAT"<<endl;		
		if (rank_remainder==0){modifyarrival(-5,0);
				montycarlo("Early_Adult.txt");
				modifyarrival(0,-5);
				montycarlo("Early_Juven.txt");}
		if (rank_remainder==1){modifyarrival(5,0);
				montycarlo("Late_Adult.txt");
				modifyarrival(0,5);
				montycarlo("Late_Juven.txt");}
		if (rank_remainder==2){modifyarrival(-5,-5);
				montycarlo("Early_both.txt");
				modifyarrival(5,5);
				montycarlo("Late_both.txt");}
		modifyarrival(0,0);
	}

	if(scenario == "predANDwesaArrival")
	{
		// Changes in both predation and wesa arrival
		// Early Falcon Early Birds
		// pred_max = -2.713038; 
		// pred_time_shape = 0.083713;
		if (rank_remainder==0){f_arrival_mod = 10;
				modifyarrival(-5,-5);
				montycarlo("Combo_EarlyFalc_EarlyWESA.txt");}

		// Early Falcon Late Birds
		// pred_max = -2.713038;
		// pred_time_shape = 0.083713;
		if (rank_remainder==1)
			{f_arrival_mod = 10;
				modifyarrival(5,5);
				montycarlo("Combo_EarlyFalc_LateWESA.txt");
		}
		
		if (rank_remainder==2)
		{// Late Falcon Early Birds
				// pred_max = -4.509178;
				// pred_time_shape = 0.048753;
			f_arrival_mod=-10;
				modifyarrival(-5,-5);
				montycarlo("Combo_LateFalc_EarlyWESA.txt");}

		if (rank_remainder==0)
		{// Late Falcon Late Birds
				// pred_time_shape = 0.048753;
				// pred_max = -4.509178;
				f_arrival_mod = -10;
				modifyarrival(5,5);
				montycarlo("Combo_LateFalc_LateWESA.txt");}
		f_arrival_mod = 0;
		// pred_time_shape = 0.066233;
		// pred_max = -3.611108;
		modifyarrival(0,0);
	}

	if(scenario =="poison")
	{
		// Lower fuel loads and fueling ability

		cout<<"Rat"<<endl;



	}

	if(scenario == "arrivalMass")
	{
		// Changes in both predation and wesa arrival
		// Early Falcon Early Birds
		double baseline_fuel[2] = {fuel_arrival[0],fuel_arrival[1]};
		if (rank_remainder==0){fuel_arrival[0] = baseline_fuel[0]*0.8;
				fuel_arrival[1] = baseline_fuel[1]*0.8;
				montycarlo("ArriveLight.txt");
				fuel_arrival[0] = baseline_fuel[0]*1.2;
				fuel_arrival[1] = baseline_fuel[1]*1.2;
				montycarlo("ArriveHeavy.txt");}
		if (rank_remainder==1)
			{fuel_arrival[0] = baseline_fuel[0]*0.6;
				fuel_arrival[1] = baseline_fuel[1]*0.6;
				montycarlo("ArriveVLight.txt");}
		if (rank_remainder==2)
			{fuel_arrival[0] = baseline_fuel[0]*1.4;
				fuel_arrival[1] = baseline_fuel[1]*1.4;
				montycarlo("ArriveVHeavy.txt");}
		fuel_arrival[0] = baseline_fuel[0];
		fuel_arrival[1] = baseline_fuel[1];
	}

	MPI_Finalize();
	return 0;
	
}