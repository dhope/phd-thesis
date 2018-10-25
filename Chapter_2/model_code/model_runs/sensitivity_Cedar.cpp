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
#include <algorithm>
#include <mpi.h> // Boost to use mpi
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


void modifypredation(){
	// proportionD_safesite = pm;
	siteDanger[0] = Danger_small_site; 
	siteDanger[1] = 1;
	// siteDanger[2] = proportionD_safesite*(1-dep_benefit)*flywayPredation;
}

void modifyfood(){
	// proportionF_safesite = fm;
	siteFuelIntake[0] = Food_multiplier_small_site; 
	siteFuelIntake[1] = 1;
	// siteFuelIntake[2] = proportionF_safesite*flywayFood;
}

int runmc(int senR, int rank, int numprocs)
{	
	string forFile;
	forFile = "/scratch/dhope/noU/Sensitivity_run_" +std::to_string(senR)+ "proc"+std::to_string(rank)+".txt" ;
				FILE *simfile;
			    if ((simfile = fopen(forFile.c_str(), "wt")) == NULL)
			    {
			      printf("%s%s\n", "Error opening", forFile.c_str());
			      exit(1);
			    }
			    fprintf(simfile, "pk\trun\ttime\tsite0\tsite1\n");
			    fclose(simfile);


	bool fmf[2]={true, false};
	printf("Run number: %d\tProc: %d\n",senR, rank );
	
	forwardsOutput = false;
	// seed = chrono::high_resolution_clock::now().time_since_epoch().count();
	// std::mt19937 mt_rand(seed);
	string txt = ".txt";

	modifypredation();
    modifyfood();

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
			std::fill_n(siteCount, (number_of_sites+1)*(final_time+1), 0);
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
			    		if(s==0){fprintf(simfile, "%d\t%d\t%d\t%d",pk, senR, t, siteCount[(s*(final_time+1)+t)]);}
			    		else fprintf(simfile, "\t%d", siteCount[(s*(final_time+1)+t)]);
			  		} fprintf(simfile, "\n"); 
			  	} else{continue;} 
		    } fclose(simfile);
	}
		
	}
 return 0;
}


int main(int argc, char *argv[])
{	
	// Setup
	using namespace std;
    int rank, numprocs;
    MPI_Init(&argc, &argv);
	MPI_Comm_rank(MPI_COMM_WORLD, &rank);
	MPI_Comm_size(MPI_COMM_WORLD, &numprocs);
	first_move_free = false;
	double elastmod;
	double elmods[2] = {1.05, 0.95};
	int plusminus = 1;
	senRun = 0;
	// Set up and run baseline
	senRun++;
	if(rank==0) runmc(senRun,rank, numprocs);
	// Run through modifications twice. 
	//Once for modifying plus and one negative
	while(plusminus<3){
		elastmod = elmods[plusminus-1];

		senRun++;

		wetleanbodymass = wetleanbodymass*elastmod;
		if(rank==1) runmc(senRun,rank, numprocs);
		wetleanbodymass = wetleanbodymass/elastmod;
		senRun++;

		cost_move= cost_move*elastmod;
		if(rank==2) runmc(senRun,rank, numprocs);
		cost_move= cost_move/elastmod;
		senRun++;


		pred_max=pred_max*elastmod;
		if(rank==3) runmc(senRun,rank, numprocs);
		pred_max=pred_max/elastmod;
		senRun++;

		pred_time_shape=pred_time_shape*elastmod;
		if(rank==4) runmc(senRun,rank, numprocs);
		pred_time_shape=pred_time_shape/elastmod;
		senRun++;

		if (plusminus==1) f_arrival_mod=f_arrival_mod+1;
		else f_arrival_mod=f_arrival_mod-1;
		if(rank==5) runmc(senRun,rank, numprocs);
		if (plusminus==1) f_arrival_mod=f_arrival_mod-1;
		else f_arrival_mod=f_arrival_mod+1;
		senRun++;

		// a_escape=a_escape*elastmod;
		// runmc(senRun,rank, numprocs);
		// senRun++;
		// b_escape=b_escape*elastmod;
		// runmc(senRun,rank, numprocs);
		// senRun++;
		// fuel_modifier=fuel_modifier*elastmod;
		// runmc(senRun,rank, numprocs);
		// fuel_modifier=fuel_modifier/elastmod;
		// senRun++;


		min_fuel_load=min_fuel_load*elastmod;
		if(rank==6) runmc(senRun,rank, numprocs);
		min_fuel_load=min_fuel_load/elastmod;
		senRun++;

		

		distance_to_migrate=distance_to_migrate*elastmod;
		if(rank==7) runmc(senRun,rank, numprocs);
		distance_to_migrate=distance_to_migrate/elastmod;
		senRun++;

		flight_par=flight_par*elastmod;
		if(rank==8) runmc(senRun,rank, numprocs);
		flight_par=flight_par/elastmod;
		senRun++;		

		// max_p_fuel_departure=max_p_fuel_departure*elastmod;
		// if(rank==7) runmc(senRun,rank, numprocs);
		// max_p_fuel_departure=max_p_fuel_departure/elastmod;
		// senRun++;

		// shape_fuel_departure=shape_fuel_departure*elastmod;
		// if(rank==8) runmc(senRun,rank, numprocs);
		// shape_fuel_departure=shape_fuel_departure/elastmod;
		// senRun++;

		flywayPredation=flywayPredation*elastmod;
		if(rank==9) runmc(senRun,rank, numprocs);
		flywayPredation=flywayPredation/elastmod;
		senRun++;

		flywayFood=flywayFood*elastmod;
		if(rank==10) runmc(senRun,rank, numprocs);
		flywayFood=flywayFood/elastmod;
		senRun++;

		sd_food=sd_food*elastmod;
		if(rank==11) runmc(senRun,rank, numprocs);
		sd_food=sd_food/elastmod;
		senRun++;

		sd_danger=sd_danger*elastmod;
		if(rank==12) runmc(senRun,rank, numprocs);
		sd_danger=sd_danger/elastmod;
		senRun++;

		sd_arrival[0]=sd_arrival[0]*elastmod;
		if(rank==13) runmc(senRun,rank, numprocs);
		sd_arrival[0]=sd_arrival[0]/elastmod;
		senRun++;

		sd_arrival_fuel=sd_arrival_fuel*elastmod;
		if(rank==14) runmc(senRun,rank, numprocs);
		sd_arrival_fuel=sd_arrival_fuel/elastmod;
		senRun++;

		fuel_arrival[0]=fuel_arrival[0]*elastmod;
		if(rank==15) runmc(senRun,rank, numprocs);
		fuel_arrival[0]=fuel_arrival[0]/elastmod;
		senRun++;

		fuel_arrival[1]=fuel_arrival[1]*elastmod;
		if(rank==16) runmc(senRun,rank, numprocs);
		fuel_arrival[1]=fuel_arrival[1]/elastmod;
		senRun++;

		if (plusminus==1) arrival_dates[0]=arrival_dates[0]+1;
		else arrival_dates[0]=arrival_dates[0]-1;
		if(rank==17) runmc(senRun,rank, numprocs);
		if (plusminus==1) arrival_dates[0]=arrival_dates[0]-1;
		else arrival_dates[0]=arrival_dates[0]+1;
		senRun++;

		if (plusminus==1) arrival_dates[1] =arrival_dates[1]+1;
		else arrival_dates[1] =arrival_dates[1]-1;
		if(rank==18) runmc(senRun,rank, numprocs);
		if (plusminus==1) arrival_dates[1] =arrival_dates[1]-1;
		else arrival_dates[1] =arrival_dates[1]+1;
		senRun++;

		number_birds[0]=number_birds[0]*elastmod;
		// if (plusminus == 1) number_birds[0]=number_birds[0]+1;
		// else number_birds[0]=number_birds[0]-1;
		if(rank==19) runmc(senRun,rank, numprocs);
		number_birds[0]=number_birds[0]/elastmod;
		senRun++;

		number_birds[1]=number_birds[1]*elastmod;
		// if (plusminus == 1) number_birds[1]=number_birds[1]+1;
		// else number_birds[1]=number_birds[1]-1;
		if(rank==20) runmc(senRun,rank, numprocs);
		number_birds[1]=number_birds[1]/elastmod;
		senRun++;

		// dep_benefit=dep_benefit*elastmod;
		// runmc(senRun,rank, numprocs);
		// dep_benefit=dep_benefit/elastmod;
		// senRun++;
		if (plusminus==1) dailyfuelcost=dailyfuelcost+.1;
		else dailyfuelcost=dailyfuelcost-.1;
		if(rank==21) runmc(senRun,rank, numprocs);
		if (plusminus==1) dailyfuelcost=dailyfuelcost-.1;
		else dailyfuelcost=dailyfuelcost+.1;
		senRun++;

		Danger_small_site=Danger_small_site*elastmod;
		if(rank==22) runmc(senRun,rank, numprocs);
		Danger_small_site=Danger_small_site/elastmod;
		senRun++;
		
		Food_multiplier_small_site=Food_multiplier_small_site*elastmod;
		if(rank==23) runmc(senRun,rank, numprocs);
		Food_multiplier_small_site=Food_multiplier_small_site/elastmod;
		senRun++;

		a = a*elastmod;
		if(rank==24) runmc(senRun,rank, numprocs);
		a = a/elastmod;
		senRun++;

		flywayPredationX=flywayPredationX *elastmod; 
		if(rank==25) runmc(senRun,rank, numprocs);
		flywayPredationX=flywayPredationX /elastmod; 
		senRun++;

		flywayPredationDep=flywayPredationDep*elastmod;
		if(rank==26) runmc(senRun,rank, numprocs);
		flywayPredationDep=flywayPredationDep/elastmod;
		senRun++;

		attackrate=attackrate*elastmod;
		if(rank==27) runmc(senRun,rank, numprocs);
		attackrate=attackrate/elastmod;
		senRun++;
		
		settle_time=settle_time*elastmod;
		if(rank==28) runmc(senRun,rank, numprocs);
		settle_time=settle_time/elastmod;
		senRun++;
		
		fuel_loading_rate=fuel_loading_rate*elastmod;
		if(rank==29) runmc(senRun,rank, numprocs);
		fuel_loading_rate=fuel_loading_rate/elastmod;
		senRun++;
		
		falcon_speed_of_migration=falcon_speed_of_migration*elastmod;
		if(rank==30) runmc(senRun,rank, numprocs);
		falcon_speed_of_migration=falcon_speed_of_migration/elastmod;
		senRun++;
		
		maxSeasonal=maxSeasonal*elastmod;
		if(rank==31) runmc(senRun,rank, numprocs);
		maxSeasonal=maxSeasonal/elastmod;
		senRun++;
		
		breeding_falc=breeding_falc*elastmod;
		if(rank==32) runmc(senRun,rank, numprocs);
		breeding_falc=breeding_falc/elastmod;
		senRun++;
		
		departureBreedingpop=departureBreedingpop*elastmod;
		if(rank==33) runmc(senRun,rank, numprocs);
		departureBreedingpop=departureBreedingpop/elastmod;
		senRun++;
		
		a_mod=a_mod*elastmod;
		if(rank==34) runmc(senRun,rank, numprocs);
		a_mod=a_mod/elastmod;
		senRun++;
		
		wintering_mortality=wintering_mortality*elastmod;
		if(rank==35) runmc(senRun,rank, numprocs);
		wintering_mortality=wintering_mortality/elastmod;
		senRun++;

		sd_arrival[1]=sd_arrival[1]*elastmod;
		if(rank==1) runmc(senRun,rank, numprocs);
		sd_arrival[1]=sd_arrival[1]/elastmod;
		


		plusminus++;

	}

		

		if(b!=0 && rank==0 )
		{ for(int i=0;i<2;i++)
			{senRun++;
			elastmod =elmods[i];
			b=b*elastmod;
			runmc(senRun,rank, numprocs);
			b=b*elastmod;
			}
		}

	MPI_Finalize();
	return 0;
}