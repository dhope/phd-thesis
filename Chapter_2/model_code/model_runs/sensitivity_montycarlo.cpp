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

int runmc(int senR)
{	
	string forFile;
	bool fmf[2]={true, false};
	printf("Run number: %d\n",senR );
	
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
	// cout<< *(decisions + addr(15, 10, 0)) << endl;
	for (int pk = 0;pk<=1;pk++){
		// string forFile = "./output/SenAnal/noU/SenRes_"+ std::to_string(senR)+".txt";//MC/MC_Counts_alllim.txt";//+std::to_string(run)+txt ;
			first_move_free = fmf[pk];
			if(first_move_free == true) forFile = "./OrganizedResults/noU/sensitivity/pk/SenRes_"+ std::to_string(senR)+".txt";
			else forFile = "./OrganizedResults/noU/sensitivity/no_pk/SenRes_"+ std::to_string(senR)+".txt";

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
				// string forFile = "./output/MC/MC_Counts"+std::to_string(run)+txt ;
				// FILE *simfile;
			 //    if ((simfile = fopen(forFile.c_str(), "wt")) == NULL)
			 //    {
			 //      printf("%s%s\n", "Error opening", forFile.c_str());
			 //      exit(1);
			 //    }
			 //    fprintf(simfile, "run\ttime\tsite0\tsite1\n");
			 //    fclose(simfile);
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
					    		if(s==0){fprintf(simfile, "%d\t%d\t%d", senR, t, siteCount[(s*(final_time+1)+t)]);}
					    		else fprintf(simfile, "\t%d", siteCount[(s*(final_time+1)+t)]);
					  		} fprintf(simfile, "\n"); 
				  	} else{continue;} 
				    }
				    fclose(simfile);
				    // cout<<food_dist(e2)<< endl;
			}
		}

	// fullForward(fitness,decisions); // Simulate the forwards model
	// printarr(fitness);
 return 0;

}


int main()
{	
	first_move_free = false;
	double elastmod;
	double elmods[2] = {1.05, 0.95};
	int plusminus = 1;
	senRun = 0;
	// Set up and run baseline
	senRun++;
	
	runmc(senRun);
	// Run through modifications twice. 
	//Once for modifying plus and one negative
	while(plusminus<3){
		elastmod = elmods[plusminus-1];

		senRun++;

		wetleanbodymass = wetleanbodymass*elastmod;
		runmc(senRun);
		wetleanbodymass = wetleanbodymass/elastmod;
		senRun++;

		cost_move= cost_move*elastmod;
		runmc(senRun);
		cost_move= cost_move/elastmod;
		senRun++;


		pred_max=pred_max*elastmod;
		runmc(senRun);
		pred_max=pred_max/elastmod;
		senRun++;

		pred_time_shape=pred_time_shape*elastmod;
		runmc(senRun);
		pred_time_shape=pred_time_shape/elastmod;
		senRun++;

		if (plusminus==1) f_arrival_mod=f_arrival_mod+1;
		else f_arrival_mod=f_arrival_mod-1;
		runmc(senRun);
		if (plusminus==1) f_arrival_mod=f_arrival_mod-1;
		else f_arrival_mod=f_arrival_mod+1;
		senRun++;

		// a_escape=a_escape*elastmod;
		// runmc(senRun);
		// senRun++;
		// b_escape=b_escape*elastmod;
		// runmc(senRun);
		// senRun++;
		// fuel_modifier=fuel_modifier*elastmod;
		// runmc(senRun);
		// fuel_modifier=fuel_modifier/elastmod;
		// senRun++;


		min_fuel_load=min_fuel_load*elastmod;
		runmc(senRun);
		min_fuel_load=min_fuel_load/elastmod;
		senRun++;

		max_p_fuel_departure=max_p_fuel_departure*elastmod;
		runmc(senRun);
		max_p_fuel_departure=max_p_fuel_departure/elastmod;
		senRun++;

		shape_fuel_departure=shape_fuel_departure*elastmod;
		runmc(senRun);
		shape_fuel_departure=shape_fuel_departure/elastmod;
		senRun++;

		flywayPredation=flywayPredation*elastmod;
		runmc(senRun);
		flywayPredation=flywayPredation/elastmod;
		senRun++;

		flywayFood=flywayFood*elastmod;
		runmc(senRun);
		flywayFood=flywayFood/elastmod;
		senRun++;

		sd_food=sd_food*elastmod;
		runmc(senRun);
		sd_food=sd_food/elastmod;
		senRun++;

		sd_danger=sd_danger*elastmod;
		runmc(senRun);
		sd_danger=sd_danger/elastmod;
		senRun++;

		sd_arrival=sd_arrival*elastmod;
		runmc(senRun);
		sd_arrival=sd_arrival/elastmod;
		senRun++;

		sd_arrival_fuel=sd_arrival_fuel*elastmod;
		runmc(senRun);
		sd_arrival_fuel=sd_arrival_fuel/elastmod;
		senRun++;

		fuel_arrival[0]=fuel_arrival[0]*elastmod;
		runmc(senRun);
		fuel_arrival[0]=fuel_arrival[0]/elastmod;
		senRun++;

		fuel_arrival[1]=fuel_arrival[1]*elastmod;
		runmc(senRun);
		fuel_arrival[1]=fuel_arrival[1]/elastmod;
		senRun++;

		if (plusminus==1) arrival_dates[0]=arrival_dates[0]+1;
		else arrival_dates[0]=arrival_dates[0]-1;
		runmc(senRun);
		if (plusminus==1) arrival_dates[0]=arrival_dates[0]-1;
		else arrival_dates[0]=arrival_dates[0]+1;
		senRun++;

		if (plusminus==1) arrival_dates[1] =arrival_dates[1]+1;
		else arrival_dates[1] =arrival_dates[1]-1;
		runmc(senRun);
		if (plusminus==1) arrival_dates[1] =arrival_dates[1]-1;
		else arrival_dates[1] =arrival_dates[1]+1;
		senRun++;

		number_birds[0]=number_birds[0]*elastmod;
		// if (plusminus == 1) number_birds[0]=number_birds[0]+1;
		// else number_birds[0]=number_birds[0]-1;
		runmc(senRun);
		number_birds[0]=number_birds[0]/elastmod;
		senRun++;

		number_birds[1]=number_birds[1]*elastmod;
		// if (plusminus == 1) number_birds[1]=number_birds[1]+1;
		// else number_birds[1]=number_birds[1]-1;
		runmc(senRun);
		number_birds[1]=number_birds[1]/elastmod;
		senRun++;

		// dep_benefit=dep_benefit*elastmod;
		// runmc(senRun);
		// dep_benefit=dep_benefit/elastmod;
		// senRun++;
		if (plusminus==1) dailyfuelcost=dailyfuelcost+.1;
		else dailyfuelcost=dailyfuelcost-.1;
		runmc(senRun);
		if (plusminus==1) dailyfuelcost=dailyfuelcost-.1;
		else dailyfuelcost=dailyfuelcost+.1;
		senRun++;

		Danger_small_site=Danger_small_site*elastmod;
		runmc(senRun);
		Danger_small_site=Danger_small_site/elastmod;
		senRun++;
		
		Food_multiplier_small_site=Food_multiplier_small_site*elastmod;
		runmc(senRun);
		Food_multiplier_small_site=Food_multiplier_small_site/elastmod;
		senRun++;

		a = a*elastmod;
		runmc(senRun);
		a = a/elastmod;
		senRun++;

		flywayPredationX=flywayPredationX *elastmod; 
		runmc(senRun);
		flywayPredationX=flywayPredationX /elastmod; 
		senRun++;

		flywayPredationDep=flywayPredationDep*elastmod;
		runmc(senRun);
		flywayPredationDep=flywayPredationDep/elastmod;
		senRun++;

		attackrate=attackrate*elastmod;
		runmc(senRun);
		attackrate=attackrate/elastmod;




		plusminus++;

	}

		

		if(b!=0)
		{ for(int i=0;i<2;i++)
			{senRun++;
			elastmod =elmods[i];
			b=b*elastmod;
			runmc(senRun);
			b=b*elastmod;
			}
		}

	MPI_Finalize();
	return 0;
}