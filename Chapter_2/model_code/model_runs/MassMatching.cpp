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


int runmod(string backfilename, string forfilename, string directory, string directory_pk)
{
	auto filename = directory + backfilename; // File name for backwards output
	double fitness[(max_fuel_load + 1)*(final_time+1)*(number_of_sites + 1) ]; // Define the fitness array
	std::fill_n(fitness, (max_fuel_load + 1)*(final_time+1)*(number_of_sites + 1), -9);
	int decisions[(max_fuel_load + 1)*(final_time+1)*(number_of_sites + 1)] ; // Define the decision array
	std::fill_n(decisions, (max_fuel_load + 1)*(final_time+1)*(number_of_sites + 1), -9);
	// cout<< *(decisions + addr(15, 10, 0)) << endl;
	// printarr(fitness);
	cout<< filename.c_str() << endl;
	fullback(fitness,decisions, filename.c_str(), true	); // Run the backwards programming equation
	// cout<< *(decisions + addr(15, 10, 0)) << endl;
	
	// Run without prior knowledge
	first_move_free = false;
	string for_nopk = directory +forfilename;
	cout<< for_nopk.c_str() << endl;
	fullForward(fitness,decisions, for_nopk.c_str()); // Simulate the forwards model
	// Run with prior knowledge
	string for_pk = directory_pk +forfilename; 
	first_move_free = true;
	cout<< for_pk.c_str() << endl;
	fullForward(fitness,decisions,for_pk.c_str() ); // Simulate the forwards model

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
	// modifypredation(1);
	
	string basedir = "./OrganizedResults/noU/MassCorrection/";
	// Loop through mut, pk, baseline, low, high
	string knowledgetypes[2] = {"PK/", "noPK/"};
	string modelttype[3] = {"mut/", "deltaf/", "changed_x/"};
	string runs[3] = {"baseline/", "lowpred/", "highpred/"};
	int mt = 2;
	// for(int mt =0; mt<=1;mt++){
		// if(modelttype[mt]=="mut/")
		// {
		// 	pred_time_shape = pred_time_shape * 0.5;
		// 	fuel_arrival[1] = 1.0;
		// 	proportionF_safesite = 0.3;
		// 	modifyfood(1);
		// cost_move = 5;
		// sd_arrival_fuel = 0.4;


		// }
		// if(modelttype[mt]=="deltaf/")
		// {
		// 	// Food to 0.36
		// 	proportionF_safesite = 0.44;//36;
		// 	modifyfood(1);
		// 	// Food beta
		// 	food_betas[0] = 1.0/90.0;
		// }
		// cout<<food_betas[0] << endl;
		double blpred = flywayPredation;
		double plbreed = breeding_falc;
		// double blpredX = flywayPredationX;
		// double blpredDep = flywayPredationDep;
		for (int run =0;run<=2;run++)
		{ 
			if(run==1) 
			{
				flywayPredation = flywayPredation*.25;//modifypredation(0.5);
				// breeding_falc *= 0.25;
				// departureBreedingpop *= 0.25;
				// flywayPredationX = flywayPredationX* 0.5;//modifypredation(0.5);
				// flywayPredationDep = flywayPredationDep* 0.5;//modifypredation(0.5);
			}
			if(run==2)
			{
				flywayPredation = flywayPredation*1.5;//modifypredation(1.5);	
				// breeding_falc *=1.5;
				// departureBreedingpop *= 1.5;
				// flywayPredationX = flywayPredationX* 1.5;//modifypredation(1.5);	
				// flywayPredationDep = flywayPredationDep* 1.5;//modifypredation(1.5);	
			} 
			// cout << fuel_modifier << endl;
			string outdir = basedir + knowledgetypes[1] + modelttype[mt] + runs[run];
			string outdirpk = basedir + knowledgetypes[0] + modelttype[mt] + runs[run];
			runmod("full_back_single.txt", "forwards_sim.txt",outdir, outdirpk);
			flywayPredation = blpred;
			breeding_falc = plbreed;
			// flywayPredationX = blpredX;
			// flywayPredationDep = blpredDep;
		}
	// }
	return 0;
}