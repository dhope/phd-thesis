#include <stdio.h>
#include <iostream>
#include <stdlib.h>
#include <time.h>
#include <math.h>
#include <typeinfo>
#include <random>
#include "global.h"
#include "backfun.h"
#include "forfun.h"
#include <algorithm>


// ****************************  Forwards Functions  *****************************
using namespace std;
//~ std::mt19937 e2(rd());
//~ std::normal_distribution<double> food_dist(0.0,sd_food);
//~ std::normal_distribution<double> dang_dist(1.0,sd_danger);
//~ std::normal_distribution<double> date_dist(0.0,sd_arrival);
//~ std::normal_distribution<double> fuel_arrival_dist(0.0, sd_arrival_fuel);
//~ std::uniform_int_distribution<int> site_choice(0, number_of_sites - 1);

void WESA::die()
	{
		dead = true;
//		fitness = 0
		LengthofStay = 0;
		// std::cout << "Oh dear I died" << std::endl;
	}
void WESA::movefeed(double fitness[], int decisions[], bool move)
	{
		if(move)
		{
            // std::cout << site << std::endl;
            // printf("Dec: %d\n", *(decisions + addr(fuel, current_time,  site) ) );
			site = *(decisions + addr(fuel, current_time,  site));
			// if(*(fitness + addr(fuel, current_time, site))==0) die();
			if(site > number_of_sites || site < 0)
				printf("SHeeit why is %d bird here: %d\t%d\t%d\n", id, site, fuel, current_time);
			// std::cout << site << std::endl;;
			LengthofStay = 0;
		}
		true_fuel += (FeedingAmount(site,current_time, move, true) + food_dist(e2)/wetleanbodymass);
		true_fuel = bound(true_fuel, 0, max_true_fuel_load);
		fuel = (int)bound(round(ConvertToFuelStep(true_fuel)), 0, max_fuel_load);
		double p_death = 1-mu(fuel,  current_time,site);
		if(rand() < p_death+dang_dist(e2)) {die();} else
		{
			// current_decision = *(decision + addr(fuel, current_time,  site));
			// current_fitness = *(fitness + addr(fuel, current_time,  site));
			// current_time += 1;
			LengthofStay +=1;
		}
	}

void WESA::depart(double fitness[])
	{
		site = number_of_sites;
		// current_time +=1;
//		fitness = *(fitness + addr(fuel, current_time,  site));
		departed = true;

	}


void WESA::OutputForwards(double fitness[], int decisions[],char const forFile[])
	{
		FILE *file2;
		if ((file2 = fopen(forFile, "at")) == NULL)
		{
   		printf("%s\n", "Error opening file forwards_sim.txt");
      		exit(1);
   		}
      // printf( "%5d\t%3d\t%3d\t%3d\t%3.5f\t%3d\t%3d\t%5d\t%3.5f\n",id, age, current_time,fuel,
      //   true_fuel,site,LengthofStay,  *(decisions + addr(fuel,current_time, site)),
      //     *(fitness + addr(fuel,current_time, site)));
   		fprintf(file2, "%5d\t%3d\t%3d\t%3d\t%3.5f\t%3d\t%3d\t%5d\t%3.5f\n",id, age, current_time,fuel,
        true_fuel,site,LengthofStay,  *(decisions + addr(fuel,current_time, site)),
      		*(fitness + addr(fuel,current_time, site)));
        fclose(file2);
	}

void WESA::migrate(double fitness[], int decisions[],char const forFile[] )
	{
		// cout << siteCount[1][15] << endl;
		while (!dead && !departed && current_time <= final_time)
		{	if(*(fitness + addr(fuel, current_time, site))==0) die();
			else if(current_time == final_time && site != number_of_sites)
				die();
			else if (fuel <= 0)
				die();
			else if (*(decisions + addr(fuel, current_time,  site)) == site)
				movefeed(fitness, decisions, false);
			else if (*(decisions + addr(fuel, current_time,  site)) == number_of_sites)
				depart(fitness);
			else
				movefeed(fitness, decisions, true);
			if(forwardsOutput) OutputForwards(fitness, decisions, forFile);
			else{
			if(!dead && !departed && current_time <= final_time){
					// cout << site << "\t" << current_time << "\t"<<fuel<< endl;//siteCount[site][current_time] << endl; 
					int i = ((site*(final_time+1)) + current_time);
					
			        (siteCount[i])++;

			        // cout << siteCount[s][t_] << endl; 
			         }
			}
				
			current_time++;//= 1;
		}
	}

void WESA::BirdArrive(int id_, int age_, double fitness[], int decisions[], char const forFile[])
	{
		id = id_;
		age = age_;
    // printf("%d\n", arrival_dates[age]);
		if (age == 0){
			current_time = bound(int(date_a_dist(e2)),0,final_time-1);
		}else{
			current_time = bound(int(date_j_dist(e2)),0,final_time-1);
		}
		//std::max(0, std::min(final_time, (int)round(arrival_dates[age] + date_dist(e2))));
		true_fuel = bound((fuel_arrival[age] + fuel_arrival_dist(e2))/wetleanbodymass, 0.5/max_fuel_load*max_true_fuel_load, max_true_fuel_load);
//        printf("%d\t", current_time);
		fuel = (int)bound(round(ConvertToFuelStep(true_fuel)), 1, max_fuel_load);
		site = site_choice(e2);
		 // printf("%d, %d, %d\n",site , fuel , current_time );
		dead = false;
		departed = false;
		LengthofStay = 0;
		if(first_move_free)// && age == 0)
		{	//cout<<"FirstyFree"<< endl;
			double V[number_of_sites];
			const int N = sizeof(V)/sizeof(double);
			for (int s=0; s<=number_of_sites; s++)
			{
				V[s] = *(fitness + addr(fuel, current_time, s));
				// cout<< V[s]<< endl;
			}
			// int tester = *(decisions + addr(fuel, current_time,  site));
			int ts = std::distance(V, std::max_element(V, V+N));
			if(*(fitness + addr(fuel, current_time, ts))==0) die();
			site = *(decisions + addr(fuel, current_time,  ts));
			
			// if(site != tester){
			// 	printf("Original: %d-%f; New: %d-%f\tDelta -%.15e\nCondition: %d\t%d\n",
			// 		tester,V[tester], site, V[site], 
			// 		V[site]-V[tester], fuel, current_time);
			// }
//            printf("%d, %d, %d\n",site , fuel , current_time );
			// site = *(decisions + addr(fuel, current_time,  site));
			if(site==number_of_sites)
			 	{
			 		departed = true;
			 		n_skippers[age_] = n_skippers[age_]+1;
			 	}
		} //else{cout << "no, I'm stuck here" << endl;}
		if(forwardsOutput)
			{
				OutputForwards(fitness, decisions, forFile);
			}else{
			if(!dead && !departed && current_time <= final_time){
					// cout << site << "\t" << current_time << "\t"<<fuel<< endl;//siteCount[site][current_time] << endl; 
					int i = ((site*(final_time+1)) + current_time);
					
			        (siteCount[i])++;
			        // cout << siteCount[s][t_] << endl; 
			         }
			}
		current_time ++;
		 // printf("%d, %d, %d\n",site , fuel , current_time );
		// std::cout << "created a new bird: " << id << std::endl;
	}
	
	
void fullForward(double fitness[],int decisions[],char const forFile[], bool plotout)
{
	using namespace std;
	if(plotout)
    {
	
		FILE *file2;                     //Clear output file
		if ((file2 = fopen(forFile, "wt")) == NULL)
		{
		printf("Error opening file %s\n", forFile);
		exit(1);
		}
		//~ //   fprintf(file2, "");
		fprintf(file2,"id\tAge\tt\tintFuel\tfuel\ts\tLoS\td(s,t,x)\tF(s,t,x)\n");
		fclose(file2);
	}
	WESA birds[(number_birds[0] + number_birds[1])];
	// printf("%d\n", sizeof(birds)/ sizeof(*birds));
	int id = 0;
	for(int a_j =0; a_j <=1; a_j++){
	for (int bird =0; bird<number_birds[a_j]; bird++){
		birds[id].BirdArrive(id, a_j, fitness, decisions, forFile);
		if(!birds[id].dead && !birds[id].departed)birds[id].migrate(fitness, decisions, forFile);
		id ++;


					}}
					
}

