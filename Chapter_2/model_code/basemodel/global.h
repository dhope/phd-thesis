#ifndef MY_GLOBALS_H
#define MY_GLOBALS_H

// https://stackoverflow.com/questions/12290451/access-extern-variable-in-c-from-another-file
// This is a declaration of your variable, which tells the linker this value
// is found elsewhere.  Anyone who wishes to use it must include global.h,
// either directly or indirectly.

#include <stdio.h>
#include <string>
#include <iostream>
#include <stdlib.h>
#include <time.h>
#include <math.h>
#include <typeinfo>
#include <random>
#include <chrono>


// ******************************************
// Constant Variables

extern const int number_of_sites; // Number of stopover sites
extern const int final_time; // Terminal time for model
extern const int max_fuel_load; // maximum potential fuel load
extern const double max_true_fuel_load; // maximum fuel load in relative fuel units
extern const double wetleanbodymass; //wet lean body mass
extern const float cost_move; // cost of moving in grams

// Mortality function variables
// Seasonal pattern in predator presence
extern const double pred_max; // Shape variable 1 of 2
extern const double pred_time_shape; // Shape variable 2 of 2
// Shape coefficients for escape probability by fuel load
// extern const double a_escape; // Max probability of escaping an attack
// extern const double b_escape; // Shape variable for escape by fuel load
extern const double fuel_modifier;
extern const double mu_x_importance ;
extern const double a;
extern const double a_mod;
extern const double b;

// Terminal fitness values
extern const double min_fuel_load; // minimum fuel load required to have a chance of completing migration
extern const double max_p_fuel_departure;//0.9; // Max prob of surviving migration
extern const double shape_fuel_departure; // Shape coefficients for fitness at departure by fuel load
extern const double dep_benefit; 
extern const double future_survival;

// Food and danger variables constant for now
extern double flywayPredation; // Global predation modifier
extern double maxSeasonal; // Global predation modifier
extern const double flywayPredationX;
extern const double flywayPredationDep;
extern const double wintering_mortality;
extern const bool CanJumpAhead;
extern const bool ComplexDeparture;
// extern const double lengthofmigration;
extern double attackrate;
extern double delta_shape;
extern const double baselineflywayPredation;
extern double flywayFood;
extern double Danger_small_site; // Relative danger at small siteS
// extern double proportionD_midsite; // Relative danger at safe siteS
extern double Food_multiplier_small_site; // Relative food at large site
// extern double proportionF_midsite; // Relative food at safe site
extern double siteDanger[]; //  relative danger of site
extern double siteFuelIntake[]; // maximum fuel loaded per day
extern const double f;
extern const double siteVar;

extern const double dailyfuelcost; // cost in grams of not foraging

extern const float sd_food;
extern const float sd_danger;
extern const float sd_arrival[];
extern const float sd_arrival_fuel;
extern const double fuel_arrival[] ;
extern const unsigned int arrival_dates[];
extern bool first_move_free;
extern int number_birds[];
extern float prop_dist_a[];
extern float prop_dist_j[];
extern const unsigned int distances[];


extern const double food_betas[];

// Random variables
extern std::random_device rd;
extern int seed;
extern std::mt19937_64 e2;
extern std::normal_distribution<double> food_dist;
extern std::normal_distribution<double> dang_dist;
// extern std::normal_distribution<double> date_dist;
// extern std::poisson_distribution<int> date_dist;
// extern std::poisson_distribution<int> date_a_dist;
// extern std::poisson_distribution<int> date_j_dist;
extern std::normal_distribution<float> date_a_dist;
extern std::normal_distribution<float> date_j_dist;
// extern std::negative_binomial_distribution<int> date_a_dist;
// extern std::negative_binomial_distribution<int> date_j_dist;

extern std::normal_distribution<double> fuel_arrival_dist;
extern std::uniform_int_distribution<int> site_choice;

// Model controls
extern bool forwardsOutput;
extern int siteCount[];
extern bool outputExtra; 
extern int nruns;
extern bool outputAll;
extern int outputTimes[];
extern int senRun;


// SESA controls
extern const int f_arrival_mod;
extern const float breeding_falc;
extern const double departureBreedingpop;


extern bool useRandomDraw;
extern double sd_food_sites[];
extern double sd_danger_sites[];
extern double data_danger[];
extern double data_food[];
extern bool returnValues;
extern double prop_pk[];
extern double optimaldistance;


extern double distance_to_migrate ;
extern const double flight_par;
extern const double settle_time;
extern double fuel_loading_rate;
extern const double falcon_speed_of_migration;

extern std::vector<double>date_d;
extern std::vector<double>totalBirds;
extern std::vector<double>pLarge;
extern int n_skippers[];
// extern double baselineFood[];

#endif
