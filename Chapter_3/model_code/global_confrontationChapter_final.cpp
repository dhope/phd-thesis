#include "global.h"

// This is the definition of your variable.  It can only happen in one place.
// You must include global.h so that the compiler matches it to the correct
// one, and doesn't implicitly convert it to static.

const int number_of_sites = 2; // Number of stopover sites
const int final_time = 150; // Terminal time for model
const int max_fuel_load = 1000; // maximum potential fuel load
const double max_true_fuel_load = 1.0; // maximum fuel load in relative fuel units
const double wetleanbodymass = 22.7; //wet lean body mass
const float cost_move = 0.39; // cost of moving in grams

// Mortality function variables
// Seasonal pattern in predator presence
const double pred_max = -3.54485317;//-3.611108; // Shape variable 1 of 2 //-6.07518; // 	
const double pred_time_shape = 0.06623284; // Shape variable 2 of 2 //= 0.06659;//
// Shape coefficients for escape probability by fuel load
// const double a_escape = 0.9; // Max probability of escaping an attack
// const double b_escape = 1.3333333333; //3.33 initial// Shape variable for escape by fuel load
// const double fuel_modifier = 1.56;//exp(.56)/flywayPredation;//2.5;//1.7/0.0008;//1.7;
// const double mu_x_importance = 10; 
const double a = 3;
const double a_mod = 0.48;//0.18;
const double b=0;
// const double site_specific_constant_attack_rate;



// Terminal fitness values
const double min_fuel_load = 0.01; // minimum fuel load required to have a chance of completing migration
// const double max_p_fuel_departure = .1;//.15;//.005;//0.9; // Max prob of surviving migration
// const double shape_fuel_departure = 24;//31/3;// 5.3333333; // Shape coefficients for fitness at departure by fuel load
const double dep_benefit = 0.;//0.958;//0.01;//

// Global Food and danger variables constant for now
double flywayPredation = 1;//1.2E-3;//0.08;//1.; // Global predation modifier
double flywayFood = .5;//1.47; // Global food modifier (grams /day)
double optimaldistance = 0;

// Predation timing variables
double attackrate = 2; 		 				// Number of attacks per falcon per day
const int f_arrival_mod = 0; 				// Arrival modifier !!+ Is Early!!
double maxSeasonal = 2;  	 				// Maximum number of falcons in a season per day
const float breeding_falc = .05; 			// Minimum seasonal falcons per day 
const double departureBreedingpop = 0.05; 	// Minimum falcons per day on departure

// Fuel load effects of capture risk for local (x) and departed (Dep)
const double flywayPredationX = .002;//0.1/4; 
const double flywayPredationDep =  flywayPredationX*.15;//.08;//.7E-4;//2.25E-05;//
const double wintering_mortality =flywayPredationX*.01;
// const double lengthofmigration=25;
const bool CanJumpAhead = true;
const bool ComplexDeparture = false;

// Site Specific variables for food and danger
double Danger_small_site = 1/0.24; //0.12;// Relative danger at safe siteS
// double proportionD_midsite = 0.75; // Relative danger at safe siteS2
double Food_multiplier_small_site = 4.16/2.04;//1/.3; //0.5132785;//Relative food at safe site 0.25;//
// double proportionF_midsite = 0.75; // Relative food at safe site
double siteDanger[number_of_sites] = {Danger_small_site,1};
double siteFuelIntake[number_of_sites] = {Food_multiplier_small_site, 1}; //{1.47,0.9,999}; // maximum fuel loaded per day //flywayFood*proportionF_midsite,flywayFood, flywayFood* 0.75, flywayFood* 0.79};//, 

// --- Add speed of migration to model

double distance_to_migrate = 5000;
const double flight_par = 14000;
const double settle_time = 0.5;
double fuel_loading_rate = flywayFood*(Food_multiplier_small_site/2+1/2)/wetleanbodymass;
const double falcon_speed_of_migration = 172;



// const float wetleanmass_forward = {};
const float sd_food = 0.06;//0.02 // Increased from .2 related to sd in ydenberg 2002
const float sd_danger = 1.E-4;
const float sd_arrival[2] = {6,6};
const float sd_arrival_fuel = 3;//0.45;//0.4; //
// const float mf_diff_arrival = {1.22, 1.14};
const double fuel_arrival[2] = {1.0, 1.0};
const unsigned int arrival_dates[2] = {24, 53};
bool first_move_free = true;
int number_birds[2] = {10000, 10000};
float prop_dist_a[6] = {0.1,0.1,0.3,0.2,0.3,0.05};
float prop_dist_j[6] = {0.25,0.15,0.1,0.1,0.25,0.15};
const unsigned int distances[6] = {1300,2300,3500,5000,7000,9000};

const double food_betas[2] = {0,0};

// With foraging intensity - daily cost of maintanence in grams
const double dailyfuelcost = 0.;


//~ std::random_device rd; // Use this to make completely random
//~ std::mt19937 e2(rd());

int seed = 65491; // set the initial seed for random generator
std::mt19937_64 e2(seed);
std::normal_distribution<double> food_dist(0.0,sd_food);
std::normal_distribution<double> dang_dist(0.0,sd_danger);
// std::normal_distribution<double> date_dist(0.0,sd_arrival);
// std::poisson_distribution<int> date_a_dist(arrival_dates[0]);
// std::poisson_distribution<int> date_j_dist(arrival_dates[1]);
// std::negative_binomial_distribution<int> date_a_dist(60,0.7);
// std::negative_binomial_distribution<int> date_j_dist(56,0.5);
std::normal_distribution<float> date_a_dist(arrival_dates[0],sd_arrival[0]);
std::normal_distribution<float> date_j_dist(arrival_dates[1],sd_arrival[1]);

std::normal_distribution<double> fuel_arrival_dist(0.0, sd_arrival_fuel);
std::uniform_int_distribution<int> site_choice(0, number_of_sites - 1);

// Model controls
bool forwardsOutput = true;
int siteCount[(number_of_sites+1)*(final_time+1)]={0};
bool outputExtra = false;
int nruns = 1000;
bool outputAll = true;
int outputTimes[2] = {25, 56}; //Output Dates



bool useRandomDraw = true;
double sd_food_sites[number_of_sites] = {0.,0.};//{1.15,0.701};//{0.1,0.1,0.1};//
double sd_danger_sites[number_of_sites] = {1.569587,0.0498};//{1.60,0.46};//{0.7210252, 1.5861472, 0.6296655};//
double data_danger[number_of_sites] ={6.707637,1.};// {5.44, 1.59};//{1.530009,7.859727,14.194961};//{Danger_small_site,1};//
double data_food[number_of_sites] = {Food_multiplier_small_site*.24*6.707637,1.};//{1,2,4};//

double prop_pk[2] = {1, 1};

//  --- Unused variables
double delta_shape = 1;
// const double baselineflywayPredation= 1E-4;
const double f = 1;
// const double future_survival = 0.9;

std::vector<double>date_d;
std::vector<double>totalBirds;
std::vector<double>pLarge;
int n_skippers[2] = {0,0};
// double baselineFood[2] = {0.59, 0.48};

int senRun = 43;