#ifndef BACK_H    // To make sure you don't declare the function more than once by including the header multiple times.
#define BACK_H


#include <stdio.h>
#include <cstring>
#include <iostream>
#include <stdlib.h>
#include <time.h>
#include <math.h>
#include <typeinfo>
#include <random>
#include "global.h"


// ****************************  Backwards Functions  *****************************

long addr(int fuel, int time, int site);					//	Address [x,t,s]
double bound(double x, double a, double b);					// bound data function (integers)
double ConvertToRealFuel(int fuel_in); 						// Convert computer integer to real fuel load
double ConvertToFuelStep(double fuel_in);					// Convert back to computer int
double FeedingAmount(int site,int time, bool move,bool returntrue);           // Calculate feeding amount
void TerminalFitness(double* fitness); //At time T, individuals calculate fitness based on  fuel load at departure
// double ProbabilitySuriveOnwardsMigration(int fuel, int time); // Caculate probability of surviving onwards


double FitnessEarlyDeparture(int time, int fuel);
double SpeedOfMigration(double fuel, bool rangeonly = false);
// double altFitnessEarlyDeparture(int time, double fuel);
void CalculateFitness( double* fitness,int* decisions,int time) ;


double interpolate_fitness(double next_fuel_load, int new_site, int next_day, double fitness[]);// Note time called here should be next day


//# The value of departing based on the probability of surviving to time T
//# from the current time and then surviving onwards migration.
double CalculateValues(int fuel, int time, int site, int newSite, double* fitness);
double mu(int fuel, int time, int site);
double mu_x(int fuel, int site);
double mu_t(double time);
void OutputBackwards(double* fitness, int* decisions, int time,char const backfile_[] ="full_back_single.txt");    			// Output fitness and decisions
void fullback(double* fitness,int* decisions, char const backfile_[] = "full_back_single.txt", bool plotout = true);
double optimizedistance(int time, int fuel);
#endif

