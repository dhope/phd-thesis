#ifndef FORW_H    // To make sure you don't declare the function more than once by including the header multiple times.
#define FORW_H


#include <stdio.h>
#include <iostream>
#include <stdlib.h>
#include <time.h>
#include <math.h>
#include <typeinfo>
#include <random>
#include "global.h"
#include "backfun.h"

// ****************************  Forwards Functions  *****************************

class WESA
{
public:
	// Attributes
	unsigned short int id;
	unsigned short int age;
	// unsigned short int arrival_time;
	unsigned short int current_time;
	unsigned short int fuel;
	double true_fuel;
	unsigned short int site;
	bool dead;
	bool departed;
	// unsigned short int current_decision;
	// unsigned double current_fitness;
	unsigned short int LengthofStay;

	// Memeber functions:
	void die();
	void movefeed(double* fitness, int* decisions, bool move = false);
	void depart(double fitness[]);
	void OutputForwards(double* fitness, int* decisions,char const forFile[] = "forwards_sim.txt" );
	void migrate(double* fitness, int* decisions, char const forFile[]);
	void BirdArrive(int id_, int age_, double* fitness, int* decisions, char const forFile[]);
	};
	
void fullForward(double* fitness,int* decisions,char const forFile[] = "forwards_sim.txt", bool plotout = true);

#endif
