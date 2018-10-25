#include <stdio.h>
#include <iostream>
#include <cstring>
#include <stdlib.h>
#include <time.h>
#include <math.h>
#include <typeinfo>
#include <random>
#include <algorithm>
#include "global.h"
#include "backfun.h"




// ****************************  Backwards Functions  *****************************

long addr(int fuel, int time, int site)                 //  Address [x,t,s]
{
return ( site*(final_time+1)*(max_fuel_load+1) + time*(max_fuel_load+1) + fuel);
//site*(final_time+1)*(max_fuel_load+1) + time*(final_time+1) + fuel);        //fuel + (final_time+1) * (time + (max_fuel_load+1)*site));
//              //      e.g., f[x,t] is *(f+addr(x,t))
}

double bound(double x, double a, double b)                  // bound data function (integers)
{
        if (a <= x && x <= b) return x;
    else if (x < a) return a;
        else return b;
}

double ConvertToRealFuel(int fuel_in){
        return( double(max_true_fuel_load)/double(max_fuel_load)) * double(fuel_in) ;
        }
double ConvertToFuelStep(double fuel_in){
        return(max_fuel_load/ (max_true_fuel_load) * fuel_in) ;
    }

double FeedingAmount(int site,int time, bool move =false, bool returntrue = false){
    // std::cout << site<<"\t"<<move << std::endl;
    // double beta = food_betas[site];
    double potential_fuel = (flywayFood * siteFuelIntake[site] + time * food_betas[site]) / wetleanbodymass;
     // std::cout << potential_fuel << std::endl;
    // std::cout << site<<"\t"<<move << std::endl;
    double moveCost = cost_move / wetleanbodymass;
    double net_intake = potential_fuel;
    if(move){net_intake -= moveCost;}
    
   // printf("ComputerAmount: %f - %f \n", computerFeedingAmount, net_intake);
   if(returntrue) {return(net_intake);}
   else 
    {
        float computerFeedingAmount = ConvertToFuelStep(net_intake);
        return(computerFeedingAmount);
    }
}



void TerminalFitness(double fitness[]) //At time T, individuals calculate fitness based on  fuel load at departure
{
    // int fuel, site;
    for (int fuel=0; fuel<=max_fuel_load; fuel++){ // Loop through fuel and site and assign fitness at time T.
        for (int site=0; site<number_of_sites; site++){
            //printf("Array is: %lu; Address is: %ld\n", 
            //sizeof((fitness)),addr(fuel,final_time, site)/sizeof(*(fitness)),addr(fuel,final_time, site)) ;
            *(fitness + addr(fuel,final_time, site)) = 0.0; }
    *(fitness + addr(fuel,final_time, number_of_sites)) = 0;//ProbabilitySuriveOnwardsMigration(fuel, final_time);
    // Calculate the fitness for birds that have departed. 

    //printf("Fitness is : %f\n", *(fitness + addr(fuel,final_time, number_of_sites)));
    }
}



// double ProbabilitySuriveOnwardsMigration(int fuel, int t){
//     /*Probability of surviving the rest of migration,
//     related to relative fuel load.*/
//     // double depmod = 1;
//     double true_fat = ConvertToRealFuel(fuel);
//     if(true_fat<min_fuel_load) return(0.0);
//     // if(true_fat>0.3)return(0.0);
//     // if(t +f_arrival_mod >= -pred_max/pred_time_shape) depmod = 0.999;
//     double sx =1+ max_p_fuel_departure*(1.-exp(-shape_fuel_departure * (true_fat - min_fuel_load)));
//     return(0);//sx*depmod);
// }



double FitnessEarlyDeparture(int time, int fuel){
    // std::cout << distance_to_migrate << std::endl;
    double true_fat = ConvertToRealFuel(fuel);

    if(true_fat<min_fuel_load)return(0.0);
    // true_fat -= min_fuel_load;
    double mux_; double flightrange;
    double speed = SpeedOfMigration(true_fat);
    flightrange = SpeedOfMigration(true_fat, true);
    long double probSurv =flywayPredationDep;//*mu_x(fuel, 1);
    double lengthofmigration = (distance_to_migrate) / speed; //-flightrange
    double headstart = 0;
    if(CanJumpAhead || ComplexDeparture) 
    {   double flighttime =flightrange/1340;
        // lengthofmigration = (distance_to_migrate) / speed; //-flightrange
        headstart = flighttime-flightrange/falcon_speed_of_migration;
        lengthofmigration = bound(distance_to_migrate-flightrange,0,20000) / speed + flighttime;
        // lengthofmigration = (distance_to_migrate-flighttime) / speed;
        // flightrange / (215 - speed);
        //        bound(flightrange/speed-flightrange/215 ,-5,5);
        // mux_ = altFitnessEarlyDeparture(time,fuel);
        // if(true_fat<0.2) std::cout << headstart << "\t" << SpeedOfMigration(true_fat, true) << "\t" << 1/speed << std::endl;

    }
    if(final_time-time-lengthofmigration<0)return(0.);  
    if (ComplexDeparture)
    {
        double deltaZ = (falcon_speed_of_migration-speed)/falcon_speed_of_migration;//1/(speed*(falcon_speed_of_migration-speed));
        // std::cout << deltaZ << std::endl;
         // double time_to_next_location = flightrange/1340; 

        double migratorysurvival = 1;
        double intital_headstart = headstart;
        for (int i =0; i<= lengthofmigration; i++)
        {   
            if(i==0) headstart = intital_headstart;
            else headstart = intital_headstart+deltaZ * i;
            mux_ = mu_t(time + headstart);
            double numberattacks = attackrate * (((maxSeasonal-distance_to_migrate*2/10000)-departureBreedingpop )* \
                                    mux_ + \
                                    departureBreedingpop) ;
            migratorysurvival *= pow(1-probSurv, numberattacks);
        }
        bool iteratewinter = true;
        if(iteratewinter)
        {
            double winter_survival = 1;
            for(int i = time+lengthofmigration; i <= final_time-time-lengthofmigration; i++)
            {
                mux_ = mu_t(time + headstart);
                double numberattacks = (attackrate * (maxSeasonal-distance_to_migrate*2/10000)-0 )* \
                                        mux_ + \
                                        0 ;
                winter_survival *= pow(1-wintering_mortality, numberattacks);
                headstart++;
            }
            return(migratorysurvival*winter_survival);
        }
        
        return(bound( 1*migratorysurvival*exp(-wintering_mortality*(final_time-time-lengthofmigration)) ,0,1));
    }


    mux_ = mu_t(time + headstart);
    double mut_ = attackrate * ((maxSeasonal-departureBreedingpop )* \
    mux_ + \
    departureBreedingpop);
    
    
    // if(final_time-time-lengthofmigration<0)return(0.);
    long double totalsurvival = exp(-probSurv*mut_*flywayPredation*(1-dep_benefit)*(lengthofmigration) );
    // long double totalsurvival = pow((1-probSurv),mut_*flywayPredation*(1-dep_benefit)*(lengthofmigration) );
    // long double ps=  ProbabilitySuriveOnwardsMigration(fuel, time);
    // std::cout<<lengthofmigration << "\t";
    return(bound(1*totalsurvival* exp(-wintering_mortality*(final_time-time-lengthofmigration)) ,0,1)); //(-wintering_mortality/5000*(distance_to_migrate-5000)+wintering_mortality)
    // 1*totalsurvival*pow(1-wintering_mortality, final_time-time-lengthofmigration),0,1));
}


double optimizedistance(int time, int fuel){
    double distances[5] = {1000,2500,5000,7000,9000};
    double V[5];
    const int N = sizeof(V) / sizeof(double);
    for (int i = 0; i <5; i++){
        distance_to_migrate = distances[i];
        V[i] = FitnessEarlyDeparture(time, fuel);
    }

     int nopt = std::distance(V, std::max_element(V, V + N));
     optimaldistance = distances[nopt];
     // std::cout << distances[nopt] << std::endl;
     return(V[nopt]);
}

// double altFitnessEarlyDeparture(int time, double fuel)
// {
//     double flightrange = SpeedOfMigration(fuel, true);
//     double time_to_next_location = flightrange/1340; 
//     double time_to_next_with_fuel = time_to_next_location + (settle_time + fuel/fuel_loading_rate);
//     // std::cout << time_to_next_with_fuel << "\t" <<flightrange/215 << std::endl;
//     double mut_ = mu_t(time + time_to_next_with_fuel-flightrange/215);
//     return(mut_);
// }

double SpeedOfMigration(double fuel, bool rangeonly)
{
    double range = flight_par * (1-1/sqrt(1 + fuel));
    // double range = flight_par/2 * log(1 + fuel);
    if(rangeonly)return(range);
    else return(range/(settle_time + fuel/fuel_loading_rate) );
}



void CalculateFitness( double fitness[],int decisions[],int time)
{
    // int s; int x; //int newsite;
    // Set Fitness to zero for birds with zero fuel
    for(int s=0;s<number_of_sites;s++)
    {
        *(fitness + addr(0,time, s)) = 0.0;
        *(decisions + addr(0,time,s)) = -9;
    }
    // s=0;
    // x=1;
    // Loop through fuel
    for (int x=1; x<=max_fuel_load; x++)
    { //s=0;    // Loop through sites
        *(fitness + addr(x, time, number_of_sites)) = FitnessEarlyDeparture(time, x);//optimizedistance
        // *(fitness + addr(x, time, number_of_sites)) = optimizedistance(time, x);//
        *(decisions + addr(x, time,  number_of_sites)) = optimaldistance;//number_of_sites;
        for(int s=0;s<number_of_sites;s++)
        {
            double V[number_of_sites+1];
            const int N = sizeof(V) / sizeof(double);
                for (int newsite = number_of_sites; newsite>=0; newsite--)
                {
                    V[newsite] = CalculateValues(x, time, s,newsite, fitness);
                
                }
            int nopt = std::distance(V, std::max_element(V, V + N));
            // if((time<90) && ((V[nopt]== V[0] && V[nopt] == V[1]) || (V[nopt]== V[0] && V[nopt] == V[2]) || (V[nopt]== V[1] && V[nopt] == V[2])) )
            // if(x>20 && x<30 && time ==24 && s == 1 )                
            // {
            //     printf("%.8f\t%.8f\t%.8f\t%.8f\t%d\t%d\t%d\t%d\n", V[nopt], V[nopt]-V[0], V[nopt]-V[1], V[nopt]-V[2], nopt, x,time,s );
            // }
            double maxrhs = V[nopt];
            *(fitness + addr(x, time, s)) = maxrhs;
            if(maxrhs==0) *(decisions + addr(x,time, s)) = -9;
            else *(decisions + addr(x,time, s)) = nopt;
            // if(nopt == number_of_sites) std::cout<< SpeedOfMigration(ConvertToRealFuel(x)) << std::endl;
            // int nx = 0;
            // for(int i = 0; i<=number_of_sites; i++ )
            // {
            //     if(V[i]== i) nx++;
            // } if (nx >1) printf("Ties here: %d\t%d\t%d\t%d\n",nx, s, x,time );
        }
        
    }

}      


double interpolate_fitness(double next_fuel_load, int new_site, int next_day, double fitness[]){// Note time called here should be next day
    //Interpolate the fitness for a given fuel
    double lower_fitness, upper_fitness, fit_est;
    double fuel_int = ConvertToFuelStep(next_fuel_load);
    int floor_fuel = bound((int)fuel_int,0,max_fuel_load);
    int ceil_fuel = bound(floor_fuel+1,0,max_fuel_load);
    double delta_fat = fuel_int - double(floor_fuel);
    lower_fitness = *(fitness + addr(floor_fuel, next_day, new_site));
    upper_fitness = *(fitness + addr(ceil_fuel, next_day, new_site));
    fit_est = (1-delta_fat) * lower_fitness + delta_fat * upper_fitness;
   // if(next_day == 28)
   // printf("Fitness Estimate - %d, %d, %f, %f, %f, %f\n",new_site, next_day, delta_fat, upper_fitness, fit_est, lower_fitness);
    return(fit_est);
}


//# The value of departing based on the probability of surviving to time T
//# from the current time and then surviving onwards migration.
double CalculateValues(int fuel, int time, int site, int newSite, double fitness[]){
     bool move; double Value;
    // for (newSite=0; newSite<=number_of_sites; newSite++){
        if (newSite == number_of_sites) {
            Value = *(fitness + addr(fuel, time, number_of_sites));//optimizedistance(time, fuel);
//            if(time ==89)
//            printf("Fit Depart: %f, %f, %d\n", FitnessEarlyDeparture(time, fuel), Values[newSite], newSite);
            }
        if (newSite != number_of_sites)
        { int movefat = 0.;
            if (newSite == site)
            {
                move = false;
            }
            if (newSite != site)
            {
                move = true;
                movefat = cost_move / wetleanbodymass;
            }
            double real_fuel = ConvertToRealFuel(fuel);
            double x_next = bound(real_fuel + FeedingAmount(newSite,time, move, true),0, max_true_fuel_load);
            double fitness_est = interpolate_fitness(x_next, newSite, (time+1), fitness);
            // *(fitness + addr((int)ConvertToFuelStep(x_next), time+1, newSite));
            Value = (mu(fuel-movefat, (time), newSite)) * fitness_est;
        }
        return(Value);
    }

// Fuel load modifier of vulnerability to predation
double mu_x(int fuel, int site) 

{  // std::cout << (double)ConvertToRealFuel(fuel) << std::endl;
    // double d_int[2]={1,4};
     // double mu_ = flywayPredationX*exp(a*siteDanger[site]*ConvertToRealFuel(fuel));
    double rf = ConvertToRealFuel(fuel);
    double deltaF = (siteFuelIntake[site] * flywayFood) / wetleanbodymass;
    double x_ = flywayPredationX*pow(siteDanger[site], flywayPredation);
    double shapemod = (a + 1) + a_mod*(1 - siteDanger[site] );
    double y1 = pow(rf + deltaF,shapemod) - pow(rf,shapemod);
    double y2 = deltaF * (shapemod);
    double mu_ = x_ * y1 / y2;
    return(bound(mu_,0,1));
}

double mu_t(double time)
{
    return(  delta_shape/ \
        ((1/f) + exp( -pred_max - pred_time_shape * \
            ( time + f_arrival_mod)  )\
         ) ) ;
}


double mu(int fuel,  int time,int site){
     double dailyattacks = attackrate*((maxSeasonal-breeding_falc) * mu_t((double)time) + breeding_falc) ;
     double mu_abs = exp(-mu_x(fuel, site)*flywayPredation*dailyattacks);
     // pow(1-mu_x(fuel, site),flywayPredation*dailyattacks)
    return(bound(mu_abs,0,1));
}


void OutputBackwards(double fitness[], int decisions[], int time, char const backfile_[])               // Output fitness and decisions
{
    int x,site;

    FILE *file1;
    if ((file1 = fopen(backfile_, "at")) == NULL)
    {
        printf("%s\n", "Error opening file full_back_single");
            exit(1);
     }

//  fprintf(file1, "Time period %3d\n", time);

//  for (i=1; i<=22; i++) fprintf(file1,"-"); fprintf(file1,"\n");
//      fprintf(file1," t x  s  dec(x,t)  F(x,t)\n");
//  for (i=1; i<=22; i++) fprintf(file1,"-"); fprintf(file1,"\n");
    for (x = 0; x <= max_fuel_load; x++){
        for (site=0; site<=number_of_sites; site++){
        fprintf(file1, "%3d\t%3d\t%3d\t%5d\t%3.9f\n",time,  x,site, *(decisions + addr(x,time, site)),
            *(fitness + addr(x,time, site)));
}}
//  for (i=1; i<=22; i++) fprintf(file1,"-"); fprintf(file1,"\n\n");
    fclose(file1);
}


//~ void fullback(char const backfile_[], bool plotout[])
void fullback(double fitness[],int decisions[], char const backfile_[], bool plotout)
{
    using namespace std;
    
    //~ string backfile = backfile_;
    if(plotout)
    {
        FILE *file1;                     //Clear output file
        if ((file1 = fopen(backfile_, "wt")) == NULL)
        {
          printf("Error opening the backwards programming file %s; called from fullback.\n", backfile_);
          exit(1);
        }
        fprintf(file1,"t\tfuel\ts\td(s,t,x)\tF(s,t,x)\n");
        fclose(file1);
    }
    // 1. Calculate terminal fitness
    TerminalFitness(fitness);
    if(plotout){
        OutputBackwards(fitness, decisions, final_time, backfile_);
    }   
    // 2. Step backwards from time max
    for (int time_ = final_time-1; time_>=0; time_--)
    {
        CalculateFitness(fitness,decisions,time_);
        if(plotout){
            OutputBackwards(fitness, decisions, time_, backfile_); 
            // cout<< time_<< "\t" <<*(decisions + addr(10, 10, 0)) << endl;
        }
    }
}
