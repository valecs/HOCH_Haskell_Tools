#include "potentialFFI.h"

extern void getpot_(const int * natom, const double xmass[36], double xx[3][36], double * v, double * v1, double * v2, double * s);

double getFormaldehydePotential(double position[12]){
  static const int natom = 4;
  static const double xmass[36]={
    1837.15   // H
    ,29156.95 // O
    ,21874.66 // C
    ,1837.15  // H 
  }; //initializes the first 4 compnents and leaves the others

  //double xx[3][36]; 
  static double v = 0, v1 = 0, v2 = 0, s = 0;
  static double xx[3][36]; //transpose of fortran's 36x3
  
  for (int coor = 0; coor < 3; coor++){
    for (int atom = 0; atom < 4; atom++){
      xx[coor][atom]=position[(atom*3)+coor];
    }
  }
  
  getpot_(&natom, xmass, xx, &v, &v1, &v2, &s);

  return v;
}
