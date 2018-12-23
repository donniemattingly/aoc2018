#include <stdio.h>

int main(void){
  int r0 = 1;
  int r1 = 0;
  int r2 = 65536;
  int r3 = 10736359;
  int r4 = 0; // ip
  int r5 = 0;

  r1 = r2 & 255;
  r3 = r1 + r3;
  r3 = r3 & 16777215;
  r3 = r3 * 65899;
  r3 = r3 & 16777215;
  
}
