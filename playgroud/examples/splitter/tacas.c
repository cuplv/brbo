extern void __VERIFIER_error() __attribute__((noreturn));
extern void __VERIFIER_assume (int);
extern int __VERIFIER_nondet_int ();
#define static_assert __VERIFIER_assert
#define assume __VERIFIER_assume
#define LARGE_INT 1000000
#define true 1
#define false 0
#define boolean int
#define MAX 8
void __VERIFIER_assert(int cond) {
  if (!(cond)) {
    ERROR: __VERIFIER_error();
  }
  return;
}
void assert(int cond) {
  if (!(cond)) {
    ERROR: __VERIFIER_error();
  }
  return;
}
int ndInt() {
  return __VERIFIER_nondet_int();
}
int ndBool() {
  int x = ndInt();
  assume(x == 1 || x == 0);
  return x;
}
int ndInt2(int lower, int upper) {
  int x = ndInt();
  assume(lower <= x && x <= upper);
  return x;
}

void foo(){
int x, y, w, z;
int arandom, brandom, crandom;
x=0;y=0;w=0;z=0;

while(ndBool()) {
       if(ndBool()){
               x=x+1;
               y=y+100;
	}
       else{
                if(ndBool()){
                       if(x>=4){
                               x=x+1;
                               y=y+1;
                       }
		}
               else{
                       if(y>10*w && z>=100*x){
                               y=-y;
                       }
               }
	}
       w=w+1;z=z+10;
}
if(x>=4)
  assert(y>2);
}
