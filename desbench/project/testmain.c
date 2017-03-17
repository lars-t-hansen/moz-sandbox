#include <stdio.h>

unsigned long des_encrypt( unsigned long );

int main(int argc, char** argv)
{
  int i;
  printf( "%lx\n", des_encrypt( 0x123456789abcdef ) );
  return 0;
}
