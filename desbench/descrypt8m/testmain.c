#include <stdio.h>

unsigned long long des_encrypt( unsigned long long );

int main(int argc, char** argv)
{
  int i;
  printf( "%llx\n", des_encrypt( 0x123456789abcdefLLU ) );
  return 0;
}
