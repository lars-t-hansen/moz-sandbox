#include <stdio.h>

unsigned long long des_encrypt( unsigned long long );

int main(int argc, char** argv)
{
  int i;
  unsigned long long sum = 0;
  for (i = 0 ; i < 10000000 ; i++ )
    sum += des_encrypt( 0x123456789abcdefULL );
  printf("%llu\n", sum);
  return 0;
}

