#include <stdio.h>
#include <inttypes.h>

void des_encrypt( uint64_t* ptr, uint64_t* limit );

int main(int argc, char** argv)
{
  uint64_t data[] = { 0x123456789abcdefLLU };
  des_encrypt( data, data+1 );
  printf( "%llx\n", data[0] );
  return 0;
}
