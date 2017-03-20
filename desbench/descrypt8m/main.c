#include <stdio.h>
#include <inttypes.h>

void des_encrypt( uint64_t* ptr, uint64_t* limit );
char buffer[1024*1024];

int main(int argc, char** argv)
{
  int i;
  for (i = 0 ; i < 80 ; i++ )
      des_encrypt( (uint64_t*)buffer, (uint64_t*)(buffer+sizeof(buffer)) );
  return 0;
}

