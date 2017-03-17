unsigned long des_encrypt( unsigned long );

main()
{
  int i;
  printf( "%lx\n", des_encrypt( 0x123456789abcdef ) );
  return 0;
}
