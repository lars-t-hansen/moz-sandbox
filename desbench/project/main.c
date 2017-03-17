unsigned long des_encrypt( unsigned long );

main()
{
  int i;
  for (i = 0 ; i < 1000000 ; i++ )
    des_encrypt( 0x123456789abcdef );
  return 0;
}

