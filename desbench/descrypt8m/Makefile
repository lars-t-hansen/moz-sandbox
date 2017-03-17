r8:	main.o descrypt8m.o
	cc -o r8 main.o descrypt8m.o

t8:	testmain.o descrypt8m.o
	cc -o t8 testmain.o descrypt8m.o

r9:	main.o descrypt9m.o
	cc -o r9 main.o descrypt9m.o

t9:	testmain.o descrypt9m.o
	cc -o t9 testmain.o descrypt9m.o

clean:
	rm -f *~ *.o r8 t8 r9 t9 *.log *.aux *.dvi *.ps

main.o:	main.c Makefile
	cc -O3 -c main.c

testmain.o:	testmain.c Makefile
	cc -O3 -c testmain.c

descrypt8m.o:	descrypt8m.c Makefile
	cc -O3 -c descrypt8m.c

descrypt9m.o:	descrypt9m.c Makefile
	cc -O3 -c descrypt9m.c
