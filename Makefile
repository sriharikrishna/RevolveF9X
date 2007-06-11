ifndef F90C
F90C=gfortran
endif

ifndef F90CFLAGS
F90CFLAGS=-O3
endif

example : revolve.o example.f90
	$(F90C) $(F90CFLAGS) -o $@ $^

%.o: %.f90
	$(F90C) $(F90CFLAGS) -c -o $@ $<

clean: 
	rm -f *.o *.mod example

