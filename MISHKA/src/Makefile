TITLE = mishka.exe
POSITION = ../
OBJS = mishka1.o
MISLIB = $(POSITION)mishkalib.a
LIBS = -lblas $(MISLIB)
OPTS = -fdefault-real-8 -fallow-argument-mismatch
FC = gfortran

all : $(OBJS)
	$(FC) -o $(POSITION)$(TITLE) $(OBJS) $(LIBS) $(OPTS);


mishka1.o : pre
	./pre.exe <mishka1_info.source.f> mishka1.f;
	$(FC) -c -o mishka1.o mishka1.f;

pre :
	$(FC) -o pre.exe pre.f

clean :
	rm *.o $(POSITION)$(TITLE) pre.exe;
