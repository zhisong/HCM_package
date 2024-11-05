TITLE = mishka.exe
POSITION = ../
OBJS = mishka2flow.o ppplib.o
LIBS = -lblas -llapack
OPTS = -fdefault-real-8 -fallow-argument-mismatch
FC = gfortran

all : $(OBJS)
	$(FC) -o $(POSITION)$(TITLE) $(OBJS) $(LIBS) $(OPTS);


mishka2flow.o : pre
	./pre.exe <mishka2flow.source.f> mishka2flow.f;
	$(FC) -c -o mishka2flow.o mishka2flow.f $(OPTS);

ppplib.o : ppplib.f
	$(FC) -c -o ppplib.o ppplib.f $(OPTS);

pre :
	$(FC) -o pre.exe pre.f $(OPTS);

clean :
	rm *.o $(POSITION)$(TITLE) pre.exe;
