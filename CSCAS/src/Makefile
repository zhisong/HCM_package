TITLE = cscas.exe
POSITION = ../
OBJS = cscas.o ppplib.o
LIBS = -lblas -llapack
FC = gfortran
OPTS = -fdefault-real-8 -fallow-argument-mismatch

all: $(OBJS)
	$(FC) -o $(POSITION)$(TITLE) $(OBJS) $(LIBS) $(OPTS); 

pre: pre.f
	$(FC) -o pre.exe pre.f;

ppplib.o: ppplib.f
	$(FC) -c -o ppplib.o ppplib.f $(OPTS);

cscas.o: pre cscas.f
	cp cscas.source.f fort.10
	./pre.exe
	mv fort.40 cscas.f
	rm fort.*
	$(FC) -c -o cscas.o cscas.f $(OPTS);

clean:
	rm *.o $(POSITION)$(TITLE) pre.exe;
