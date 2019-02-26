# makefile created on 2019-02-27 08:25:26.672422
# by pymake (version 1.1.0)
# using the gfortran fortran and gcc c/c++ compilers.

# Define the directories for the object and module files,
# the executable, and the executable name and path.
OBJDIR = ./obj_temp
BINDIR = .
PROGRAM = mfusg

SOURCEDIR1=../src

VPATH = \
${SOURCEDIR1} 

.SUFFIXES: .c .cpp .f .f90 .F90 .fpp .o

# Define the Fortran compile flags
FC = gfortran
FFLAGS = -O2 -fbacktrace 

# Define the C compile flags
CC = gcc
CFLAGS = -O3 -D_UF 

# Define the libraries
SYSLIBS = 

OBJECTS = \
$(OBJDIR)/gmodules.o \
$(OBJDIR)/gwf2sub7u1.o \
$(OBJDIR)/gwf2rch8u1.o \
$(OBJDIR)/sparse.o \
$(OBJDIR)/disu2gncn1.o \
$(OBJDIR)/lak_gag_sfr_modules.o \
$(OBJDIR)/gwf2drn7u1.o \
$(OBJDIR)/gwf2gag7u1.o \
$(OBJDIR)/gwf2str7u1.o \
$(OBJDIR)/gwf2ghb7u1.o \
$(OBJDIR)/gwf2basu1.o \
$(OBJDIR)/parutl7.o \
$(OBJDIR)/cln2props1.o \
$(OBJDIR)/utl7u1.o \
$(OBJDIR)/gwf2drt8u.o \
$(OBJDIR)/gwf2chd7u1.o \
$(OBJDIR)/glo2btnu1.o \
$(OBJDIR)/gwf2fhb7u1.o \
$(OBJDIR)/gwf2ets8u1.o \
$(OBJDIR)/pcgu7.o \
$(OBJDIR)/gwf2hfb7u1.o \
$(OBJDIR)/cln2basu1.o \
$(OBJDIR)/xmdlib_2.o \
$(OBJDIR)/disu2gncb1.o \
$(OBJDIR)/gwf2riv7u1.o \
$(OBJDIR)/gwf2lak7u1.o \
$(OBJDIR)/gwf2wel7u1.o \
$(OBJDIR)/glo2basu1.o \
$(OBJDIR)/gwf2sfr7u1.o \
$(OBJDIR)/gwf2evt8u1.o \
$(OBJDIR)/xmd.o \
$(OBJDIR)/glo2sms-u1.o \
$(OBJDIR)/gwf2bcf-lpf-u1.o \
$(OBJDIR)/mfusg.o 

# Define task functions

# Create the bin directory and compile and link the program
all: makebin | $(PROGRAM)

# Make the bin directory for the executable
makebin :
	mkdir -p $(BINDIR)

# Define the objects that make up the program
$(PROGRAM) : $(OBJECTS)
	-$(FC) $(FFLAGS) -o $@ $(OBJECTS) $(SYSLIBS) -I$(OBJDIR) -J$(OBJDIR) 

$(OBJDIR)/%.o : %.f
	@mkdir -p $(@D)
	$(FC) $(FFLAGS) -c $< -o $@ -I$(OBJDIR) -J$(OBJDIR) 

$(OBJDIR)/%.o : %.f90
	@mkdir -p $(@D)
	$(FC) $(FFLAGS) -c $< -o $@ -I$(OBJDIR) -J$(OBJDIR) 

$(OBJDIR)/%.o : %.F90
	@mkdir -p $(@D)
	$(FC) $(FFLAGS) -c $< -o $@ -I$(OBJDIR) -J$(OBJDIR) 

$(OBJDIR)/%.o : %.fpp
	@mkdir -p $(@D)
	$(FC) $(FFLAGS) -c $< -o $@ -I$(OBJDIR) -J$(OBJDIR) 

$(OBJDIR)/%.o : %.c
	@mkdir -p $(@D)
	$(CC) $(CFLAGS) -c $< -o $@

$(OBJDIR)/%.o : %.cpp
	@mkdir -p $(@D)
	$(CC) $(CFLAGS) -c $< -o $@

# Clean the object and module files and the executable
.PHONY : clean
clean : 
	-rm -rf $(OBJDIR)
	-rm -rf $(PROGRAM)

# Clean the object and module files
.PHONY : cleanobj
cleanobj : 
	-rm -rf $(OBJDIR)

