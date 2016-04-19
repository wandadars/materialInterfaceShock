# Makefile for building Material Interface code.
# Start of the makefile

SRC1 = fit3.f fit3_input.f
CC = gfortran
FLAGS = 
TARGET1 = material_interface
OBJECTS = fit3.o fit3_input.o

$(TARGET1): $(SRC1)
	$(CC) $(FLAGS) $(SRC1) -o $(TARGET1) 

all: $(TARGET1)

clean:
	rm $(TARGET1) $(OBJECTS) core
