#  Copyright 2020 University of Edinburgh
#
#  Licensed under the Apache License, Version 2.0 (the "License");
#  you may not use this file except in compliance with the License.
#  You may obtain a copy of the License at
#
#      http://www.apache.org/licenses/LICENSE-2.0
#
#  Unless required by applicable law or agreed to in writing, software
#  distributed under the License is distributed on an "AS IS" BASIS,
#  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
#  See the License for the specific language governing permissions and
#  limitations under the License.
#
###########################################################################
#
#  This software was developed as part of the EPSRC funded project
#  ASiMoV (Project ID: EP/S005072/1)
#
###########################################################################

CC = gcc
FC = gfortran

CFLAGS = -O3
FFLAGS = -O3 -std=f2003 -fcray-pointer

all: build

build-c:
	$(CC) $(CFLAGS) -c cmalloc.c

build: build-c
	$(FC) $(FFLAGS) -o stream stream.f90 cmalloc.o

clean:
	rm stream *.mod *.o
