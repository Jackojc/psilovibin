# psilovibin version (ISO 8601)
VERSION=2022-12-29

PREFIX=/usr/local
MANPREFIX=$(PREFIX)/share/man

INC=-Isrc/ -Imodules/conflict/include
LIB=-ljack

CXX=g++

PV_CXXSTD=c++17
PV_CXXWARN=-Werror -Wall -Wextra -Wno-unused -pedantic -Wno-unused-parameter

PV_DBG_CPPFLAGS=-DPV_VERSION=\"$(VERSION)\"
PV_DBG_CXXFLAGS=-std=$(PV_CXXSTD) $(PV_CXXWARN) $(PV_CPPFLAGS) \
	-Og -g -fno-omit-frame-pointer $(CXXFLAGS) $(INC)
PV_DBG_LDFLAGS=$(LIB) $(LDFLAGS)

PV_REL_CPPFLAGS=-DPV_VERSION=\"$(VERSION)\" -DNDEBUG
PV_REL_CXXFLAGS=-std=$(PV_CXXSTD) $(PV_CXXWARN) $(PV_CPPFLAGS) \
	-march=native -O3 $(CXXFLAGS) $(INC)
PV_REL_LDFLAGS=-flto -s $(LIB) $(LDFLAGS)

release?=no

ifeq ($(release),yes)
	PV_CPPFLAGS=$(PV_REL_CPPFLAGS)
	PV_CXXFLAGS=$(PV_REL_CXXFLAGS)
	PV_LDFLAGS=$(PV_REL_LDFLAGS)
else ifeq ($(release),no)
	PV_CPPFLAGS=$(PV_DBG_CPPFLAGS)
	PV_CXXFLAGS=$(PV_DBG_CXXFLAGS)
	PV_LDFLAGS=$(PV_DBG_LDFLAGS)
else
$(error DBG should be either yes or no)
endif

