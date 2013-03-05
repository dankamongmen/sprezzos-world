#!/usr/bin/make -f

MCS = /usr/bin/mono-csc
TARGET = Nini.dll
KEYFILE = debian/Nini.snk

SOURCES = Source/AssemblyInfo.cs \
          Source/Config/*.cs \
	  Source/Ini/*.cs \
	  Source/Util/*.cs

DEFINES = -define:MONO_2_0 \
          -define:STRONG
	  
REFERENCES = -r:System.dll \
             -r:System.Xml.dll

all: $(TARGET)

$(TARGET): $(KEYFILE) $(SOURCES)
	mkdir -p build
	$(MCS) -debug -nowarn:1616 -target:library -out:build/"$@" \
	$(DEFINES) $(REFERENCES) -keyfile:$(KEYFILE) $(SOURCES)


