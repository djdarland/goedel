# @(#)Makefile 1.1 jiwei 92/08/01 17:50:28
# GOEDEL: the directory which contains the released Goedel system
# GOEDEL=/usr/local/lib/goedel
GOEDEL=/media/ramdisk/Temp/goedel/v4_2

# BIN_DIR: the directory that you want to install the Goedel binary 
BIN_DIR=/media/ramdisk/Temp/goedel/bin

# SICSTUS: your SICStus Prolog
SICSTUS=/usr/local/sicstus4.6.0/bin/sicstus -f
# SICSTUS=/home/miki/goedel-miki/sicstus/sicstus2.1/sicstus


# You don't normally need to change anything below.
SRC_DIR=src

# install: goedel 
# 	mv goedel $(BIN_DIR)/goedel

goedel:
	echo "consult('$(GOEDEL)/$(SRC_DIR)/init.pl')," \
	"goedel,halt." | $(SICSTUS)

#	sicstus -l '$(GOEDEL)/$(SRC_DIR)/init.pl'
#	sicstus -f -l '$(GOEDEL)/$(SRC_DIR)/init.pl'
#	"abolish(system_directory/1),"\
#	"assert(system_directory('$(GOEDEL)/$(SRC_DIR)/')),"\
