# @(#)Makefile 1.1 jiwei 92/08/01 17:50:28
# GOEDEL: the directory which contains the released Goedel system
GOEDEL=/usr/local/lib/goedel

# BIN_DIR: the directory that you want to install the Goedel binary 
BIN_DIR=/usr/local/lib/goedel

# SICSTUS: your SICStus Prolog
SICSTUS=/usr/local/bin/sicstus -f
# SICSTUS=/home/miki/goedel-miki/sicstus/sicstus2.1/sicstus


# You don't normally need to change anything below.
SRC_DIR=src

install: goedel 
	mv goedel $(BIN_DIR)/goedel

goedel:	
	echo "['$(GOEDEL)/$(SRC_DIR)/init.pl']," \
	"abolish(system_directory/1),"\
	"assert(system_directory('$(GOEDEL)/$(SRC_DIR)/')),"\
	"save_goedel,halt." | $(SICSTUS)

