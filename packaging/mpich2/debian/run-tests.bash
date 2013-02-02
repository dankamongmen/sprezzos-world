#!/bin/bash

set -x
set -e

# Disabled on 2011-12-19, after mail from Pavan Balaji:
# > 4. The run-tests.bash script seems to exit if run as root. Does this
# > have to do something with MPD's problems with running as root as
# > well? The current MPICH2 doesn't distinguish root vs. non-root
# > users, so it should not have any such problem.
# if [ $(id -u) -eq 0 ]; then
#  echo "Building as root, cant run test suite" 
#  exit 0
# fi

cd test/mpi
export LD_LIBRARY_PATH=$(pwd)/../../lib
MPIEXEC=$(pwd)/../../src/pm/hydra/mpiexec.hydra ./configure --with-mpi=$(pwd)/../../
RUNTESTS_VERBOSE=1 make testing || true
