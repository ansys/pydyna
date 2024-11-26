#! /bin/bash

# inputs are all environment variables
# DYNA_OPTION: lsdyna solver choice, DP, SP, DP_MPP, or SP_MPP
# DYNA_NCPU: ncpu (or number of ranks if MPI)
# DYNA_ARGS: rest of the command line args, e.g. i=, memory=

cd /run

if [[ "$DYNA_OPTION" == "DP" ]]; then
    $LSDYNA_DP ncpu=$DYNA_NCPU $DYNA_ARGS
elif [[ "$DYNA_OPTION" == "SP" ]]; then
    $LSDYNA_SP ncpu=$DYNA_NCPU $DYNA_ARGS
elif [[ "$DYNA_OPTION" == "DP_MPP" ]]; then
    source $MPIVARS
    $MPIRUN -np $DYNA_NCPU $LSDYNA_DP_MPP $DYNA_ARGS
elif [[ "$DYNA_OPTION" == "SP_MPP" ]]; then
    source $MPIVARS
    $MPIRUN -np $DYNA_NCPU $LSDYNA_SP_MPP $DYNA_ARGS
fi
