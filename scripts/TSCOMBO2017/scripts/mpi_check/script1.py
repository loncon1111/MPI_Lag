from mpi4py import MPI
#import module_common_mpi
import function_mpi4py
import numpy as np

comm = MPI.COMM_WORLD
fcomm = comm.py2f()
nprocs = comm.Get_size()
irank  = comm.Get_rank()
name   = MPI.Get_processor_name()


ntra = 4512

ista,iend = function_mpi4py.para_range(0, ntra, nprocs, irank)
print ("I am process %d of %d on %s running from %d to %d" %(irank, nprocs, name, ista, iend))
    
