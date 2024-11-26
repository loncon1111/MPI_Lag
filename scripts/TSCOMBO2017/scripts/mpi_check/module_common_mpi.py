#!/usr/bin/env python
# coding: utf-8

# import libraries
from mpi4py import MPI
import numpy as np

def para_range(n1, n2, nprocs, irank):
    
    iwork1 = (n2 - n1) / nprocs
    iwork2 = (n2 - n1) % nprocs
    ista   = irank * iwork1 + n1 + min(irank, iwork2)
    iend   = ista + iwork1 
    if iwork2 > irank:
        iend += 1
    return int(ista),int(iend) 

def para_type_block2a(imin, imax, ilen, jlen):
    inewtype = MPI.Datatype.Create_vector(jlen,ilen,imax - imin + 1) 
    inewtype.Commit()
    return inewtype

def para_type_block2(imin,imax,jmin,ista,iend,jsta,jend,ioldtype):
    lb,isize = ioldtype.Get_extent()
    ilen = len(range(ista,iend))
    jlen = len(range(jsta,jend))
    itemp = ioldtype.Create_vector(jlen,ilen,imax - imin + 1)

    iblock = np.empty(2)
    idisp  = np.empty(2)
    itype  = np.empty(2,dtype = MPI.Datatype)

    iblock[0] = 1
    iblock[1] = 1
    idisp[0]  = 0
    idisp[1]  = ((imax-imin+1) * (jsta-jmin) + (ista-imin)) * isize
    itype[0]  = MPI.LB
    itype[1]  = itemp
    inewtype = MPI.Datatype.Create_struct(iblock,idisp,itype)
    inewtype.Commit()
    return inewtype  
