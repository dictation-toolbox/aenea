#!/usr/bin/python

import comsat, sys

with  comsat.ComSat() as cs:
  print cs
  rpc = cs.getRPCProxy()
  rpc.callLog("hello")
