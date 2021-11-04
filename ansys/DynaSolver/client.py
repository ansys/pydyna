#!/usr/bin/python3
# import logging

import os
import sys
from DynaSolver import *
#
hostname = 'localhost'
port = '5000'
#
def get_from_minikube(service):
  f=os.popen("minikube ip","r")
  ip=f.readline().strip()
  f.close
  f=os.popen("kubectl get svc %s" % service,"r")
  f.readline()
  p=f.readline()
  n1=p.find(":")
  n2=p.find("/")
  p=p[n1+1:n2]
  return (ip,p)
#
# Open gRPC connection to the server, and loop
# processing commands until we get a "quit" command
#
try:
  hostname = sys.argv[1]
except:
  hostname = 'localhost'
try:
  port    = sys.argv[2]
  service = port
except:
  port    = '5000'
  service = 'server'
#
# Special code here for testing on my system with minikube:
# if run with just "minikube" as the argument, figure out
# the correct IP address and port to use.  If there is a second
# argument, it is the name of the "server" service, which defaults to "server"
#
if(hostname == 'minikube'):
  (hostname,port) = get_from_minikube(service)
  print("Using %s:%s" % (hostname,port))
#
# Open gRPC connection to the server
#
dyna = DynaSolver(hostname,port)
#
# Loop, processing commands until we get a "quit" command
#
while(1):
  cmdin=input("> ").rstrip()
  try:
    dyna.send(cmdin)
    if(cmdin == 'quit'):
      sys.exit(0)
  except RunningError as err:
    print(err)
