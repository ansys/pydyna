#!/usr/bin/python3
# import logging

import grpc
import dynasolver_pb2
import dynasolver_pb2_grpc
import os
import sys
from grpc_tags import *
#
class RunningError(Exception):
  pass
class NotFound(Exception):
  pass
class UnexpectedResponse(Exception):
  pass
#
class DynaSolver:
  """Class for the gRPC client side of DYNA"""

  def __init__(self,hostname,port):
    """Create client instance connected to the hostname (or ip) and port"""
    self.channel = grpc.insecure_channel(hostname + ":" + port)
    self.stub = dynasolver_pb2_grpc.DynaSolverCommStub(self.channel)

  def _argcheck(self,cmd,ngiven,nrequired):
    if(ngiven < nrequired):
      print("Bad input for command %s:" % cmd)
      print("At least %d arguments are required, but only %d were given" % (nrequired,ngiven))
      return 0
    return 1

  def check_return(self,response):
    """Internally used routine for checking command response"""
    if(response.tag == TAG_ACK):
      return
    if(response.tag == TAG_NOTRUNNING):
      raise RunningError("DYNA is not running")
    if(response.tag == TAG_RUNNING):
      raise RunningError("DYNA is already running")
    if(response.tag == TAG_NOTFOUND):
      raise NotFound("Requested %s (%s) not found" % (self.itemtype,self.itemname))
    raise UnexpectedResponse("Unknown return value %d" % response.tag)
    return

  def listfiles(self,subname=None):
    """
    Return size information about one or more files in the Dyna working directory

    If subname is not None, only files whose names contain subname as a substring
    are returned.  This check is case insensitive.

    An array is returned holding (filename, size in bytes) pairs
    """
    request = dynasolver_pb2.DynaSolverFileRequest()
    if(subname):
      request.name = bytes(subname,'utf-8')
    response=self.stub.listFiles(request)
    ret=[]
    n=len(response.name)
    for i in range(n):
      ret.append((str(response.name[i],'utf-8'),response.size[i]))
    return ret

  def node(self,n):
    """
    Return the current location and velocity of a node

    A pair of 3-tuples are returned, giving the (x,y,z) coordinates and
    velocities of the node.  If the node number given does not exist
    in the model, then (None,None) is returned.
    """
    request = dynasolver_pb2.DynaSolverRelay()
    request.tag = TAG_NODE
    self.itemtype='node'
    self.itemname="%d" % n
    request.i8.append(n)
    response = self.stub.sendRequest(request)
    if(response.tag == TAG_NODE):
      X = tuple(response.r8[0:3])
      V = tuple(response.r8[3:6])
      return (X,V)
    self.check_return(response)
    return (None,None)

  def pause(self):
    """
    Pause DYNA execution

    Execution will stop until told to resume or quit.  Most "switch" commands
    will result in 1 cycle being executed so that the switch can be handled,
    and then DYNA will pause again
    """
    request = dynasolver_pb2.DynaSolverRelay()
    request.tag = TAG_PAUSE
    response=self.stub.sendRequest(request)
    self.check_return(response)
    return

  def pull(self,fname):
    """
    Pull a file from the gRPC server

    Returns the number of bytes received
    """
  #
  # This makes a single request, but gets a stream of data back.
  #
    request=dynasolver_pb2.DynaSolverFileRequest(name=bytes(fname,'utf-8'))
    response_iterator = self.stub.downloadFile(request)
    fp = open(fname,'wb')
    fsize=0
    for response in response_iterator:  # process all returned packets
      fp.write(response.b)
      fsize=fsize+len(response.b)
    fp.close()
    return fsize

  def push(self,fname):
    """
    Push a file to the gRPC server

    Returns the number of bytes sent
    """
    #
    # This request sends a stream of data to the server, and
    # gets a single response back.
    #
    # First packet contains filename, the rest hold the contents
    #
    fsize=0
    def push_packets(fname):
      nonlocal fsize
      request = dynasolver_pb2.DynaSolverFileData()
# Only send the base file name, not the whole path!
      bfname=os.path.split(fname)[1]
      request.b=bytes(bfname,'utf-8')
      yield request
      fp = open(fname, 'rb')
      blocksize = 1000000
      n = blocksize
      while (n == blocksize):
        request = dynasolver_pb2.DynaSolverFileData()
        request.b = fp.read(blocksize)
        n = len(request.b)
        fsize = fsize + n
        yield request
      fp.close()
    #
    # Now use that generator to push a stream of packets to the server
    #
    self.stub.uploadFile(push_packets(fname))
    return fsize

  def quit(self):
    """
    Terminate the gRPC server program.  This does not terminate DYNA

    If the server is running inside a container, it will ignore this command and
    continue running
    """
    request = dynasolver_pb2.QuitServer()
    response = self.stub.quitServer(request)  # ALWAYS returns ACK, so ignore it
    return

  def resume(self,cycle=None,time=None):
    """
    Resume execution, with an optional cycle number and/or time at which to pause

    This command can be given whether DYNA is paused or running, and DYNA will
    run until the given cycle number or simulation time.  If both are given,
    it will stop based on whichever occurs first.  If neither are given, it will
    run until termination or until paused
    """
    request = dynasolver_pb2.DynaSolverRelay()
    request.tag = TAG_RESUME
    try:
      if(not cycle == None):
        request.i8.append(cycle)
    except:
      raise TypeError("Cycle must be an integer, or None")
    try:
      if(not time == None):
        request.r8.append(time)
    except:
      raise TypeError("Time must be a number, or None")
    response = self.stub.sendRequest(request)
    self.check_return(response)
    return

  def run(self,args):
    """
    Begin execution with the given string as the command line arguments
    """
    request = dynasolver_pb2.DynaSolverRelay()
    request.tag = TAG_RUN
    request.b = bytes(args,'utf-8')
    response = self.stub.sendRequest(request)
    self.check_return(response)
    return

  def setlc(self,lc,value):
    """
    Set the given load curve to a constant given value
    """
    request = dynasolver_pb2.DynaSolverRelay()
    request.tag = TAG_SETLC
    try:
      request.i8.append(lc)
    except:
      raise TypeError("Loadcurve ID must be an integer")
    try:
      request.r8.append(value)
    except:
      raise TypeError("Value must be a number")
    self.itemtype='loadcurve'
    self.itemname="%d" % lc
    response = self.stub.sendRequest(request)
    self.check_return(response)
    return

  def start(self,nproc):
    """
    Start nproc cores of DYNA in the current Docker or Kubernetes environment

    The program will start and await further input.  To actually begin a simulation
    the "run" command must used to send the command line arguements.

    After starting, but before running, a "resume" command can be sent to set a
    pause time or cycle.
    """
    request = dynasolver_pb2.DynaSolverStart()
    request.exename=b'mppdyna'
    request.nproc=nproc
    response = self.stub.startSolver(request)
    if(response.status == TAG_RUNNING):
      raise RunningError("DYNA is already running")
    return

  def switch(self,args):
    """Send a "sense switch" to DYNA"""
    request = dynasolver_pb2.DynaSolverRelay()
    request.tag = TAG_SWITCH
    request.b = bytes(args,'utf-8')
    response=self.stub.sendRequest(request)
    if(response.tag == TAG_SWITCH):
      return str(response.b,'utf-8')
    self.check_return(response)
    return ""

  def tail(self,which):
    """
    Monitor the stdout or stderr of the running job

    which: 1=stdout, 2=stderr

    the contents will be streamed continuously to the client, updated
    in real time, until interrupted or DYNA terminates.
    """
  #
  # This makes a single request, but gets a stream of data back.
  #
    request=dynasolver_pb2.DynaSolverTailRequest(which=which)  # 1 for stdout, 2 for stderr
    response_iterator = self.stub.tailFile(request)
    for response in response_iterator:  # process all returned packets
      sys.stdout.write(str(response.b,'utf-8'))
      sys.stdout.flush()

  def time(self):
    """
    Return the current cycle count and simulation time
    """
    request = dynasolver_pb2.DynaSolverRelay()
    request.tag = TAG_TIME
    response = self.stub.sendRequest(request)
    if(response.tag == TAG_TIME):
      return (response.i8[0], response.r8[0])
    self.check_return(response)
    return (None,None)

  def send(self,cmdin):
    """
    Command line interface to send one request to DYNA:

    Description                                   Example

    "list" then optional file name                list d3plot
    "node" user node number                       node 43444
    "pause" (no arguments)                        pause
    "pull" then file name                         pull glstat
    "push" then file name                         push input.k
    "quit" (no arguments)                         quit
    "resume" optional cycle and/or time           resume 5000 0.25
    "run" then the command line                   run i=input.k jobid=xx
    "setlc" user load curve number and value      setlc 1075 0.245
    "start" number of processors                  start 5
    "switch" then switch text                     switch sw2. 
    "tail" then 1 for stdout, 2 for stderr        tail 1
    "time" (no arguments)                         time
    """
    try:
      (cmd, args) = cmdin.split(None, 1)
    except:
      cmd = cmdin
      args = ""
    sargs=args.split()
    nsargs=len(sargs)
#
    if (cmd == 'list'):
      if(nsargs > 0):
        finfo=self.listfiles(sargs[0])
      else:
        finfo=self.listfiles()
# To make things pretty, find a reasonable formatting to use
      max1=0
      max2=0
      for dat in finfo:
        n1=len("%s" % dat[0])
        n2=len("%d" % dat[1])
        if(n1 > max1):
          max1=n1
        if(n2 > max2):
          max2=n2
        fmt="%%-%ds  %%%dd" % (max1,max2)
      for dat in finfo:
        print(fmt % dat)
    elif (cmd == 'node'):
      if(not self._argcheck("node",nsargs,1)):
        return
      try:
        (a,b) = self.node(int(sargs[0]))
      except NotFound as err:
        print(err)
      else:
        if(not a==None):
          print("X=%.10e %.10e %.10e" % a)
          print("V=%.10e %.10e %.10e" % b)
    elif (cmd == 'pause'):
      self.pause()
    elif (cmd == 'pull'):
      if(not self._argcheck("pull",nsargs,1)):
        return
      flen=self.pull(sargs[0])
      print("Pulled %d bytes" % flen)
    elif (cmd == 'push'):
      if(not self._argcheck("push",nsargs,1)):
        return
      flen=self.push(sargs[0])
      print("Pushed %d bytes" % flen)
    elif (cmd == 'quit'):
      self.quit()
    elif (cmd == 'resume'):
      c=None
      t=None
      for a in sargs:
        if(a.find(".") >= 0):
          t=float(a)
        else:
          c=int(a)
      self.resume(c,t)
    elif (cmd == 'run'):
      if(not self._argcheck("run",nsargs,1)):
        return
      self.run(args)
    elif (cmd == 'setlc'):
      if(not self._argcheck("setlc",nsargs,2)):
        return
      try:
        self.setlc(int(sargs[0]),float(sargs[1]))
      except NotFound as err:
        print(err)
    elif (cmd == 'start'):
      if(not self._argcheck("start",nsargs,1)):
        return
      nproc=int(sargs[0])
      try:
        self.start(nproc)
      except RunningError as err:
        print(err)
    elif (cmd == 'switch'):
      if(not self._argcheck("switch",nsargs,1)):
        return
      s = self.switch(args)
      print(s)
    elif (cmd == 'tail'):
      if(not self._argcheck("tail",nsargs,1)):
        return
      self.tail(int(sargs[0]))
    elif (cmd == 'time'):
      (a,b) = self.time()
      if(not a==None):
        print("Simulation cycle=%d, time=%.10e" % (a,b))
    else:
      print("Unknown command")
      return
    return

  def runfile(self,fname):
    """
    Read command lines from the file given, and execute them

    Each line is read from the file and echoed to the screen.  You
    must hit the "enter" key to trigger execution of the command.
    """
    f = open(fname, "r")
    cmds = [x.strip() for x in f.readlines()]
    f.close()
    for cmdin in cmds:
      input("> %s " % cmdin)
      self.send(cmdin)
