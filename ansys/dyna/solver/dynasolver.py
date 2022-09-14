"""
Dyna solver API
===============

Define Dyna solver API
"""

#!/usr/bin/python3
from . import dynasolver_pb2
from . import dynasolver_pb2_grpc
from . import grpc_tags as tag
import grpc
import logging
import os
import queue
import sys
import threading


#
# Define our own exceptions
#


class RunningError(Exception):
    pass


class NotFound(Exception):
    pass


class UnexpectedResponse(Exception):
    pass
#

class DynaSolver:
    """Class for the gRPC client side of LSDYNA."""
    #logger = None
    def __init__(self, hostname, port):
        """Create client instance connected to the hostname (or ip) and port."""
        self.hostname = hostname
        self.port = port
        self.channel = grpc.insecure_channel(hostname + ":" + port)
        self.stub = dynasolver_pb2_grpc.DynaSolverCommStub(self.channel)
        #if DynaSolver.logger is None:
        #    DynaSolver.logger = logging.getLogger("DynaSolver")
        #    DynaSolver.logger.setLevel(logging.INFO)
        #    fh = logging.FileHandler('DynaSolver.log')
        #    fm = logging.Formatter('%(asctime)s - %(name)s - %(message)s',
        #                           '%m/%d/%Y %H:%M:%S')
        #    fh.setFormatter(fm)
        #    DynaSolver.logger.addHandler(fh)
        #    DynaSolver.logger.propagate = False
        #self.logger = DynaSolver.logger
        self.logger = logging.getLogger("DynaSolver")

    def _argcheck(self, cmd, ngiven, nrequired):
        if(ngiven < nrequired):
            s = "Bad input for command %s:" % cmd
            s = s + ("At least %d arguments are required, but only %d "
                     "were given" % (nrequired, ngiven))
            self.logger.warning(s)
            return 0
        return 1

    def _check_return(self, response):
        """Internally used routine for checking command response."""
        if(response.tag == tag.ACK):
            return
        if(response.tag == tag.NOTRUNNING):
            raise RunningError("LSDYNA is not running")
        if(response.tag == tag.RUNNING):
            raise RunningError("LSDYNA is already running")
        if(response.tag == tag.NOTFOUND):
            raise NotFound(
                "Requested %s (%s) not found" % (self.itemtype, self.itemname))
        raise UnexpectedResponse("Unknown return value %d" % response.tag)
        return

    def _set_client_log_level(self, levelname):
        """Internally used routine to set logging level on the client."""
        if(levelname in ('DEBUG', 'INFO', 'WARNING', 'ERROR')):
            self.logger.setLevel(levelname)
        else:
            self.logger.setLevel('INFO')
            self.logger.info('Invalid log level %s specified, reset to INFO'
                             % levelname)

    def _set_server_log_level(self, levelname):
        """Internally used routine to set logging level on the server."""
        request = dynasolver_pb2.LogLevel()
        request.level = bytes(levelname, 'utf-8')
        self.stub.log_level(request)

    def _set_log_level(self, levelname):
        """Internally used routine to set logging level of both the
        server and the client."""
        self._set_client_log_level(levelname)
        self._set_server_log_level(levelname)

    def list_files(self, subname=None):
        """Return size information about one or more files in the LSDYNA
        working directory.

        Parameters
        ----------
        subname, string, optional, default=None :  If given, only files whose
            names contain subname as a substring are returned.  This check is
            case insensitive.

        Returns
        -------
        An array is of (filename, size in bytes) pairs.
        """
        self.logger.debug("list_files: subname=%s" % subname)
        request = dynasolver_pb2.DynaSolverFileRequest()
        if(subname):
            request.name = bytes(subname, 'utf-8')
        response = self.stub.list_files(request)
        ret = []
        n = len(response.name)
        for i in range(n):
            ret.append((str(response.name[i], 'utf-8'), response.size[i]))
        return ret

    def node(self, n):
        """Return size information about one or more files in the Dyna
        working directory.

        Parameters
        ----------
        n, integer, required : The user ID of a node in the model.

        Returns
        -------
        A pair of 3-tuples are returned, giving the (x, y, z) coordinates and
        velocities of the node.  If the node number given does not exist
        in the model, then (None, None) is returned.
        """
        self.logger.debug("node: %d" % n)
        request = dynasolver_pb2.DynaSolverRelay()
        request.tag = tag.NODE
        self.itemtype = 'node'
        self.itemname = "%d" % n
        request.i8.append(n)
        response = self.stub.send_request(request)
        if(response.tag == tag.NODE):
            X = tuple(response.r8[0:3])
            V = tuple(response.r8[3:6])
            return (X, V)
        self._check_return(response)
        return (None, None)

    def pause(self):
        """Pause LSDYNA execution.

        Execution will stop until told to resume or quit.  Most "switch"
        commands will result in 1 cycle being executed so that the switch can
        be handled, and then LSDYNA will pause again.
        """
        self.logger.debug("pause")
        request = dynasolver_pb2.DynaSolverRelay()
        request.tag = tag.PAUSE
        response = self.stub.send_request(request)
        self._check_return(response)
        return

    def pull(self, fname):
        """Alias for "download", for backward compatibility."""
        return self.download(fname)

    def download(self, fname):
        """Download a file from the gRPC server.

        Parameters
        ----------
        fname, string, required : File to download.  The file will be
            downloaded to the current working directory of the calling
            process.  fname should not begin with a leading /, and will be
            interpreted relative to the LSDYNA working directory.

        Returns
        -------
        The number of bytes received.
        """
        self.logger.debug("download: %s" % fname)
    #
    # This makes a single request, but gets a stream of data back.
    #
        request = dynasolver_pb2.DynaSolverFileRequest(
            name=bytes(fname, 'utf-8'))
        response_iterator = self.stub.download_file(request)
        fp = open(fname, 'wb')
        fsize = 0
        for response in response_iterator:    # process all returned packets
            fp.write(response.b)
            fsize = fsize+len(response.b)
        fp.close()
        return fsize

    def push(self, fname):
        """Alias for "upload", for backward compatibility."""
        return self.upload(fname)

    def upload(self, fname):
        """Upload a file to the LSDYNA working directory.

        Parameters
        ----------
        fname, string, required : The local name of the file to be sent.  The
        contents of this file are copied into the LSDYNA working directory,
        in a file of the same name, but with any path components removed.

        Returns
        -------
        Returns the number of bytes sent.
        """
        #
        # This request sends a stream of data to the server, and
        # gets a single response back.
        #
        # First packet contains filename, the rest hold the contents
        #
        self.logger.debug("upload: %s" % fname)
        fsize = 0

        def push_packets(fname):
            nonlocal fsize
            request = dynasolver_pb2.DynaSolverFileData()
            # Only send the base file name, not the whole path!
            bfname = os.path.split(fname)[1]
            request.b = bytes(bfname, 'utf-8')
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
        self.stub.upload_file(push_packets(fname))
        return fsize

    def quit(self):
        """Terminate the gRPC server program.  This does not terminate LSDYNA.

        If the server is running inside a container, it will ignore this
        command and continue running.
        """
        self.logger.debug('quit')
        request = dynasolver_pb2.QuitServer()
        # ALWAYS returns ACK, so don't bother checking
        self.stub.quit_server(request)
        return

    def resume(self, cycle=None, time=None):
        """ Resume execution.

        Parameters
        ----------
        cycle, integer, optional : cycle on which to pause execution
        time, float, optional : simulation time at which to pause execution

        This command can be given whether LSDYNA is paused or running, and
        LSDYNA will run until the given cycle number or simulation time.  If
        both are given, it will stop based on whichever occurs first.  If
        neither are given, it will run until termination or until paused.
        """
        if(cycle):
            cc = 'cycle=%d ' % cycle
        else:
            cc = 'cycle=None '
        if(time):
            tt = 'time=%f' % time
        else:
            tt = 'time=None'
        self.logger.debug('resume: '+cc+tt)
        request = dynasolver_pb2.DynaSolverRelay()
        request.tag = tag.RESUME
        try:
            if(cycle):
                request.i8.append(cycle)
        except TypeError:
            raise TypeError("resume: Cycle must be an integer, or None")
        try:
            if(time):
                request.r8.append(time)
        except TypeError:
            raise TypeError("resume: Time must be a number, or None")
        response = self.stub.send_request(request)
        self._check_return(response)
        return

    def run(self, args):
        """Begin execution with the given string as the command line arguments.

        Parameters
        ----------
        args, string, required : The command line to pass to LSDYNA.
        """
        self.logger.debug('run: %s' % args)
        request = dynasolver_pb2.DynaSolverRelay()
        request.tag = tag.RUN
        request.b = bytes(args, 'utf-8')
        response = self.stub.send_request(request)
        self._check_return(response)
        return

    def setlc(self, lc, value):
        """ Set the given load curve to a constant given value.

        Parameters
        ----------
        lc, integer, required : The user ID of the load curve to set
        value, real, required : The value to set the load curve to.
        """
        self.logger.debug('setlc: %d %f' % (lc, value))
        request = dynasolver_pb2.DynaSolverRelay()
        request.tag = tag.SETLC
        try:
            request.i8.append(lc)
        except TypeError:
            raise TypeError("setlc: Loadcurve ID must be an integer")
        try:
            request.r8.append(value)
        except TypeError:
            raise TypeError("setlc: Value must be a number")
        self.itemtype = 'loadcurve'
        self.itemname = "%d" % lc
        response = self.stub.send_request(request)
        self._check_return(response)
        return

    def start(self, nproc):
        """Start LSDYNA.

        The program will start and await further input.  To actually begin a
        simulation the "run" command must used to send the command line
        arguements.

        After starting, but before running, a "resume" command can be sent to
        set a pause time or cycle.

        Parameters
        ----------
        nproc, integer, required : The number of cores (MPI ranks) to run
        """
        self.logger.debug('start: %d' % nproc)
        request = dynasolver_pb2.DynaSolverStart()
        request.exename = b'mppdyna'
        request.nproc = nproc
        response = self.stub.start_solver(request)
        if(response.status == tag.RUNNING):
            raise RunningError("LSDYNA is already running")
        return

    def switch(self, args):
        """Send a "sense switch" to LSDYNA.

        Parameters
        ----------
        args, string, required : The sense switch string.

        Returns
        -------
        If the switch sent is "sw2.", then a string is returned with the
        usual status update information that switch generates.  Otherwise
        and empty string is returned.
        """
        self.logger.debug('switch: %s' % args)
        request = dynasolver_pb2.DynaSolverRelay()
        request.tag = tag.SWITCH
        request.b = bytes(args, 'utf-8')
        response = self.stub.send_request(request)
        if(response.tag == tag.SWITCH):
            return str(response.b, 'utf-8')
        self._check_return(response)
        return ""

    def tail(self, which=1, how=2, queuesize=0):
        """Monitor the stdout or stderr of the running job.

        Parameters
        ----------
        which, integer, default=1 : which stream to monitor.  If which=1,
        stdout is used, otherwise stderr.

        how, integer, default=2 : how the file data should be handled.
            0 : the file contents are printed to stdout and this
                method does not return until LSDYNA terminates.
            1 : a Queue instance is returned.  The queue holds raw blocks
                of text as returned by the server
            2 : a Queue instance is returned.  The queue holds individual
                lines of text

        queuesize, integer, default=0 : size to use for queue instance if
            how > 0

        Returns
        -------
        If how is > 0, a Queue instance is returned.  When a call
        to q.get() returns "None", all data has been received.
        """
        self.logger.debug('tail: which=%d, how=%d, queuesize=%d' %
                          (which, how, queuesize))
#
# If how > 0, we don't want to block.  So create another DynaSolver
# and start the real communication work inside another thread
#
        if(how > 0):
            q = queue.Queue(maxsize=queuesize)
            con2 = DynaSolver(self.hostname, self.port)
            t = threading.Thread(target=con2._tail2, args=(which, q, how),
                                 daemon=True)
            t.start()
            return q
#
# This makes a single request, but gets a stream of data back.
#
        request = dynasolver_pb2.DynaSolverTailRequest(which=which)
        response_iterator = self.stub.tail_file(request)
        for response in response_iterator:  # process all returned packets
            sys.stdout.write(str(response.b, 'utf-8'))
            sys.stdout.flush()

    def _tail2(self, which, q, how):
        """Read packets of stdout/stderr data and place them in a queue.

        Parameters
        ----------
        which : which stream to monitor.  If which=1,
            stdout is used, otherwise stderr.

        q : queue to place data in

        how : how to process the data
            1 : place each received packet into the queue as a string
            2 : convert the received data to individual lines of text
                and put those in the queue.
        """
#
# This makes a single request, but gets a stream of data back.
#
        request = dynasolver_pb2.DynaSolverTailRequest(which=which)
        response_iterator = self.stub.tail_file(request)
        if(how == 1):
            for response in response_iterator:  # process all returned packets
                q.put(str(response.b, 'utf-8'))
            q.put(None)
#
# Actual text lines might be split across responses, so if there is
# a partial text line (not ending in \n), pull it off the end and put
# it on the front of the next response that arrives.
#
        else:
            lastline = ""
            for response in response_iterator:  # process all returned packets
                buf = str(response.b, 'utf-8')
                ret = (lastline+buf).split("\n")
                lastline = ret.pop()
                for line in ret:
                    q.put(line)
# This should only happen if the file did not terminate with a \n
            if(lastline):
                q.put(lastline)
# So the caller knows the difference between "the queue is empty
# but more data is expected" and "No more data is coming", we
# put this on the queue after all data has arrived
        q.put(None)

    def time(self):
        """ Return the current cycle count and simulation time.

        Parameters
        ----------
        none

        Returns
        -------
        A (cycle_count, simulation_time) pair.
        """
        self.logger.debug('time')
        request = dynasolver_pb2.DynaSolverRelay()
        request.tag = tag.TIME
        response = self.stub.send_request(request)
        if(response.tag == tag.TIME):
            return (response.i8[0], response.r8[0])
        self._check_return(response)
        return (None, None)

    def send(self, cmdin):
        """Command line interface to send one request to LSDYNA:

        Parameters
        ----------
        cmdin, string, required : The command to send

        Returns
        -------
        There is no data returned.  Data that is returned from the underlying
        method call is just printed to the screen.  This method is used to
        support the sample interactive "client.py" program

        Description                                Example

        "list" then optional file name             list d3plot
        "node" user node number                    node 43444
        "pause" (no arguments)                     pause
        "download" then file name                  download glstat
        "upload" then file name                    upload input.k
        "quit" (no arguments)                      quit
        "resume" optional cycle and/or time        resume 5000 0.25
        "run" then the command line                run i=input.k jobid=xx
        "setlc" user load curve number and value   setlc 1075 0.245
        "start" number of processors               start 5
        "switch" then switch text                  switch sw2.
        "tail" then 1 for stdout, 2 for stderr     tail 1
        "time" (no arguments)                      time.
        """
        self.logger.debug('send: %s' % cmdin)
        try:
            (cmd, args) = cmdin.split(None, 1)
        except ValueError:
            cmd = cmdin
            args = ""
        sargs = args.split()
        nsargs = len(sargs)
#
        if (cmd == 'list'):
            if(nsargs > 0):
                finfo = self.list_files(sargs[0])
            else:
                finfo = self.list_files()
# To make things pretty, find a reasonable formatting to use
            max1 = 0
            max2 = 0
            for dat in finfo:
                n1 = len("%s" % dat[0])
                n2 = len("%d" % dat[1])
                if(n1 > max1):
                    max1 = n1
                if(n2 > max2):
                    max2 = n2
                fmt = "%%-%ds    %%%dd" % (max1, max2)
            for dat in finfo:
                print(fmt % dat)
        elif (cmd == 'node'):
            if(not self._argcheck("node", nsargs, 1)):
                return
            try:
                (a, b) = self.node(int(sargs[0]))
            except NotFound as err:
                print(err)
            else:
                if(a):
                    print("X=%.10e %.10e %.10e" % a)
                    print("V=%.10e %.10e %.10e" % b)
        elif (cmd == 'pause'):
            self.pause()
        elif (cmd == 'pull' or cmd == 'download'):
            if(not self._argcheck("download", nsargs, 1)):
                return
            flen = self.download(sargs[0])
            print("Downloaded %d bytes" % flen)
        elif (cmd == 'push' or cmd == 'upload'):
            if(not self._argcheck("upload", nsargs, 1)):
                return
            flen = self.upload(sargs[0])
            print("Uploaded %d bytes" % flen)
        elif (cmd == 'quit'):
            self.quit()
            sys.exit(0)
        elif (cmd == 'resume'):
            c = None
            t = None
            for a in sargs:
                if(a.find(".") >= 0):
                    t = float(a)
                else:
                    c = int(a)
            self.resume(c, t)
        elif (cmd == 'run'):
            if(not self._argcheck("run", nsargs, 1)):
                return
            self.run(args)
        elif (cmd == 'setlc'):
            if(not self._argcheck("setlc", nsargs, 2)):
                return
            try:
                self.setlc(int(sargs[0]), float(sargs[1]))
            except NotFound as err:
                print(err)
        elif (cmd == 'start'):
            if(not self._argcheck("start", nsargs, 1)):
                return
            nproc = int(sargs[0])
            try:
                self.start(nproc)
            except RunningError as err:
                print(err)
        elif (cmd == 'switch'):
            if(not self._argcheck("switch", nsargs, 1)):
                return
            s = self.switch(args)
            print(s)
        elif (cmd == 'tail'):
            if(not self._argcheck("tail", nsargs, 1)):
                return
            self.tail(int(sargs[0]))
        elif (cmd == 'time'):
            (a, b) = self.time()
            if(a):
                print("Simulation cycle=%d, time=%.10e" % (a, b))
        else:
            print("Unknown command")
            return
        return

    def runfile(self, fname):
        """
        Read command lines from the file given, and execute them.

        Each line is read from the file and echoed to the screen.  You
        must hit the "enter" key to trigger execution of the command.

        Parameters
        ----------
        fname, string, required : The file to read commands from.
        """
        f = open(fname, "r")
        cmds = [x.strip() for x in f.readlines()]
        f.close()
        for cmdin in cmds:
            input("> %s " % cmdin)
            self.send(cmdin)
