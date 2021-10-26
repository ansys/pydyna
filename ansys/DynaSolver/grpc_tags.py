#
# Tags for requests to DYNA
#
TAG_SWITCH = 1    # sense switch
TAG_RUN    = 2    # send command line arguments & begin execution
TAG_CWD    = 3    # server sends to DYNA to find its working directory
TAG_PAUSE  = 4    # pause until told to resume
TAG_TIME   = 5    # return simulation time
TAG_RESUME = 6    # resume (until optional cycle/time)
TAG_REQUIRES_SYNC = 1000  # values above here require MPP synchorinzation to complete
TAG_NODE   = 1001 # return (pos, vel) for a node
TAG_SETLC  = 1002 # set the value of a load curve to a supplied constant
#
# Possible return codes:
#
TAG_ACK           = 0 # Success
TAG_NOTRUNNING    = 1 # Dyna is not running (used by server, not DYNA)
TAG_RUNNING       = 2 # Dyna is already running (used by server, not DYNA)
TAG_NOTFOUND      = 3 # requested item not found
