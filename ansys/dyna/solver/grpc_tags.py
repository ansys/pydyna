#
# Tags for requests to DYNA
#
# The format here kind of sucks, but you can blame PEP8/flake8 for
# that because it INSISTS on having EXACTLY 1 space before and after
# the = signs, so I can't line them up unless I make all the tag names
# the same number of characters long.  This would be much more readable:
#
# SWITCH = 1
# RUN    = 2
# CWD    = 3
# PAUSE  = 4
#
# But no, that is not PEP8 compliant.
#
# At least it lets me line up my comments, provided they have at least TWO
# spaces before the #.
#
SWITCH = 1  # sense switch
RUN = 2  # send command line arguments & begin execution
CWD = 3  # server sends to DYNA to find its working directory
PAUSE = 4  # pause until told to resume
TIME = 5  # return simulation time
RESUME = 6  # resume (until optional cycle/time)
REQUIRES_SYNC = 1000  # values above here require MPP synchorinzation
NODE = 1001  # return (pos, vel) for a node
SETLC = 1002  # set the value of a load curve to a supplied constant
#
# Possible return codes:
#
ACK = 0  # Success
NOTRUNNING = 1  # Dyna is not running (used by server, not DYNA)
RUNNING = 2  # Dyna is already running (used by server, not DYNA)
NOTFOUND = 3  # requested item not found
