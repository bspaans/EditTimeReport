#!/usr/bin/env python
#
#
# TODO: * Change protocol command names (remove vim from vim* commands)
#       * Drop connection once the command has been made. Don't let 
#            the client wait for the flush.
#       * Drop vim username (give username via cli? Allow login from all?)


# The basicliser library provides a simple, 
# state machine based protocol. It's still a 
# tad big, but has abstracted almost all 
# the network communication and parsing away, 
# which means we can just provide some states 
# and let the library figure out the rest.
#
import basicliser.server as server
from basicliser.shared.CallMap import State, Command


import os.path as path
import sys
import time


FMT = """{0.tm_year} {0.tm_mon} {0.tm_mday} {0.tm_hour} \
{0.tm_min} {0.tm_sec} {0.tm_wday} {0.tm_yday} {1}\n"""

OUTPUT = None


class NotifyProtocol(server.ServerProtocol):
    """A simple server with two states: logged in 
    and not logged in. When logged in, the client 
    can report status updates."""


    # Add a state to the FSM

    def updateCallMap(self):

        self.callmap.add_state(State("loggedin",
                 [
                  Command("vimStarted" , "vimStarted"   , []   ), 
                  Command("vimEnded"   , "vimEnded"     , []   ), 
                  Command("writeFile"  , "vimWriteFile" , None ),
                  Command("newFile"    , "vimNewFile"   , None ),
                 ],))
        self.disableVersionCheck()
        self.dropOnInvalidCommand()


    def loggedIn(self):
        if self.userName != "vim":
            self.quitSession(None)


    # Callbacks

    def vimEnded(self, data):
        self.append("END")


    def vimStarted(self, data):
        self.append("START")


    def vimWriteFile(self, params):
        self.append("EDIT " + " ".join(params))


    def vimNewFile(self, params):
        self.append("NEW %s" % " ".join(params))



    # Append to log file

    def append(self, msg):
        OUTPUT.write(str.format(FMT, time.localtime(), msg))
        OUTPUT.flush()



if __name__ == "__main__":
    if len(sys.argv) <= 1:
        print "Missing `PATH to configuration directory' argument"
        print "The PATH will be used to save the log and the "
        print "server's user database."
        print "Usage: %s PATH" % sys.argv[0]
        sys.exit(1)
    else:
        p  = sys.argv[1]
        if not path.isdir(p):
            print "PATH argument should be a directory"
            sys.exit(1)
        p = path.realpath(p)

    chart = path.join(p, "chart.log")
    OUTPUT = open(chart, "a")

    options = {"port" : 3001, "database": path.join(p, "server.db") } 
    server.startService(conf_file = path.join(p, "server.conf"), 
                        conf_options = options,  
                        protocol = NotifyProtocol)

