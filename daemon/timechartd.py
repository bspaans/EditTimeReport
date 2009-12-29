#!/usr/bin/env python

import basicliser.server as server
import sys
import os.path as path
from basicliser.shared.CallMap import State, Command

import time


OUTPUT = None

class NotifyProtocol(server.ServerProtocol):
    def updateCallMap(self):
        self.callmap.add_state(State("loggedin",
                 [
                  Command("vimStarted" , "vimStarted" , []) , 
                  Command("vimEnded"   , "vimEnded"   , []) , 
                  Command("writeFile"  , "vimWriteFile"  , None) ,
                  Command("newFile"  , "vimNewFile"  , None) ,

                 ],
                  ))

        self.disableVersionCheck()
        self.dropOnInvalidCommand()

    def loggedIn(self):
        if self.userName != "vim":
            self.quitSession(None)

    def vimEnded(self, data):
        self.append("END")

    def vimStarted(self, data):
        self.append("START")

    def vimWriteFile(self, params):
        file = " ".join(params)
        self.append("EDIT " + file)

    def vimNewFile(self, params):
        self.append("NEW %s" % " ".join(params))

    def append(self, msg):
        t = time.localtime()
        OUTPUT.write(str.format("{0.tm_year} {0.tm_mon} {0.tm_mday} {0.tm_hour} {0.tm_min} {0.tm_sec} {0.tm_wday} {0.tm_yday} {1}\n",t, msg))
        OUTPUT.flush()

if __name__ == "__main__":
    if len(sys.argv) <= 1:
        print "Missing PATH to configuration argument"
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

