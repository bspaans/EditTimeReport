from twisted.internet.protocol import Protocol, Factory
from basicliser.shared.plainTextDataParser import plainTextDataParser
from twisted.internet import reactor


class AbstractServerProtocol(Protocol):
    """Serves as a low level protocol implementation. Deals with callbacks from Twisted."""

    ID = -1   # User ID

    def broadcast(self, data, except_self = True):
        """Sends a command to all known clients."""
        self.logMessage(data, self.log.MT_BROADCAST)
        for x in self.factory.clients:
            if except_self:
                if x != self:
                    x.writeCommand(data)
            else:
                x.writeCommand(data)

    def connectionLost(self, reason):
        """Cleans up after lost connection. Callback from Twisted."""
        self.logMessage("Connection lost", self.log.MT_CONN)
        self.factory.users.logout(self.ID)
        self.factory.clients.remove(self)


    def connectionMade(self):
        """Sets up variables. Callback from Twisted. Calls self.welcome when done."""
        self.ip = self.transport.getPeer().host

        if self.factory.users.validIP(self.ip):
            self.factory.clients.append(self)
            self.log = self.factory.log
            self.dataParser = plainTextDataParser(self)
            self.logMessage("%s connected" % self.ip, self.log.MT_CONN)
            self.welcome()
        else:
            self.error("IP is blocked")
            self.loseConnection()



    def dataReceived(self, data):
        """Logs and parses the data. Callback from Twisted."""
        self.logMessage(data, self.log.MT_DATARECV)
        self.dataParser.parseData(data)



    def error(self, data):
        """Sends an error command."""
        self.writeCommand("error %s" % data)



    def logMessage(self, msg, msg_type = -1):
        """Notifies all the log listeners."""
        self.log.notify(msg, msg_type)



    def welcome(self):
        pass



    def write(self, data):
        """Sends data to the client."""
        self.logMessage(data, self.log.MT_DATASEND)
        self.transport.write(data)



    def writeCommand(self, cmd):
        """Sends a command to the client."""
        self.write(cmd + "\n")

