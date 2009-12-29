from twisted.internet.protocol import Protocol
from basicliser.shared.plainTextDataParser import plainTextDataParser



class AbstractClientProtocol(Protocol):

    def connectionMade(self):
        self.dataParser = plainTextDataParser(self)
        self.log = self.factory.log
        self.welcome()

    def connectionLost(self, reason):
        pass

    def dataReceived(self, data):
        self.dataParser.parseData(data)

    def error(self, msg):
        print "ERROR", msg

    def logMessage(self, msg, msg_type):
        self.log.notify(msg, msg_type)

    def welcome(self):
        pass

    def writeCommand(self, cmd):
        self.transport.write(cmd + "\n")
