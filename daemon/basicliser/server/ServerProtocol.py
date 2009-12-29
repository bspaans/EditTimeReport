from AbstractServerProtocol import AbstractServerProtocol
from basicliser.shared.CallMap import CallMap, State, Command

class ServerProtocol(AbstractServerProtocol):


    def __init__(self):

        self.callmap = CallMap([
            State("GLOBALS", 
                  [Command("Commands", "sendPossibleCommands"),
                   Command("help", "sendHelp", [str]), 
                   Command("quit", "quitSession")]),
            State("connect", [Command("version", "checkClientVersion", [int])]),
            State("login",   [Command("login", "login", [str, str])]),
            State("loggedin", [])])
        self.state = "connect"

    def disableVersionCheck(self):
        self.state = "login"

    def dropOnInvalidCommand(self):
        self.dataParser.strict = True

    def checkClientVersion(self, params):

        version = params[0]

        if version == 1:
            self.writeCommand("VERSION OK")
            self.state = "login"
        else:
            self.logMessage("Unsupported version %d" % version, self.log.MT_INVALIDPARAMS)
            self.error("Unsupported version")


    def login(self, params):

        name, password = params

        ID = self.factory.users.loginOrRegister(name, password)

        if ID:
            self.ID = ID
            self.userName = name
            self.logMessage("%s logged in [ID: %d]" % (name, self.ID), self.log.MT_CLIENT)
            self.writeCommand("LOGIN OK %s" % name)
            self.state = "loggedin"
            self.loggedIn()
        else:
            self.logMessage("Failed to authenticate with name '%s'" % name, self.log.MT_CLIENT)
            self.error("Failed to authenticate. The username already exists or the password is wrong.")

    def loggedIn(self):
        """Override this function to perform certain actions after logging in."""
        pass

    def quitSession(self, params):
        self.transport.loseConnection()


    def sendPossibleCommands(self, params):
        """A basic help function which is globally available. Returns all the commands in the
        current and global state."""
        res = []
        for x in self.callmap.possible_commands(self.state):
            if x.cmd != "COMMANDS":
                res.append(x.cmd)
        self.writeCommand("COMMANDS %s" % " ".join(res))


    def sendHelp(self, params):
        """A basic help function which is globally available. Returns the description and \
number of parameters for the command."""
        res = ""
        for x in self.callmap.possible_commands(self.state):
            if x.cmd == str.upper(params[0]):
                res = "%sExpects %d parameter(s)." % \
                    (x.description + ". " if x.description != "" else "", len(x.params))
        self.writeCommand("HELP %s" % res)



    def updateCallMap(self):
        """Override this function and place some calls to self.callmap.add_state to 
        add some more states and commands."""
        pass

    def welcome(self):
        self.updateCallMap()

