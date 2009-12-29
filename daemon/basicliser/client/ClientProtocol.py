from basicliser.shared.CallMap import CallMap, State, Command
from AbstractClientProtocol import AbstractClientProtocol

class ClientProtocol(AbstractClientProtocol):

    state = "handshake"

    def __init__(self):

        self.callmap = CallMap([
            State("handshake", 
                [Command("version", "login", None),
                 Command("error", "wrongVersion", None)]),
            State("login", 
                [Command("login", "loginSuccesful", None), 
                 Command("error", "loginFailed", None)])])



    def ask(self, askstr):
        print "%s:" % askstr,
        try:
            r = raw_input()
        except:
            r = ""
        if r == "":
            print "%s can't be empty." % askstr
            return self.ask(askstr)
        return r

    def askUsername(self):
        return self.ask("Username")

    def askPassword(self):
        return self.ask("Password")

    def login(self, params):
        self.state = "login"

        opt = self.factory.options
        username = opt["username"] if opt.has_key("username") else self.askUsername()
        password = opt["password"] if opt.has_key("password") else self.askPassword()
        self.writeCommand(" ".join(["login", username, password]))

    def loginFailed(self, params):
        print "Login failed."
        print "Reason:", " ".join(params)
        self.login([])

    def loginSuccesful(self, params):
        self.state = "loggedin"
        print "Logged in!"
        self.loggedIn()


    def loggedIn(self):
        """Post login hook."""
        pass


    def updateCallMap(self):
        """Override this function and place some calls to self.callmap.add_state to 
        add some more states and commands."""
        pass

    def welcome(self):
        self.updateCallMap()
        self.startHandshake()

    def wrongVersion(self, params):
        pass


    def startHandshake(self):
        self.writeCommand("VERSION 1")

