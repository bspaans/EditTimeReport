from AbstractDataParser import AbstractDataParser

class plainTextDataParser(AbstractDataParser):


    def __init__(self, client):
        self.client = client
        self.buffer = ""
        self.strict = False # If True, clients are dropped after invalid commands

    def parseData(self, data):

        # Should wait until \n is found..
        if data.find("\n") != -1:
            map(self.parseLineOfData, (self.buffer + data).split("\n"))
            self.buffer = ""
        else:
            self.buffer += data

    def parseLineOfData(self, data):
        """Uses the callmap and state to find the proper protocol function."""



        callmap = self.client.callmap
        state = self.client.state 

        # Check if the state exists
        if not (callmap.has_state(state)):
            self.client.logMessage("Non-existent state: '%s'" % state, 
                    self.client.log.MT_ERROR_CRITICAL)
            self.client.error("Unknown state")



        # Trim the string and cut it up in words
        data = str.strip(data)
        if data == '':
            return

        parts = data.split(" ")
        command, params = parts[0], parts[1:]
        self.callCommandFunction(command, params)



    def callCommandFunction(self, command, params):
        """Calls the protocol back if a command is valid."""

        callmap = self.client.callmap
        state = self.client.state
        cmd = callmap.get_command(state, command)


        if cmd:
            self.client.logMessage("%s command [%s]" % (command, state), \
                    self.client.log.MT_VALIDCMD)

            # Get function 
            f = None
            try:
                f = getattr(self.client, cmd.func)
            except:
                self.client.logMessage("Non-existent function: '%s' in protocol. [%s]" % \
                    (cmd.func, state), self.client.log.MT_ERROR_CRITICAL)
                self.client.error("Unknown command")
                if self.strict:
                    self.client.quitSession(None)
                return 

            # Get parameters
            try:
                params = cmd.getParams(params)
            except Exception, err:
                self.client.logMessage(err, self.client.log.MT_INVALIDPARAMS)
                self.client.error(err)
                return

            # Make the call
            f(params)

        else:
            self.client.logMessage("Unknown command '%s'" % command, self.client.log.MT_INVALIDCMD)
            self.client.error("Unknown command")
            if self.strict:
                self.client.quitSession(None)

