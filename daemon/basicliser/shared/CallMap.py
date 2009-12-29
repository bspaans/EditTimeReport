class CallMap:

    def __init__(self, states = {}):
        self.states = {}
        if type(states) == dict:
            self.states = states
        if type(states) == list:
            for x in states:
                self.states[x.id] = x.commands


    def add_state(self, state):
        if self.has_state(state.id):
            self.states[state.id] += state.commands
        else:
            self.states[state.id] = state.commands

    def has_state(self, state):
        return self.states.has_key(state)

    def possible_commands(self, state):
        cmdstate = self.states[state] if self.has_state(state) else []
        globals = self.states["GLOBALS"] if self.has_state("GLOBALS") else []
        return cmdstate + globals


    def get_command(self, state, cmd):

        cmd = str.upper(cmd)
        for x in self.possible_commands(state):
            if x.cmd == cmd:
                return x
        return False


    

class State:

    id = ""
    commands = []


    def __init__(self, id, cmds = []):
        self.id = id
        self.commands = cmds

    def add_command(self, cmd):
        self.commands.append(cmd)
    

class Command:
    cmd = ""
    func = ""
    description = ""
    params = [] # list of types

    def __init__(self, command, func, params = [], description = ""):
        self.cmd = str.upper(command)
        self.func = func
        self.params = params
        self.description = description

    def getParams(self, params):
        """Converts the parameters to the expected types or throws errors when that's impossible."""

        if self.params is None:
            return params

        if len(params) != len(self.params):
            raise Exception, "Expecting %d parameters." % len(self.params)

        res = []

        for i,x in enumerate(self.params):
            if x is not None:
                try:
                    res.append(x(params[i]))
                except:
                    raise Exception, "Type-error in parameter %d. Expecting: %s" % (i + 1, x)
            else:
                res.append(x)
        return res  

