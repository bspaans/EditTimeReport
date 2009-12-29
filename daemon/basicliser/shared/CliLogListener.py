from AbstractLogListener import AbstractLogListener

class CliLogListener(AbstractLogListener):

    def notify(self, msg, msg_type):
        if msg_type < 100:
            print msg_type, msg
