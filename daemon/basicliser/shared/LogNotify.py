class LogNotify:


    listeners = []

    # Log message types
    MT_DEFAULT = -1
    MT_CONN = 0
    MT_SERVER = 1
    MT_CLIENT = 2
    
    
    MT_ERROR_WARN = 10
    MT_ERROR_CRITICAL = 11
    MT_ERROR_FATAL = 12

    # Data Parsing
    MT_VALIDCMD = 51
    MT_INVALIDCMD = 52
    MT_INVALIDPARAMS = 55

    MT_DATARECV = 100  # Raw data received
    MT_DATASEND = 101  # Raw data send
    MT_BROADCAST = 102 # Raw data broadcast

    def attach(self, listener):
        self.listeners.append(listener)

    def notify(self, msg, msg_type = -1):
        for l in self.listeners:
            l.notify(msg, msg_type)
