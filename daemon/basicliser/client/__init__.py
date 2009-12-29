from ClientProtocol import ClientProtocol
from basicliser.shared.ConfigurationParser import Parser

# Log listeners
from basicliser.shared.LogNotify import LogNotify
from basicliser.shared.AbstractLogListener import AbstractLogListener
from basicliser.shared.CliLogListener import CliLogListener

# Twisted
from twisted.internet.protocol import Protocol, Factory, ClientFactory
from twisted.internet import reactor


default_options = {"server": "192.168.1.41", "port": 2727}


class modClientFactory(ClientFactory):

    def __init__(self, protocol):
        self.protocol = protocol



def connect( conf_file = "", conf_options = default_options, protocol = ClientProtocol, cfactory = modClientFactory ):

    # Parse configuration file
    p = Parser(conf_options, "=")
    if conf_file is not None and conf_file != "":
        options = p.parse_file(conf_file)
        if options == False:
            print "Using default configuration."
            options = conf_options
    else:
        print "Using default configuration."
        options = conf_options


    # Create logging objects
    log = LogNotify()
    log.attach(CliLogListener())


    factory = cfactory(protocol)
    factory.log = log
    factory.options = options


    reactor.connectTCP(options["server"], options["port"], factory)
    reactor.run()

