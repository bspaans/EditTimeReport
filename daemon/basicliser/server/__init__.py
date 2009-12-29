# Protocol
from AbstractServerProtocol import AbstractServerProtocol
from ServerProtocol import ServerProtocol

# Log listeners
from basicliser.shared.LogNotify import LogNotify
from basicliser.shared.AbstractLogListener import AbstractLogListener
from basicliser.shared.CliLogListener import CliLogListener

from basicliser.shared.ConfigurationParser import Parser
from Users import Users

# sqlite3
import sqlite3

# Twisted
from twisted.internet.protocol import Protocol, Factory
from twisted.web import server, resource
from twisted.internet import reactor


default_options = {"port": 2727, "database": "basicliser.db"}


def startService(conf_file = "", conf_options = default_options, protocol = ServerProtocol, outside = None, users=Users):
    """Parses the configuration file -if its not None-, builds the Twisted Factory using protocol and users \
and runs the reactor. You can use outside (a singleton object) to store global data."""

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

    log.notify("Server started", log.MT_SERVER)


    # Open Database connection
    dbconn = sqlite3.connect(options["database"])
    cursor = dbconn.cursor()


    # Create tables if necessary
    cursor.execute("CREATE TABLE IF NOT EXISTS clients (id INTEGER NOT NULL PRIMARY KEY AUTOINCREMENT,"\
        "name VARCHAR(50), password TEXT, online BOOL, IP TEXT, userSince TEXT, lastLogin TEXT)")
    cursor.execute("CREATE TABLE IF NOT EXISTS blockedIP (IP TEXT PRIMARY KEY)")
    cursor.execute("CREATE TABLE IF NOT EXISTS blockedNames (name TEXT PRIMARY KEY)")


    # Create factory and run reactor
    factory = Factory()

    factory.protocol = protocol           # The server protocol
    factory.users = users(cursor, dbconn) # The user database
    factory.outside = outside             # The global singleton object
    if hasattr(outside, "databasePointers"):
        outside.databasePointers(dbconn, cursor)

    factory.db = cursor                   # The database cursor

    factory.options = options
    factory.log = log

    factory.clients = []


    port = int(options["port"])
    reactor.listenTCP(port, factory)
    reactor.run()   
