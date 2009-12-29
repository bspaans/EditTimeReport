from time import time
import sha

class Users:
    

    def __init__(self, db, conn):
        self.users = {}
        self.db = db
        self.loadUsers()
        self.connection = conn

    def hashPassword(self, password):
        return sha.new(password).hexdigest()


    def loadUsers(self):
        self.db.execute("UPDATE clients SET online = 0")
        self.db.execute("SELECT * FROM clients")
        i = 0
        for row in self.db:
            # do something?
            i += 1
        print "Loaded %d users."% i


    def loginOrRegister(self, name, password):

        if self.availableName(name):
            return self.register(name, password)
        else:
            return self.login(name, password)


    def register(self, name, password):
        """Adds new entry for name and password + logs in."""
        if self.availableName(name):
            self.db.execute("INSERT INTO clients(name,password,userSince) VALUES(?, ?, ?)", \
                    (name, self.hashPassword(password), time()))
            return self.login(name, password)
        return False



    def login(self, name, password):
        """Checks name and password, toggles online attribute."""


        self.db.execute("SELECT ID FROM clients WHERE name=? AND password=?", (name, self.hashPassword(password)))
        id = None
        for row in self.db:
            id = row[0]

        if id is not None:
            self.db.execute("UPDATE clients SET online = 1, lastLogin =? WHERE ID=?", (time(), id))
            self.connection.commit()
            return id
        return False


    def logout(self, ID):
        """Toggles online attribute."""
        if ID != -1:
            self.db.execute("UPDATE clients SET online=0 WHERE ID=?", (ID,))
            self.connection.commit()


    
    def availableName(self, name):
        """Sets the name for the user with ID if the name is \
        not blocked or already registered."""

        # Check for blocked names
        if not(self.validName(name)): 
            return False

        # Check for already registered names
        self.db.execute("SELECT COUNT(*) FROM clients WHERE name=?", (name,))
        for row in self.db:
            if row[0] != 0:
                return False

        return True

    def validIP(self, IP):

        self.db.execute("SELECT COUNT(*) FROM blockedIP WHERE IP=?", (IP, ))
        for row in self.db:
            return row[0] == 0
        return True


    def validName(self, name):

        self.db.execute("SELECT COUNT(*) FROM blockedNames WHERE name =?", (name,))
        for row in self.db:
            return row[0] == 0
        return True
