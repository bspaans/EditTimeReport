from distutils.core import setup

setup(name= "editor_time_chart_daemon",
    version = "1.0",
    description = "Client and server programs to log program usage.",
    author = "Bart Spaans",
    author_email = "onderstekop@gmail.com",
    scripts=["timechartd.py", "timechart"],
    packages=["basicliser", "basicliser.server", "basicliser.shared", "basicliser.client"],
    license="GPLv3",
    )
