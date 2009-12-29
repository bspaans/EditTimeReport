from distutils.core import setup

setup(name= "editor_time_chart_daemon",
    version = "1.0",
    description = "Logs editor usage (and generates reports in the future)",
    author = "Bart Spaans",
    author_email = "onderstekop@gmail.com",
    scripts=["timechartd.py"],
    license="GPLv3",
    )
